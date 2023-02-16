(* this is where you will convert s-expressions to expressions and definitions *)
open Sexplib.Sexp
module Sexp = Sexplib.Sexp
open Expr
open Printf

(* defines rules for what id's are valid, give this to them *)
let reserved_words = ["let"; "inc"; "dec"; "if"; "while"; "true"; "false"; "set"; "def"];;

(* converts string to Some int or None if not convertible *)
let int_of_string_opt (s: string) : int option = 
  try Some(int_of_string s) 
  with Failure _ -> None 
;;

(* checks if a string is in a list *)
let rec find (l: string list) (s: string) : bool = 
  match l with 
  | [] -> false
  | hd :: tl -> if (hd = s) then true else find tl s
;;

(* helper function for parse_args, checks if a string is in a list of tuples *)
let rec tuple_find (l: (string * typ) list) (s: string) : bool = 
  match l with
  | [] -> false
  | (n, t) :: tl -> if (n = s) then true else tuple_find tl s
;;

(* parses an expression *)
let rec sexp_to_expr (se: Sexp.t) : expr = 
  match se with
  (* ENum, EId, and EBool *)
  | Atom(s) -> (match (int_of_string_opt s) with 
                | Some(i) -> ENum(i)
                | None -> (match s with
                            | "true" -> EBool(true)
                            | "false" -> EBool(false)
                            | _ -> EId(s)))
  (* EIf *)
  | List[Atom("if"); sexp1; sexp2; sexp3] -> 
      EIf((sexp_to_expr sexp1), (sexp_to_expr sexp2), (sexp_to_expr sexp3))
  (* ELet *)
  | List(Atom("let") :: List([Atom(x); sexp]) :: bodylist) ->
      if bodylist = []
        then failwith "Empty expression list : let"
      else if (find reserved_words x)
        then failwith "Invalid variable name"
      else ELet(x, (sexp_to_expr sexp), (seq_helper bodylist))    
  (* ESet *)
  | List[Atom("set"); Atom(s); sexp] -> ESet(s, (sexp_to_expr sexp)) 
  (* EWhile *)
  | List(Atom("while") :: sexp :: bodylist) -> 
      if bodylist = []
        then failwith "Empty expression list : while"
      else EWhile((sexp_to_expr sexp), (seq_helper bodylist))
  (* EUnop *)
  | List[Atom("inc"); sexp] -> EUnop(Inc, sexp_to_expr sexp)
  | List[Atom("dec"); sexp] -> EUnop(Dec, sexp_to_expr sexp)
  (* EComp *)
  | List[Atom("<"); sexp1; sexp2] -> EComp(Less, sexp_to_expr sexp1, sexp_to_expr sexp2)
  | List[Atom(">"); sexp1; sexp2] -> EComp(Great, sexp_to_expr sexp1, sexp_to_expr sexp2)
  | List[Atom("="); sexp1; sexp2] -> EComp(Eq, sexp_to_expr sexp1, sexp_to_expr sexp2)
  (* EBinop *)
  | List[Atom("+"); sexp1; sexp2] -> EBinop(Plus, sexp_to_expr sexp1, sexp_to_expr sexp2)
  | List[Atom("-"); sexp1; sexp2] -> EBinop(Minus, sexp_to_expr sexp1, sexp_to_expr sexp2)
  | List[Atom("*"); sexp1; sexp2] -> EBinop(Times, sexp_to_expr sexp1, sexp_to_expr sexp2)
  (* EApp *)   
  | List(Atom(f) :: sexp_lst) -> EApp(f, seq_helper sexp_lst) 
  | _ -> failwith "Invalid expression"

(* takes a list of sexps and makes them into an list of expr *)
and seq_helper (body_list: Sexp.t list) : expr list = 
match body_list with 
| [] -> []
| hd :: tl -> (sexp_to_expr hd) :: (seq_helper tl)
;;

(* parses a function type *)
let parse_typ (s: string) : typ = 
  match s with
  | "Num" -> TNum 
  | "Bool" -> TBool
  | _ -> failwith "Not a permitted function return type"
;;

(* parses a function's list of arguments *)
let rec parse_args (sexps: Sexp.t list) : (string * typ) list =
  match sexps with 
  | [] -> []
  | Atom(vname) :: Atom(":") :: Atom(argtype) :: tl -> let rest_args = (parse_args tl) in
                                                    (* error check for multiple arguments with the same name *)
                                                    if (tuple_find rest_args vname) then 
                                                      failwith (sprintf "Multiple function arguments %s" vname)
                                                    else (vname, parse_typ argtype) :: rest_args
  | _ -> failwith "Not a permitted argument type for functions"
;;

(* function to parse a sexp into a definition *)
let parse_def (se: Sexp.t) : def =  
  match se with
  (* example: (def (f x y) 4 *)
  | List[Atom("def");List(Atom(fname) :: arg_lst); Atom(":"); Atom(ftype); body] ->
      DFun(fname, parse_args arg_lst, parse_typ ftype, sexp_to_expr body) 
  | _ -> failwith "Parsing Definition Error"
;;

(* helper function for parse_program to check if a def exists in a list of defs *)
let rec def_find (def_lst: def list) (current: def) : bool = 
  match def_lst, current with
  | [], _ -> false
  | DFun(fname, arg_lst, ftype, body) :: rest, DFun(cfname, carg_lst, cftype, cbody) -> if (fname = cfname) then true
                                                                                        else (def_find rest current)
;;
    
(* takes a list of sexpressions and parses it into a program *)
(* a program is a list of function definitions and an expression *)
let rec parse_program (sexps: Sexp.t list) : prog =
  match sexps with
  | [] -> failwith "Empty program" 
  (* last sexp is always the expression *)
  | [se] -> ([], sexp_to_expr se)
  | sdef :: rest -> (* this syntax tears apart a tuple and names each part *) 
                  let defs, body = parse_program rest in 
                  let current_def = parse_def sdef in
                  (* error check for function definitions with the same name *)
                  if (def_find defs current_def) then
                    match current_def with 
                    | DFun(fname, _, _, _) -> failwith (sprintf "Multiple function definitions %s" fname)
                  else (current_def :: defs, body) 
;;