(* compiler that has increment, decrement, binary +, - and * *)
(* if, comparisons, booleans *)

open Printf
open Expr
open Asm 

(* an association list to keep track of the variable environment - maps strings to ints *)
type tenv = (string * int) list

(* function to find if a variable is in either the variable stack environment or the type environment *)
let rec find (env: (string * 'a) list) (x: string) : 'a option = 
   match env with 
   | [] -> None
   | (y, i) :: rest -> 
     if y = x then Some(i) else find rest x
;;


(* helper for type_check_expr that takes a function name and returns the corresponding function definition
    or None if the name isn't found *)
let rec def_find (defs: def list) (x: string) : def option = 
   match defs with 
   | [] -> None
   | DFun(fname, args, ftype, body) :: tl -> if fname = x then Some(DFun(fname, args, ftype, body))
                                                         else def_find tl x 
;;

(*** Type Checker **)

(* a type environment is a list of string * typ  pairs tracking variables and their types *)
(* NOTE: this is distinct from environment that tracks variables and their stack location *)
type typeenv = (string * typ) list 

(* function that type checks an expression *)
(* either returns a type or fails with a Type Mismatch error *)
let rec type_check_expr (e : expr) (env: typeenv) (defs: def list): typ =
  match e with
   | ENum(_) -> TNum
   | EBool(_) -> TBool
   | EId(x) -> (match (find env x) with
                | None -> failwith (sprintf "Unbound variable identifier %s" x)
                | Some(t) -> t)
   | EUnop(_, exp) -> if (type_check_expr exp env defs = TNum) then TNum 
                       else 
                         failwith "Type Mismatch: unary operator needs an argument of type Num"
   | EBinop(_, exp1, exp2) -> 
       if ((type_check_expr exp1 env defs = TNum) && (type_check_expr exp2 env defs = TNum)) then TNum
       else
          failwith "Type Mismatch: binary operator needs arguments of type Num"
   | EComp(op, exp1, exp2) ->
       (match op with
        | Eq -> if ((type_check_expr exp1 env defs) = (type_check_expr exp2 env defs)) then TBool
                else failwith "Type Mismatch: = needs arguments of same type"
        | _ ->  if ((type_check_expr exp1 env defs = TNum) && (type_check_expr exp2 env defs = TNum)) 
                  then TBool
                else failwith "Type Mismatch: comparator needs arguments of type Num")
   | EIf(c, t, f) -> let t_type : typ = (type_check_expr t env defs) in
                       if (type_check_expr c env defs != TBool)
                         then failwith "Type Mismatch: if condition must be of type Bool"
                       else if (t_type != (type_check_expr f env defs))
                         then failwith "Type Mismatch: if branches must be of same type"
                       else t_type
   | ELet(name, value, bodylist) ->
        let val_type : typ = (type_check_expr value env defs) in
        (seq_helper bodylist ((name, val_type) :: env) defs)
   | ESet(name, exp) -> 
       (match (find env name) with
        | None -> failwith (sprintf "Unbound variable identifier %s" name)
        | Some(t) -> if (t = type_check_expr exp env defs)
                       then t
                     else failwith (sprintf "Type Mismatch: cannot set variable %s to a different type" name))
   | EWhile(cond, bodylist) ->
       if (type_check_expr cond env defs != TBool)
         then failwith "Type Mismatch: while condition must be of type Bool"
       else let _ : typ = (seq_helper bodylist env defs) in TBool
   | EApp(name, expr_lst) ->
      (match (def_find defs name) with
        | None -> failwith (sprintf "Undefined function %s" name) 
        | Some(DFun(fname, args, ftype, body)) -> type_check_arg ftype args expr_lst env defs)
   | _ -> failwith "Invalid expression"

and seq_helper (bodylist: expr list) (env: typeenv) (defs: def list): typ =
  match bodylist with
  | [] -> failwith "Error: empty expression list"
  | last :: [] -> (type_check_expr last env defs)
  | hd :: tl -> let _ : typ = (type_check_expr hd env defs) in
                (seq_helper tl env defs)


(* helper function to match the type of each provided argument with the declared type of each argument
in the function definition*)
and type_check_arg  (ftype: typ) (args: (string * typ) list) (expr_lst: expr list) 
(env: typeenv) (defs: def list) : typ = 
    if List.length args = List.length expr_lst then
        match expr_lst, args with
            | [], [] -> ftype
            | expr :: etl, (_, arg_typ) :: atl -> let expr_typ : typ = type_check_expr expr env defs in
                                                    if expr_typ = arg_typ 
                                                    then type_check_arg ftype atl etl env defs
                                                    else failwith (sprintf "Type Mismatch: expected argument type doesn't match provided type")
            | _, _ -> failwith "Type Mismatch: provided arguments don't match expected arguments"
    else failwith ("Type Mismatch: Expected number of arguments doesn't match provided number of arguments")
;;

(* function to type check definitions *)
(* returns true if function typechecks or fails with a Type Mismatch error *)
let type_check_def (d: def) (defs: def list) : bool = 
    match d with
    | DFun(_, args, ftype, body) -> if (type_check_expr body args defs) = ftype then true
    else failwith "Type Mismatch: actual function type does not match provided function type"
;;

(* function to type check whole program *)
let type_check_prog (body: expr) (defs: def list): typ = 
    (* first type check every definition in the list of definitions *)
    (* this should produce a list of true expressions *)
    (* we don't actually need the result *)
    let _ = List.map (fun d -> type_check_def d defs) defs in
    (* since the definitions typechecked, now typecheck the body expression *)
    (* the types of function arguments are contained only within those definitions *)
    (* from the perspective of the body expression, no types have been defined *)
    (* this will return a typ, which is the type of the program *)
    type_check_expr body [] defs 
;; 

(*** Actual compiler starts here  ***)  

(* code generation for the AST *)
let rec expr_to_instrs (e: expr) (env: tenv) (si: int) (defs: def list) : instr list = 
   match e with
   | ENum(n) -> [IMov(Const(n), Reg(Rax))]
   | EBool(b) -> if b then [IMov(Const(1),Reg(Rax))]
                 else [IMov(Const(0), Reg(Rax))] 
   | EId(s) -> (match (find env s) with
               | None -> failwith (sprintf "Unbound variable identifier %s" s)
               | Some(i) -> [IMov((stackloc i), Reg(Rax))])
   | EUnop(op, exp) -> let rec_call : instr list = expr_to_instrs exp env si defs in
                     let new_instr =
                       match op with
                       | Inc -> [IAdd(Const(1), Reg(Rax))]
                       | Dec -> [ISub(Const(1), Reg(Rax))]
                     in rec_call @ new_instr
   | EBinop(op, e1, e2) -> binop_helper op e1 e2 env si defs
   | EComp(c, e1, e2) -> comp_helper c e1 e2 env si defs
   | EIf(c, t, f) -> if_helper c t f env si defs
   | ELet(x, v, blist) -> let_helper x v blist env si defs
   | ESet(name, exp) -> set_helper name exp env si defs
   | EWhile(cond, bodylist) -> while_helper cond bodylist env si defs
   | EBitshl(bits, exp) -> let rec_call : instr list = expr_to_instrs exp env si defs in
                           let new_instr = [ISal(Const(bits), Reg(Rax))] in
                           rec_call @ new_instr
   | EApp(f, elist) -> 
                     (match (def_find defs f) with
                     | None -> failwith ("Undefined function"  ^ f)
                     (* extract definition *)
                     | Some(DFun(_, arg_lst, ftype, body)) ->
                           (* make new label *)
                           let after = new_label "after" in
                           let after_label = [ILab(after)] in 
                           (* move label on stack, need to move to a reg first  *)
                           let move = [IMov(Lab(after),Reg(Rax)); 
                               IMov(Reg(Rax), stackloc si)] in
                           (* save old rsp *)
                           let save_rsp = [IMov(Reg(Rsp),stackloc (si + 1))] in
                           (* call helper to deal with args *)
                           let arg_instrs = make_store_args elist env (si + 2) defs in
                           (* set rsp to point to after call *)
                           let move_rsp = [ISub(Const(8 * si),Reg(Rsp))] in
                           (* go to function *)
                           let jump_f = [IJmp(f)] in
                           (* reset rsp *)
                           let restore_rsp = [IMov(stackloc 2,Reg(Rsp))] in
                           (* smush everything together *)
                           move @ save_rsp @ arg_instrs @ move_rsp @ jump_f @ after_label @ restore_rsp)

(* helper that generates instructions for EBinop case *)
and binop_helper (op: binop) (e1: expr) (e2: expr) (env: tenv) (si: int) 
   (defs: def list) : instr list = 
   (* generate instructions for e1, increment si to not clobber *)
   let e1_instrs = expr_to_instrs e1 env (si + 1) defs in 
   (* generate instructions for e2 *)
   let e2_instrs = expr_to_instrs e2 env si defs in 
   (* move the left hand side from rax to temporary space on stack *)
   let e2_store = [IMov(Reg(Rax),stackloc si)] in
   (* evaluate e1 first so subtraction order works *)
   let up_to_op = e2_instrs @ e2_store @ e1_instrs in 
   (* specialize the op depending on what it is *)
   let op_instr = 
       (match op with 
       | Plus -> [IAdd(stackloc si, Reg(Rax))]
       | Minus -> [ISub(stackloc si, Reg(Rax))]
       | Times -> [IMul(stackloc si, Reg(Rax))])
   in up_to_op @ op_instr 

(* helper for ELet case: 
* takes binding list and let body as well as environment and stack index *)
and let_helper (name: string) (value: expr) (bodylist: expr list) (env: tenv) (si: int) (defs: def list) : instr list =
   let exp_instrs : instr list = (expr_to_instrs value env si defs) in
   let store : instr list = [IMov(Reg(Rax), (stackloc si))] in
   let process_bodylist : instr list = (seq_helper bodylist ((name, si) :: env) (si + 1) defs) in
   exp_instrs @ store @ process_bodylist

(* go through and generate instructions for each expression in a list *) 
and seq_helper (body_list: expr list) (env: tenv) (si: int) 
   (defs: def list) : instr list =
   match body_list with
   | [] -> []
   | hd :: tl -> (expr_to_instrs hd env si defs) @ (seq_helper tl env si defs) 

(* a mutually recursive helper function is declared with and *)
and if_helper (c: expr) (t: expr) (f: expr) (env: tenv) (si: int) 
   (defs: def list) : instr list = 
   (* process condition-instrs *)
   let cinstrs : instr list = expr_to_instrs c env si defs in 
   (* process if-instrs *)
   let tinstrs : instr list = expr_to_instrs t env si defs in
   (* process else-instrs *)
   let finstrs : instr list = expr_to_instrs f env si defs in
   (* make else and both label *)
   let estring : string = new_label "else" in
   let bstring : string = new_label "both" in 
   let elab : instr list = [ILab(estring)] in
   let blab : instr list = [ILab(bstring)] in
   (* make jumps *)
   let bjump : instr list = [IJmp(bstring)] in
   let ejump : instr list = [IJe(estring)] in
   (* make comparison *)
   let comp : instr list = [ICmp(Const(0), Reg(Rax))] in
   (* smush everything together in the right order *)
   cinstrs @ comp @ ejump @ tinstrs @ bjump @ elab @ finstrs @ blab 

(* helper function that generates instructions for EComp *)
and comp_helper (c: comp) (e1: expr) (e2: expr) (env: tenv) (si: int)
  (defs: def list) : instr list = 
  (* eval e1 -> rax *) 
  let e1instrs : instr list = expr_to_instrs e1 env si defs in
  (* rax -> stack *)
  let store : instr list = [IMov(Reg(Rax), stackloc si)] in
  (* eval e2 -> rax, don't clobber e1 so do si + 1 not si!  *)
  let e2instrs : instr list = expr_to_instrs e2 env (si + 1) defs in
  (* compare rax to stack space *)
  let cmp : instr list = [ICmp(Reg(Rax), stackloc si)] in
  (* label for what comes after comparison *)
  let b : string = new_label "both" in
  let blabel : instr list = [ILab(b)] in
  (* comparison result instructions *)
  let tinstrs : instr list = [IMov(Const(1), Reg(Rax))] in
  let finstrs : instr list = [IMov(Const(0), Reg(Rax))] in
  (* before reacting to the comparison *)
  (* always moves 0 into rax, if comparison is false will jump to both *)
  (* otherwise, will move 1 into rax *)
  let uptojmp : instr list = e1instrs @ store @ e2instrs @ cmp @ finstrs in
  (* after reacting to the comparison *)
  let afterjmp : instr list = tinstrs @ blabel in
      match c with 
      (* want to have as little repeated code as possible among match cases *)
      | Eq -> uptojmp @ [IJne(b)] @ afterjmp
      | Less -> uptojmp @ [IJge(b)]  @ afterjmp
      | Great -> uptojmp @ [IJle(b)] @ afterjmp   

(* helper function that generates instructions for ESet *)
and set_helper (name: string) (exp: expr) (env: tenv) (si: int) (defs: def list): instr list = 
   let location : int =
       (match (find env name) with
       | None -> failwith (sprintf "Unbound variable identifier %s" name)
       | Some(n) -> n) in
   (* process instructions *)
   let exp_instrs : instr list = (expr_to_instrs exp env si defs) in
   let update_instrs : instr list = [IMov(Reg(Rax), stackloc location)] in
   exp_instrs @ update_instrs
  
(* helper function that generated instructions for EWhile *)
and while_helper (exp: expr) (bodylist: expr list) (env: tenv) (si: int) (defs: def list): instr list =
   let rax : arg = Reg(Rax) in
   (* make labels *)
   let cond : string = new_label "cond" in
   let cond_label : instr list = [ILab(cond)] in
   let after : string = new_label "after" in
   let after_label : instr list = [ILab(after)] in
   (* check condition *) 
   let cond_instrs : instr list = (expr_to_instrs exp env si defs) in
   let compare : instr list = [ICmp(Const(0), rax)] in
   (* jump to after if false *)
   let jump_equal : instr list = [IJe(after)] in
   (* while body *)
   let body_instrs : instr list = (seq_helper bodylist env si defs) in
   (* jump back to condition *) 
   let jump : instr list = [IJmp(cond)] in
   cond_label @ cond_instrs @ compare @ jump_equal @ body_instrs @ jump @ after_label

(* helper function for expr_to_instrs that generates instructions for every argument in 
  the argument list and stores their values appropriately *)
and make_store_args (args: expr list) (env: tenv) (si: int) (defs: def list): instr list =
   match args with
   | [] -> []
   | hd :: tl -> let e_instrs = expr_to_instrs hd env si defs in 
                 let save_arg = [IMov(Reg(Rax), stackloc si)] in
                 e_instrs @ save_arg @ make_store_args tl env (si + 1) defs
;; 

(* helper function for compile_def that builds the environment for a function body *)
let rec build_func_env (args: (string * typ) list) (si: int) : tenv =
 match args with
 | [] -> []
 | (name, _) :: tl -> (name, si) :: build_func_env tl (si + 1)
;;      

(*  compiles an individual function definition into a list of instructions *)
let compile_def (d: def) (defs: def list) : instr list = 
   match d with
   | DFun(fname, arg_lst, ftype, body) -> 
       let si = List.length arg_lst + 2 in 
       (* build function environment *)
       let build_env = build_func_env arg_lst 2 in
       (* generate instructions for body expression *)
       let body_instrs = expr_to_instrs body build_env si defs in
       (* make a label for the function name *)
       (* append a return after the body instructions *)
       [ILab(fname)] @ body_instrs @ [IRet]  
;;

(* compile a list of function definitions into one list of instructions *)
let rec compile_defs (defs: def list) (defs_copy : def list) : instr list =
   match defs with
   | [] -> []
   | hd :: tl -> compile_def hd defs_copy @ compile_defs tl defs_copy
;;

(* optimization code starts here *)

(* function that maps an optimization function onto expressions *)
(* this function exists to avoid duplicating recursive cases
 * in constant_fold, constsnt_prop and replace *)
 let rec map_optimize (f: expr -> expr) (e: expr) : expr = 
   match e with
   | EBinop(op, e1, e2) -> 
                   EBinop(op, f e1, f e2)
   | EUnop(op, e1) -> EUnop(op, f e1)
   | EIf(e1,e2,e3) -> EIf(f e1, f e2, f e3)
   | EComp(op, e1, e2) -> EComp(op, f e1, f e2)
   | ESet(x, e') -> ESet(x, f e')
   | ELet(x, v, blist) -> ELet(x, f v, List.map f blist)
   | EWhile(c,blist) -> EWhile(f c, List.map f blist)
   | EBitshl(bit, e') -> EBitshl(bit, f e')
   | EApp(fname,elist) -> EApp(fname, List.map f elist)
   | _ -> e
;;

(* constant folding *)

(* function that applies constant folding to an expression *)
let rec constant_fold (e: expr) : expr = 
   match e with
   (* base cases *)
   | EBinop(Plus,ENum(n1),ENum(n2)) -> ENum(n1+n2)
   | EBinop(Minus,ENum(n1),ENum(n2)) -> ENum(n1-n2)
   | EBinop(Times,ENum(n1),ENum(n2)) -> ENum(n1*n2)
   | EUnop(Inc,ENum(n)) -> ENum(n+1)
   | EUnop(Dec,ENum(n)) -> ENum(n-1)
   | EComp(Less,ENum(n1), ENum(n2)) -> EBool(n1 < n2)
   | EComp(Great,ENum(n1), ENum(n2)) -> EBool(n1 > n2)
   | EComp(Eq, ENum(n1), ENum(n2)) -> EBool(n1 = n2)
   | EComp(Eq, EBool(b1), EBool(b2)) -> EBool(b1 = b2)
   (* dead code cases *)
   | EIf(EBool(true), e1, e2) -> constant_fold e1
   | EIf(EBool(false), e1, e2) -> constant_fold e2
   | EWhile(EBool(false),blist) -> EBool(false)
   (* constant_fold is passed as an argument to map_optimize *)
   | _ -> map_optimize constant_fold e
;;

(* constant propagation *)

(* this function returns true is f is true for at least one elt of list *)
let rec or_f_list (f: expr -> bool) (elist: expr list) : bool =
   match elist with
   | [] -> false
   | hd :: tl -> (f hd) || (or_f_list f tl)
 ;;

(* check if expression sets ANY variables *)
let rec any_vars_set (e: expr) : bool = 
   match e with 
   | ENum(n) -> false
   | EBool(b) -> false
   | EId(x) -> false
   | EUnop(op,e1) -> any_vars_set e1
   | EBinop(op, e1, e2) -> (any_vars_set e1) || (any_vars_set e2)
   | EComp(comp, e1, e2) -> (any_vars_set e1) || (any_vars_set e2)
   | EIf(c,t,f) -> (any_vars_set c) || (any_vars_set t) || (any_vars_set f)
   | ELet(x,v,blist) -> (any_vars_set v) || (or_f_list any_vars_set blist)
   | EWhile(c, blist) -> (any_vars_set c) || (or_f_list any_vars_set blist)
   | EBitshl(bits, e1) -> (any_vars_set e1)
   | EApp(f,elist) -> (or_f_list any_vars_set elist)
   | ESet(x,e') -> true
;;

(* helper function for constant_prop that actually propagates var *)
(* replaces x with newe inside the old expression *)
let rec replace (x: string) (newe: expr) (old: expr) : expr = 
   match old with
   (* base case that actually replaces var with value *)
   | EId(y) -> if x = y then newe else EId(y)
   (* need to be careful in ELet case *)
   (* only replace into the body of the let if the nested let has a diff var *)
   | ELet(y,v,blist) -> (* don't want to clobber y so don't replace into b *)
                    if x = y then ELet(y, replace x newe v, blist) 
                    (* replace into the b of the let *)
                    else ELet(y, replace x newe v, List.map (fun e-> replace x newe e) blist)
   (* don't replace into a set if it updates var *)
   | ESet(y, e') -> if x = y then ESet(y,e')
                    else ESet(y, replace x newe e')   
   (* need to pass a expr -> expr function to map_optimize *)
   (* can partially apply replace by giving it first two args to get 
    * the right type of function *)
   | _ -> map_optimize (replace x newe) old  
;;

(* function that propagates functions through an expression *)
let rec constant_prop (e: expr) : expr = 
   match e with
   (* only propagate bools and nums *)
   (* once propagation happens, no more lets *)
   (* only propagate into a let with one body *)
   | ELet(x, ENum(n), blist) -> 
                   if List.length blist = 1 && not (any_vars_set e) then 
                   replace x (ENum(n)) (List.hd blist)
                   else e
   | ELet(x, EBool(t), blist) -> 
                   if List.length blist = 1 && not (any_vars_set e) then 
                   replace x (EBool(t)) (List.hd blist)
                   else e 
   (* pass constant_prop as argument to map_optimize *)
   | _ -> map_optimize constant_prop e
;;

(* copy propagation *)
let rec copy_prop (e: expr) : expr = 
   match e with
   | ELet(y, EId(x), blist) -> replace y (EId(x)) (List.hd blist)
   (* pass copy_prop as argument to map_optimize *)
   | _ -> map_optimize copy_prop e
;;

(* algebraic simplification *)
let rec algebra (e: expr) : expr =
   (* should call constant_fold! *)
   let cf_e : expr = (constant_fold e) in
   match cf_e with 
   (* e1 + (e2 + e3) = (e1 + e2) + e3 *)
   (* e1 * (e2 * e3) = (e1 * e2) * e3 *)
   | EBinop(op1, ENum(n1), EBinop(op2, ENum(n2), e1))
   | EBinop(op1, ENum(n1), EBinop(op2, e1, ENum(n2)))
   | EBinop(op1, EBinop(op2, ENum(n1), e1), ENum(n2))
   | EBinop(op1, EBinop(op2, e1, ENum(n1)), ENum(n2))
      when op1 = op2 && op1 != Minus -> EBinop(op1, (constant_fold (EBinop(op1, ENum(n1), ENum(n2)))), algebra e1)
   (* e + 0 = e | 0 + e = e *)
   | EBinop(Plus, e1, ENum(0)) -> algebra e1
   | EBinop(Plus, ENum(0), e1) -> algebra e1
   (* e - 0 = e *)
   | EBinop(Minus, e1, ENum(0)) -> algebra e1
   (* e * 1 = e | 1 * e = e *)
   | EBinop(Times, e1, ENum(1)) -> algebra e1
   | EBinop(Times, ENum(1), e1) -> algebra e1
   (* e * 0 = e | 0 * e = e *)
   | EBinop(Times, e1, ENum(0)) -> ENum(0)
   | EBinop(Times, ENum(0), e1) -> ENum(0)
   (* boring recursive cases *)
   | EUnop(op, e1) -> EUnop(op, algebra e1)
   | EBinop(op, e1, e2) -> EBinop(op, algebra e1, algebra e2)
   | EComp(op, e1, e2) -> EComp(op, algebra e1, algebra e2)
   | EIf(c, t, f) -> EIf(algebra c, algebra t, algebra f)
   | EApp(f, elist) -> EApp(f, List.map algebra elist) 
   | ELet(x, v, blist) -> ELet(x, algebra v, List.map algebra blist)
   | ESet(x, e1) -> ESet(x, algebra e1)
   | _ -> cf_e 
;;

(* strength reduction *)

(* helper function for strength_reduce that returns log2(x)- since log2 not supported *)
let rec log_2 (x: float) (ctr: int) : int option =
   if x < 1.0 then None
   else if x = 1.0 then Some(ctr)
   else log_2 (x/.2.0) (ctr+1)
;;

(* helper function for strength reduction that builds recursive Plus exprs *)
let rec mult_to_add (e: expr) (newe: expr) (x: int): expr = 
   (* base case for negative int multiplication *)
   if x = -1 then EBinop(Minus, e, newe)
      (* recursive case *)
   else if x > 0 || x < 0 then let newexpr = EBinop(Plus, e, newe) in
      if x > 0 then mult_to_add e newexpr (x-1) 
      (* in negative case adds are nested and then subtracted from e in the base case *)
      else mult_to_add e newexpr (x+1)
   (* base case for positive int multiplication *)
   else newe
;;

(* function that applies strength reduction by converting high-cost multiplication 
expressions to some lower-cost combination of addition, subtraction and bitshift *)
let rec strength_reduce (e: expr) : expr =
   match e with
   | EBinop(Times, e1, ENum(x))
   | EBinop(Times, ENum(x), e1) -> 
      (match log_2 (float_of_int x) 0 with
      (* if x not a power of two do addition/subtraction *)
      | None -> let newexpr = EBinop(Plus, e1, e1) in
                  if x > 0 then mult_to_add e1 newexpr (x-2) 
                  else mult_to_add e1 newexpr x 
      (* if x is a power of 2 then do left bit shift *)
      | Some(bits) ->  EBitshl(bits, e1)) 
   | _ -> e

;;

(* fixpoint *)
(* tries to optimize an expression until it can no longer be optimized *)
let rec optimize (e: expr) : expr =
   (* for each round of optimization try to do algebraic simplification, constant propagation
    * and strength reduction *)
   (* not calling constant_fold separately because algebra will call it *)
   let (improved_expr : expr) =  strength_reduce (constant_prop (copy_prop (algebra e))) in 
   (* check if we made progress *)
   (* if no, stop optimizing *)
   if improved_expr = e then improved_expr
   (* if yes, try to optimize again *)
   else optimize improved_expr

(* compiles a program to an x86 string *)
let compile (p: prog) : string = 
  let (defs, body) = p in 
  (* type check the program*)
  let _  = type_check_prog body defs in
  (* try to optimize the body of the program*)
  let improved_body = optimize body in
    (* generate instructions for the definitions *)
  let defs_instrs = compile_defs defs defs in
  (* make string for defs instrs *)
  let defs_str = instrs_to_string defs_instrs in
  (* generate instrs for the expression *)
  let expr_instrs = expr_to_instrs improved_body [] 1 defs in
  (* make string for expr instrs *)
  let expr_str = instrs_to_string (expr_instrs @ [IRet]) in
  (* add the boilerplate to instructions to make it work *)
  sprintf "
  .text
  .globl our_code_starts_here
  %s
  our_code_starts_here:
  %s
  \n" defs_str expr_str
  (* note that definitions go ABOVE our_code_starts_here *)
;;
