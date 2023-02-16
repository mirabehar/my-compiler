(* file that defines types to represent the assembly *)
(* also contains functions to convert the assembly to strings *)
open Printf

(* registers *)
type reg =
   | Rax
   | Rsp
   | Rbx 

(* arguments to instructions like add or mov *)
type arg =
   | Const of int
   | Reg of reg
   | RegOffset of int * reg
   | Lab of string 

(* type for assembly instructions *)
type instr =
   (* mov: source, dest *)
   | IMov of arg * arg
   (* add: to-add, dest *)
   | IAdd of arg * arg
   (* sub: to-sub, dest *)
   | ISub of arg * arg
   (* mul: to-mul, dest *)
   | IMul of arg * arg
   (* compare second arg to first arg and set flag *)
   | ISal of arg * arg
   | ICmp of arg * arg 
   (* uncondionally jump to a label *)
   | IJmp of string 
   (* jump if cmp flag is equal *)
   | IJe of string 
   (* jump if cmp flag is not equal *)
   | IJne of string 
   (* jump if cmp flag set to less or equal *)
   | IJle of string
   (* jump if flag is set to greater or equal *)
   | IJge of string 
   (* jumo if flag is set to greater *)
   | IJg of string
   (* jump if flag is set to less *)
   | IJl of string
   (* the actual label *)
   | ILab of string 
   (* return *)
   | IRet

(* calculates the offset from the stack pointer *)
let stackloc (i: int) : arg = RegOffset(-8 * i, Rsp)

(* ----------------------------------------------------------------------*)

(* HELPER FUNCTIONS TO DEAL WITH LABELS *)

(* initialize a counter to keep track of how many labels we have *)
(* this is a MUTABLE variable *)
let counter : int ref = ref 0;;

(* helper function to generate unique labels *)
let new_label (s: string) : string =

    (* get the current value of the counter *)
    let cur : int  = !counter in

    (* update the counter -- need ; since this is non-functional code *)
    counter := cur + 1;

    (* generate label string -- example: else3 *)
    sprintf "%s%d" s cur
;;

(* -----------------------------------------------------------*)

(* HELPER FUNCTIONS TO TURN ASSEMBLY INTO STRINGS *)

(* turn a register into a string *)
let reg_to_string (r: reg) : string =
   match r with
   | Rax -> "%rax"
   | Rsp -> "%rsp"
   | Rbx -> "%rbx"
;;

(* turn an argument to an instruction into a string *)
let arg_to_string (a: arg) : string =
   match a with
   | Const(n) -> sprintf "$%d" n
   | Reg(r) -> reg_to_string r
   | RegOffset(n,r) -> sprintf "%d(%s)" n (reg_to_string r)
   | Lab(s) -> sprintf "$%s" s
;;

(* turn an instruction into a string *)
let instr_to_string (i:instr) : string =
   match i with
   | IMov(s,d) -> sprintf "mov %s, %s" (arg_to_string s) (arg_to_string d)
   | IAdd(to_add, dest) -> sprintf "add %s, %s" (arg_to_string to_add) (arg_to_string dest)
   | ISub(to_sub, dest) -> sprintf "sub %s, %s" (arg_to_string to_sub) (arg_to_string dest)
   | IMul(to_mul, dest) -> sprintf "imul %s, %s" (arg_to_string to_mul) (arg_to_string dest)
   | ICmp(to_cmp, dest) -> sprintf "cmp %s, %s" (arg_to_string to_cmp) (arg_to_string dest) 
   | ISal(bit, dest) -> sprintf "sal %s, %s" (arg_to_string bit) (arg_to_string dest)
   | IJmp(label) -> sprintf "jmp %s" label 
   | IJe(label) -> sprintf "je %s" label 
   | IJne(label) -> sprintf "jne %s" label
   | IJle(label) -> sprintf "jle %s" label
   | IJge(label) -> sprintf "jge %s" label 
   | IJg(label) -> sprintf "jg %s" label
   | IJl(label) -> sprintf "jl %s" label
   | ILab(label) -> sprintf "%s:" label 
   | IRet -> "ret"
;;

(* converts list of instructions into one nice assembly string *)
let rec instrs_to_string (is: instr list) : string =
   match is with
   | [] -> ""
   | hd :: tl -> (instr_to_string hd) ^ "\n  "  ^ (instrs_to_string tl)
;;

