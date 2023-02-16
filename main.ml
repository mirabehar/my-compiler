open Compile
open Runner
open Printf
open Lexing

(* take an argument form the command line, and compile it *)
(* print the resulting assembly string *)
let () =
  let name = Sys.argv.(1) in
  let input_file = open_in name in
  let program = compile_file_to_string input_file in
  printf "%s\n" program;;

