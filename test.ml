open Runner
open Expr
open Printf
open OUnit2


(* test_run tests expected value against value produced by compiler generated assembly *)
(* this function combines the name of the test with test_run *)
let t name program expected = name>::test_run program name expected;;

(* parse_string parses a program string into an expression *)
(* this function checks whether the parsed value is equal to the expected value *)
(* also combines the name of the test with the actual test *)
let t_parse name program expected =
  name>::(fun _ -> assert_equal expected (parse_string_full program));;

(* test_err tests expected errors against errors actually produced by the compiler *)
(* works for both compiler and parser errors *)
(* this function combines the name of the test with test_err *)
let t_err name program expected =
  name>::test_err program name expected;;

(* test_parse_err tests expected parser errors against errors produced by the parser *)
(* this function combines the name of the test with test_parse_err *)
let t_parse_err name program expected =
  name>::test_parse_err program name expected;;

(* if you don't want to make all of your tests strings, you can make files instead *)
(* converts a file name to a string - MAKE SURE THE FILE ENDS WITH .INT *)
(* put the test file in the input folder *)
let f_to_s fname = string_of_file ("input/" ^ fname);;

(* makes a string out of a file and runs a test with a name attached to it *)
let t_file test_name file_name expected = (t test_name (f_to_s file_name) expected);;

(* now, if you have a test file named sloth.int which should generate 5 as a result *)
(* you can make a test like this: t_file "sloth_test" "sloth.int" "5" *)

(* example tests that should pass and generate integer results *)
let forty_one = "(dec 42)";;
let forty = "(dec (dec 42))";;
let add1 = "(inc (inc (inc 3)))";;
let ifTest = "(if true 5 6)";;
let boolTest = "true";;
let defTest = "(def (abs_val x : Num) : Num (if (< x 0) (* -1 x) x)) (abs_val -3)";;
let assoc_add = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (+ (+ z 1) 4))";;
let sub = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (- (- z 1) 3))";;
let assoc_mul = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* (* z 2) 3))";;
let zero = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (+ 0 z))";;
let zero2 = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (- z 0))";;
let zero3 = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* z 0))";;
let id = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* 1 z))";;
let nid = "(def (f x : Num y : Num) : Num (+ x y))(let (z (f 2 3)) (* -1 z))";;
let commute = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (+ (+ 1 z) 4))";;
let strength = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* z 4))";;
let mult = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* (* z 3) 4))";;
let power = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 2 3)) (* 8 z))";;
let thrash = "(def (f x : Num y : Num) : Num (+ x y)) (let (z (f 1 3)) (+ z (+ z 3)))";;

(* tests that should cause error *)
(* unbound variable - compiler error *)
let failID = "x";;

(* tests for type mismatch errors *)
let inc_args = "(inc true)";;
let plus_args = "(+ 1 false)";;
let arith_nested = "(+ 1 (- 2 (* true false)))";;
let if_cond = "(if 1 2 3)";;
let if_branch = "(if true false (inc 3))";;
let comp = "(< 3 true)";;
let bad_logic = "(< true false)";;
let fun_body  = "(def (abs_val x : Num) : Num (if (< x 0) true false)) (abs_val -3)"
let fun_app = "(def (abs_val x : Num) : Num (if (< x 0) (* -1 x) x)) (abs_val true)"
let wrong_args1 = "(def (f x : Num ) : Num (+ x 1)) (f 3 2)";;
let wrong_args2 = "(def (f x : Num y : Num) : Num (+ x y)) (f 1)";;

(* function parsing issues *)
let mult_defs = "(def (f x : Num) : Num 4) (def (f y : Num) : Num 5) (+ (f 5) (f 4))";;
let mult_args = "(def (f x : Num x : Num) : Num (+ x y)) (f 3 4)";;

(* this is the program that forty_one should parse to *)
(* function definition list then expression body *)
let forty_one_p = ([],EUnop(Dec, ENum(42)));;

(* this is the program that defTest should parse to *)
let defTest_p = ([DFun("abs_val", [("x",TNum)],TNum,EIf(EComp(Less,EId("x"),ENum(0)),EBinop(Times,ENum(-1),EId("x")),EId("x")))],EApp("abs_val",[ENum(-3)]));;

(* My tests *)
let inc_test1 = "(inc 41)";;
let inc_test2 = "(inc (inc (inc (inc (inc (inc (inc 70)))))))";;
let dec_test1 = "(dec 70)";;
let dec_test2 = "(dec (dec (dec (dec 424))))";;
let combo_test1 = "(inc (dec (inc (inc (dec (dec 528491))))))";;
let plus_test1 = "(+ 16 17)";;
let plus_test2 = "(+ (+ (+ (+ 1 2) 3) 4) 5)";;
let minus_test1 = "(- 5 4)";;
let minus_test2 = "(- ( - (- 10 1) 4) 5)";;
let times_test1 = "(* 7 4)";;
let times_test2 = "(* (* (* (* 2 2) 4) 1) 4)";;
let times_test3 = "(let (x 5) (set x (* 0 x)))";;
let times_test4 = "(let (x 3) (* x 8))";;
let times_test5 = "(* 3 -8)";;
let combo_test2 = "(* (- (inc (+ (dec (* (+ 13 5) 7)) 9)) 30) 2)";;
let combo_test3 = "(* 5 (+ (dec (- (inc -3) 5)) -3))";;
let combo_test4 = "(* 3 (dec (- (inc -3) 5)))";;
let comp_test1 = "(< 2 10)";;
let comp_test2 = "(> -4 -2)";;
let comp_test3 = "(let (x 3) (= 3 x))";;
let let_test1 = "(let (x 5) x)";;
let let_test2 = "(let (x 10) (inc x))";;
let set_test1 = "(let (x 5) (set x 0))";;
let set_test2 = "(let (x 2) (set x (* x 2)))";;
let while_test1 = "(let (x 1) (while (< x 4) (set x (inc x))) x)";;
let while_test2 = "(let (x 5) (while (= x 5) (set x -5)) x)";;
let while_test3 = "(let (x 2) (while (< x 10) (set x (* x 2))) x)";;
let def_test1 = "(def (min3_or_add2 x : Num) : Num (if (< x 3) (+ 2 x) (- 3 x))) (min3_or_add2 3)";;
let def_test2 = "(def (mult_or_sub x : Num y : Num) : Num (if (< x y) (* x y) (- x y))) (mult_or_sub 2 4)";;
let def_test3 = "(def (make_eq x : Num y : Num) : Bool (while (< x y) (set x (inc x)))) (make_eq 1 5)";;
let def_test4 = "(def (threebools x : Bool y : Bool z : Bool) : Bool (if (= x z) y z)) (threebools true false true)";;
let def_test5 = "(def (fourmix v : Bool x : Num y : Num z : Num) : Num (if v (+ x y) (* y z))) (fourmix false 3 5 7)";;
(* my type mismatch tests *)
let type_test1 = "(let (x 3) (if (< x 5) (set x 5) true))";;
let type_test2 = "(let (x 2) (if (= x true) (inc x) (dec x)))";;
let type_test3 = "(let (x 2) (set y 5))";;
let type_test4 = "(def (mult_sub1 x : Num y : Bool) : Num (if (< x y) (* x y) (- x y))) (mult_sub1 2 4)";;
let type_test5 = "(def (mult_sub2 x : Num y : Num) : Bool (if (< x y) (* x y) (- x y))) (mult_sub2 2 4)";;
let type_test6 = "(def (mult_sub3 x : Num y : Num) : Num (if (< x y) (* x y) (- x y))) (mult_sub3 2 4 5)";;

(* a list of test cases for the parser if you want to test it separately from the compiler *)
let parseTestList =
  [
      t_parse "forty_one_parse" forty_one forty_one_p;
      t_parse "defTest_parse" defTest defTest_p;
      t_parse_err "mult_defs" mult_defs "Parse error: Multiple function definitions f";
      t_parse_err "mult_args" mult_args "Parse error: Multiple function arguments x";
  ]

  (* tests the compiler should pass *)
let annaTestList = 
  [
    t "forty_one" forty_one "41";
    t "forty" forty "40";
    t "add1" add1 "6";
    t "ifTest" ifTest "5";
    t "boolTest" boolTest "1";
    t "assoc_add" assoc_add "10";
    t "sub" sub "1";
    t "assoc_mul" assoc_mul "30";
    t "zero" zero "5";
    t "zero2" zero2 "5";
    t "zero3" zero3 "0";
    t "id" id "5";
    t "nid" nid "-5";
    t "commute" commute "10";
    t "strength" strength "20";
    t "mult" mult "60";
    t "power" power "40";
    t "thrash" thrash "11";
  ]

(* tests that should raise errors *)
let testFailList =
  [
    t_err "failID" failID "Compile error: Unbound variable identifier x";
    t_err "inc_args" inc_args "Compile error: Type Mismatch";
    t_err "plus_args" plus_args "Compile error: Type Mismatch";
    t_err "arith_nested" arith_nested "Compile error: Type Mismatch";
    t_err "if_cond" if_cond "Compile error: Type Mismatch";
    t_err "if_branch" if_branch "Compile error: Type Mismatch";
    t_err "comp" comp "Compile error: Type Mismatch";
    t_err "bad_logic" bad_logic "Compile error: Type Mismatch";
    t_err "fun_body" fun_body "Compile error: Type Mismatch";
    t_err "fun_app" fun_app "Compile error: Type Mismatch";
    t_err "wrong_args1" wrong_args1 "Compile error: Type Mismatch";
    t_err "wrong_args2" wrong_args2 "Compile error: Type Mismatch";
  ]

(* PUT YOUR TESTS HERE *)
let myTestList = 
  [
    (* testing functionality *)
    t "inc_test1" inc_test1 "42";
    t "inc_test2" inc_test2 "77";
    t "dec_test1" dec_test1 "69";
    t "dec_test2" dec_test2 "420";
    t "combo_test1" combo_test1 "528491";
    t "plus_test1" plus_test1 "33";
    t "plus_test2" plus_test2 "15";
    t "minus_test1" minus_test1 "1";
    t "minus_test2" minus_test2 "0";
    t "times_test1" times_test1 "28";
    t "times_test2" times_test2 "64";
    t "times_test3" times_test3 "0";
    t "times_test4" times_test4 "24";
    t "times_test5" times_test5 "-24";
    t "combo_test2" combo_test2 "210";
    t "combo_test3" combo_test3 "-55";
    t "combo_test4" combo_test4 "-24";
    t "comp_test1" comp_test1 "1";
    t "comp_test2" comp_test2 "0";
    t "comp_test3" comp_test3 "1";
    t "let_test1" let_test1 "5";
    t "let_test2" let_test2 "11";
    t "set_test1" set_test1 "0";
    t "set_test2" set_test2 "4";
    t "while_test1" while_test1 "4";
    t "while_test2" while_test2 "-5";
    t "while_test3" while_test3 "16";
    t "def_test1" def_test1 "0";
    t "def_test2" def_test2 "8";
    t "def_test3" def_test3 "0";
    t "def_test4" def_test4 "0";
    t "def_test5" def_test5 "35";
    (* type mismatch test fails *)
    t_err "type_test1" type_test1 "Type Mismatch: if branches must be of same type";
    t_err "type_test2" type_test2 "Type Mismatch: = needs arguments of same type";
    t_err "type_test3" type_test3 "Unbound variable identifier y";
    t_err "type_test4" type_test4 "Type Mismatch: binary operator needs arguments of type Num";
    t_err "type_test5" type_test5 "Type Mismatch: actual function type does not match provided function type";
    t_err "type_test6" type_test6 "Type Mismatch: Expected number of arguments doesn't match provided number of arguments"
  ]

(* appends all the lists of tests together *)
let suite =
  "suite">:::
  parseTestList @ annaTestList @ testFailList @ myTestList

(* run the actual suite *)
let () =
  run_test_tt_main suite
;;
