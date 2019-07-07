open OUnit2
open Ast
open Main

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal 22 (interpret "22"));
  "add"  >:: (fun _ -> assert_equal 22 (interpret "11+11"));
  "adds" >:: (fun _ -> assert_equal 22 (interpret "(10+1)+(5+6)"));
  "let"  >:: (fun _ -> assert_equal 22 (interpret "let x=22 in x"));
  "lets" >:: (fun _ -> assert_equal 22 (interpret "let x = 0 in let x = 22 in x"));
]

let _ = run_test_tt_main ("suite" >::: tests)
