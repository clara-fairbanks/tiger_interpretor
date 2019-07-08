open OUnit2
open Ast
open Main

(* A few test cases *)
let tests = [
  "int"  >:: (fun _ -> assert_equal 40 (interp "40"));
  "add"  >:: (fun _ -> assert_equal 22 (interp "22+18"));
  "add2" >:: (fun _ -> assert_equal 22 (interp "(30+1)+(2+7)"));
  "let"  >:: (fun _ -> assert_equal 22 (interp "let z = 40 in z"));
  "let2" >:: (fun _ -> assert_equal 22 (interp "let z = 0 in let z = 40 in z"));
]

let _ = run_test_tt_main ("suite" >::: tests)
