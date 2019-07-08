open OUnit2
open Ast
open Main

(* A few test cases *)
let calc_tests = [
  "Eval int"  >:: (fun _ -> assert_equal 40 (interpr "40"));
  "Eval Add"  >:: (fun _ -> assert_equal 40 (interpr "22+18"));
  "Eval add 2" >:: (fun _ -> assert_equal 40 (interpr "(30+1)+(2+7)"));
  "Eval let"  >:: (fun _ -> assert_equal 40 (interpr "let z = 40 in z"));
  "Eval let 2" >:: (fun _ -> assert_equal 40 (interpr "let z = 0 in let z = 40 in z"));
]

let _ = run_test_tt_main ("suite" >::: calc_tests)
