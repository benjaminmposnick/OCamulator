open OUnit2
open Ast 
open Eval

let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

let eval_tests = []

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    eval_tests
  ]

let _ = run_test_tt_main suite