open OUnit2
open Ast 

let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

let modulo_tests = let open Eval in [
    test "modulo no remainder" (3 mod 3 |> float_of_int) (Eval.modulo 3. 3.) string_of_float;
    test "modulo with p > q" (4 mod 3 |> float_of_int) (Eval.modulo 4. 3.) string_of_float;
    test "modulo with p < q" (3 mod 4 |> float_of_int) (Eval.modulo 3. 4.) string_of_float;
    test "modulo with -p" (~-3 mod 4 |> float_of_int) (Eval.modulo ~-.3. 4.) string_of_float;
  ]

let eval_tests = []

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    modulo_tests
  ]

let _ = run_test_tt_main suite