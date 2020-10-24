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

let var_present_tests = let open Eval in [
    test "var_present with no var"
      false (Eval.var_present (Binop (Add, Int 3, Int 3)) ) string_of_bool;
    test "var_present with var"
      true (Eval.var_present (Binop (Add, Int 3, Var "x")) ) string_of_bool;  
    test "var_present with var nested"
      true (Eval.var_present (Binop (Add, Binop (Mul, Int 2, Var "y" ), Int 6)))
      string_of_bool;  
    test "var_present with multiple vars"
      true (Eval.var_present (Binop (Add, Var "x", Var "y")) ) string_of_bool;  
  ]

let inverse_tests = let open Inverse in [
    test "basic inverse for addition equation x + 4 = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (inverse (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )) ("x")) Ast.string_of_expr;
  ]

let eval_tests = [] 

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    modulo_tests;
    var_present_tests
  ]
let _ = run_test_tt_main suite