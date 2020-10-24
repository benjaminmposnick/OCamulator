open OUnit2
open Ast

let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

let parse_test name expected_output fn_output =
  test name expected_output fn_output Ast.string_of_expr

let parse_tests = let open Ast in [
    (* Var tests *)
    parse_test "variable 1 charcter name" (Var "x") (Eval.parse "x");
    parse_test "variable 2 charcter name" (Var "xi") (Eval.parse "xi");
    parse_test "variable multi-charcter name" (Var "delta") (Eval.parse "delta");

    (* Int tests *)
    parse_test "one digit int" (Int 1) (Eval.parse "1");
    parse_test "two digit int" (Int 10) (Eval.parse "10");
    parse_test "three digit int" (Int 100) (Eval.parse "100");
    parse_test "one digit negative int" (Int ~-1) (Eval.parse "-1");
    parse_test "two digit negative int" (Int ~-10) (Eval.parse "-10");
    parse_test "three digit negative int" (Int ~-100) (Eval.parse "-100");

    (* Float tests *)
    parse_test "1 with one decimal place" (Float 1.0) (Eval.parse "1.0");
    parse_test "1 with two decimal places" (Float 1.0) (Eval.parse "1.00");
    parse_test "10 with one decimal place" (Float 10.0) (Eval.parse "10.0");
    parse_test "10 with two decimal places" (Float 10.0) (Eval.parse "10.00");
    parse_test "100 with one decimal place" (Float 100.0) (Eval.parse "100.0");
    parse_test "100 with two decimal places" (Float 100.0) (Eval.parse "100.00");
    parse_test "float 0.1 with leading zero" (Float 0.1) (Eval.parse "0.1");
    parse_test "float 0.1 without leading zero" (Float 0.1) (Eval.parse ".1");
    parse_test "float 0.1 with trailing zero" (Float 0.1) (Eval.parse "0.10");
    parse_test "float 0.01 with leading zero" (Float 0.01) (Eval.parse "0.01");
    parse_test "float 0.01 without leading zero" (Float 0.01) (Eval.parse ".01");
    parse_test "float 0.01 with trailing zero" (Float 0.01) (Eval.parse "0.010");

    parse_test "-1 with one decimal place" (Float ~-.1.0) (Eval.parse "-1.0");
    parse_test "-1 with two decimal places" (Float ~-.1.0) (Eval.parse "-1.00");
    parse_test "-10 with one decimal place" (Float ~-.10.0) (Eval.parse "-10.0");
    parse_test "-10 with two decimal places" (Float ~-.10.0) (Eval.parse "-10.00");
    parse_test "-100 with one decimal place" (Float ~-.100.0) (Eval.parse "-100.0");
    parse_test "-100 with two decimal places" (Float ~-.100.0) (Eval.parse "-100.00");
    parse_test "float -0.1 with leading zero" (Float ~-.0.1) (Eval.parse "-0.1");
    parse_test "float -0.1 without leading zero" (Float ~-.0.1) (Eval.parse "-.1");
    parse_test "float -0.1 with trailing zero" (Float ~-.0.1) (Eval.parse "-0.10");
    parse_test "float -0.01 with leading zero" (Float ~-.0.01) (Eval.parse "-0.01");
    parse_test "float -0.01 without leading zero" (Float ~-.0.01) (Eval.parse "-.01");
    parse_test "float -0.01 with trailing zero" (Float ~-.0.01) (Eval.parse "-0.010");

    (* Space tests *)
    parse_test "Space left of operator" (Binop (Add, Int 1, Int 2)) (Eval.parse "1 +2");
    parse_test "Space right of operator " (Binop (Add, Int 1, Int 2)) (Eval.parse "1+ 2");
    parse_test "Space both sides of operator " (Binop (Add, Int 1, Int 2)) (Eval.parse "1 + 2");

    (* Add tests *)
    parse_test "Add two Ints" (Binop (Add, Int 1, Int 2)) (Eval.parse "1 + 2");
    parse_test "Add two Floats" (Binop (Add, Float 1., Float 2.)) (Eval.parse "1.0 + 2.0");
    parse_test "Add two Vars" (Binop (Add, Var "x", Var "y")) (Eval.parse "x + y");
    parse_test "Add one Int and one Float" (Binop (Add, Int 1, Float 2.)) (Eval.parse "1 + 2.0");
    parse_test "Add one Int and one Var" (Binop (Add, Int 1, Var "x")) (Eval.parse "1 + x");
    parse_test "Add one Float and one Var" (Binop (Add, Float 1.0, Var "x")) (Eval.parse "1.0 + x");

    parse_test "Add an Int and Binop" (Binop (Add, Binop (Add, Int 1, Int 2), Int 3)) (Eval.parse "1 + 2 + 3");
    parse_test "Add an Int and Binop, forced right assoc" (Binop (Add, Int 1, Binop (Add, Int 2, Int 3))) (Eval.parse "1 + (2 + 3)");
    parse_test "Add an Float and Binop" (Binop (Add, Float 1.0, Binop (Add, Int 2, Int 3))) (Eval.parse "1.0 + (2 + 3)");
    parse_test "Add an Var and Binop" (Binop (Add, Var "x", Binop (Add, Int 2, Int 3))) (Eval.parse "x + (2 + 3)");
    parse_test "Add two Binops" (Binop (Add, Binop (Add, Int 2, Int 3), Binop (Add, Int 2, Int 3))) (Eval.parse "(2 + 3) + (2 + 3)");
    parse_test "Add three Binops" (Binop (Add, Binop (Add, Binop (Add, Int 2, Int 3), Int 2), Int 3)) (Eval.parse "2 + 3 + 2 + 3");

    parse_test "Add - and + Ints" (Binop (Add, Int ~-1, Int 2)) (Eval.parse "-1 + 2");
    parse_test "Add + and - Ints" (Binop (Add, Int 1, Int ~-2)) (Eval.parse "1 + -2");
    parse_test "Add + and - Ints with parens" (Binop (Add, Int 1, Int ~-2)) (Eval.parse "1 + (-2)");
    parse_test "Add two - Ints" (Binop (Add, Int ~-1, Int ~-2)) (Eval.parse "-1 + -2");
    parse_test "Add + and - Ints with parens" (Binop (Add, Int ~-1, Int ~-2)) (Eval.parse "(-1) + (-2)");

    parse_test "Add - and + Floats" (Binop (Add, Float ~-.1., Float 2.)) (Eval.parse "-1.0 + 2.0");
    parse_test "Add + and - Floats" (Binop (Add, Float 1., Float ~-.2.)) (Eval.parse "1.0 + -2.0");
    parse_test "Add + and - Floats with parens" (Binop (Add, Float 1., Float ~-.2.)) (Eval.parse "1.0 + (-2.0)");
    parse_test "Add two - Floats" (Binop (Add, Float ~-.1., Float ~-.2.)) (Eval.parse "-1.0 + -2.0");
    parse_test "Add + and - Floats with parens" (Binop (Add, Float ~-.1., Float ~-.2.)) (Eval.parse "(-1.0) + (-2.0)");

    (* Sub tests *)
    parse_test "Subtract two Ints" (Binop (Sub, Int 1, Int 2)) (Eval.parse "1 - 2");
    parse_test "Subtract two Floats" (Binop (Sub, Float 1., Float 2.)) (Eval.parse "1.0 - 2.0");
    parse_test "Subtract two Vars" (Binop (Sub, Var "x", Var "y")) (Eval.parse "x - y");
    parse_test "Subtract two Binops" (Binop (Sub, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "(x + y) - (a + b)");
    parse_test "Subtract one Int and one Float" (Binop (Sub, Int 1, Float 2.)) (Eval.parse "1 - 2.0");
    parse_test "Subtract one Int and one Var" (Binop (Sub, Int 1, Var "x")) (Eval.parse "1 - x");
    parse_test "Subtract one Float and one Var" (Binop (Sub, Float 1.0, Var "x")) (Eval.parse "1.0 - x");

    parse_test "Subtract - and + Ints" (Binop (Sub, Int ~-1, Int 2)) (Eval.parse "-1 - 2");
    parse_test "Subtract + and - Ints" (Binop (Sub, Int 1, Int ~-2)) (Eval.parse "1 - -2");
    parse_test "Subtract + and - Ints with parens" (Binop (Sub, Int 1, Int ~-2)) (Eval.parse "1 - (-2)");
    parse_test "Subtract two - Ints" (Binop (Sub, Int ~-1, Int ~-2)) (Eval.parse "-1 - -2");
    parse_test "Subtract + and - Ints with parens" (Binop (Sub, Int ~-1, Int ~-2)) (Eval.parse "(-1) - (-2)");

    parse_test "Subtract - and + Floats" (Binop (Sub, Float ~-.1., Float 2.)) (Eval.parse "-1.0 - 2.0");
    parse_test "Subtract + and - Floats" (Binop (Sub, Float 1., Float ~-.2.)) (Eval.parse "1.0 - -2.0");
    parse_test "Subtract + and - Floats with parens" (Binop (Sub, Float 1., Float ~-.2.)) (Eval.parse "1.0 - (-2.0)");
    parse_test "Subtract two - Floats" (Binop (Sub, Float ~-.1., Float ~-.2.)) (Eval.parse "-1.0 - -2.0");
    parse_test "Subtract + and - Floats with parens" (Binop (Sub, Float ~-.1., Float ~-.2.)) (Eval.parse "(-1.0) - (-2.0)");

    (* Mul tests *)
    parse_test "Multiply two Ints" (Binop (Mul, Int 1, Int 2)) (Eval.parse "1 * 2");
    parse_test "Multiply two Floats" (Binop (Mul, Float 1., Float 2.)) (Eval.parse "1.0 * 2.0");
    parse_test "Multiply two Vars" (Binop (Mul, Var "x", Var "y")) (Eval.parse "x * y");
    parse_test "Multiply two Binops" (Binop (Mul, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "(x + y) * (a + b)");
    parse_test "Multiply one Int and one Float" (Binop (Mul, Int 1, Float 2.)) (Eval.parse "1 * 2.0");
    parse_test "Multiply one Int and one Var" (Binop (Mul, Int 1, Var "x")) (Eval.parse "1 * x");
    parse_test "Multiply one Float and one Var" (Binop (Mul, Float 1.0, Var "x")) (Eval.parse "1.0 * x");
    parse_test "Multiply one Int and one Var shortcut" (Binop (Mul, Int 2, Var "x")) (Eval.parse "2x");
    parse_test "Multiply one Float and one Var shortcut" (Binop (Mul, Float 2.0, Var "x")) (Eval.parse "2.0x");
    parse_test "Multiply one - Int and one Var shortcut" (Binop (Mul, Int ~-2, Var "x")) (Eval.parse "-2x");
    parse_test "Multiply one - Float and one Var shortcut" (Binop (Mul, Float ~-.2.0, Var "x")) (Eval.parse "-2.0x");

    (* Div tests *)
    parse_test "Divide two Ints" (Binop (Div, Int 1, Int 2)) (Eval.parse "1 / 2");
    parse_test "Divide two Floats" (Binop (Div, Float 1., Float 2.)) (Eval.parse "1.0 / 2.0");
    parse_test "Divide two Vars" (Binop (Div, Var "x", Var "y")) (Eval.parse "x / y");
    parse_test "Divide two Binops" (Binop (Div, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "(x + y) / (a + b)");
    parse_test "Divide one Int and one Float" (Binop (Div, Int 1, Float 2.)) (Eval.parse "1 / 2.0");
    parse_test "Divide one Int and one Var" (Binop (Div, Int 1, Var "x")) (Eval.parse "1 / x");
    parse_test "Divide one Float and one Var" (Binop (Div, Float 1.0, Var "x")) (Eval.parse "1.0 /  x");

    (* Mod tests *)

    (* Pow tests *)
    parse_test "Exponentiate two Ints" (Binop (Pow, Int 1, Int 2)) (Eval.parse "1 ^ 2");
    parse_test "Exponentiate two Floats" (Binop (Pow, Float 1., Float 2.)) (Eval.parse "1.0 ^ 2.0");
    parse_test "Exponentiate two Vars" (Binop (Pow, Var "x", Var "y")) (Eval.parse "x ^ y");
    parse_test "Exponentiate two Binops" (Binop (Pow, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "(x + y) ^ (a + b)");
    parse_test "Exponentiate one Int and one Float" (Binop (Pow, Int 1, Float 2.)) (Eval.parse "1 ^ 2.0");
    parse_test "Exponentiate one Float and one Int" (Binop (Pow, Float 2., Int 1)) (Eval.parse "2.0 ^ 1");
    parse_test "Exponentiate one Int and one Var" (Binop (Pow, Int 1, Var "x")) (Eval.parse "1 ^ x");
    parse_test "Exponentiate one Var and one Int" (Binop (Pow, Var "x", Int 1)) (Eval.parse "x ^ 1");
    parse_test "Exponentiate one Float and one Var" (Binop (Pow, Float 1.0, Var "x")) (Eval.parse "1.0 ^ x");
    parse_test "Exponentiate one Var and one Float" (Binop (Pow, Var "x", Float 1.0)) (Eval.parse "x ^ 1.0");

    (* Eq tests *)
    parse_test "Equation of Ints" (Binop (Eq, Int 1, Int 1)) (Eval.parse "1 = 1");
    parse_test "Equation of Floats" (Binop (Eq, Float 1., Float 1.)) (Eval.parse "1.0 = 1.0");
    parse_test "Equation of Vars" (Binop (Eq, Var "x", Var "y")) (Eval.parse "x = y");
    parse_test "Equation two Binops" (Binop (Eq, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "x + y = a + b");
    parse_test "Equation of Int and Float" (Binop (Eq, Int 1, Float 1.)) (Eval.parse "1 = 1.0");
    parse_test "Equation of Int and Var" (Binop (Eq, Var "x", Int 1)) (Eval.parse "x = 1");
    parse_test "Equation of Float and Var" (Binop (Eq, Var "x", Float 1.)) (Eval.parse "x = 1.0");

    (* GT tests *)
    parse_test "> inequality of Ints" (Binop (GT, Int 2, Int 1)) (Eval.parse "2 > 1");
    parse_test "> inequality of Floats" (Binop (GT, Float 2., Float 1.)) (Eval.parse "2.0 > 1.0");
    parse_test "> inequality of Vars" (Binop (GT, Var "x", Var "y")) (Eval.parse "x > y");
    parse_test "> inequality  two Binops" (Binop (GT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "x + y > a + b");
    parse_test "> inequality of Int and Float" (Binop (GT, Int 2, Float 1.)) (Eval.parse "2 > 1.0");
    parse_test "> inequality of Int and Var" (Binop (GT, Var "x", Int 1)) (Eval.parse "x > 1");
    parse_test "> inequality of Float and Var" (Binop (GT, Var "x", Float 1.)) (Eval.parse "x > 1.0");

    (* GTE tests *)
    parse_test ">= inequality of Ints" (Binop (GTE, Int 2, Int 1)) (Eval.parse "2 >= 1");
    parse_test ">= inequality of Floats" (Binop (GTE, Float 2., Float 1.)) (Eval.parse "2.0 >= 1.0");
    parse_test ">= inequality of Vars" (Binop (GTE, Var "x", Var "y")) (Eval.parse "x >= y");
    parse_test ">= inequality two Binops" (Binop (GTE, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "x + y >= a + b");
    parse_test ">= inequality of Int and Float" (Binop (GTE, Int 2, Float 1.)) (Eval.parse "2 >= 1.0");
    parse_test ">= inequality of Int and Var" (Binop (GTE, Var "x", Int 1)) (Eval.parse "x >= 1");
    parse_test ">= inequality of Float and Var" (Binop (GTE, Var "x", Float 1.)) (Eval.parse "x >= 1.0");

    (* LT tests *)
    parse_test "< inequality of Ints" (Binop (LT, Int 1, Int 2)) (Eval.parse "1 < 2");
    parse_test "< inequality of Floats" (Binop (LT, Float 1., Float 2.)) (Eval.parse "1.0 < 2.0");
    parse_test "< inequality of Vars" (Binop (LT, Var "x", Var "y")) (Eval.parse "x < y");
    parse_test "< inequality two Binops" (Binop (LT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "x + y < a + b");
    parse_test "< inequality of Int and Float" (Binop (LT, Float 1., Int 2)) (Eval.parse "1.0 < 2");
    parse_test "< inequality of Int and Var" (Binop (LT, Var "x", Int 1)) (Eval.parse "x < 1");
    parse_test "< inequality of Float and Var" (Binop (LT, Var "x", Float 1.)) (Eval.parse "x < 1.0");

    (* LTE tests *)
    parse_test "<= inequality of Ints" (Binop (LTE, Int 1, Int 2)) (Eval.parse "1 <= 2");
    parse_test "<= inequality of Floats" (Binop (LTE, Float 1., Float 2.)) (Eval.parse "1.0 <= 2.0");
    parse_test "<= inequality of Vars" (Binop (LTE, Var "x", Var "y")) (Eval.parse "x <= y");
    parse_test "<= inequality two Binops" (Binop (LTE, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) (Eval.parse "x + y <= a + b");
    parse_test "<= inequality of Int and Float" (Binop (LTE, Float 1., Int 2)) (Eval.parse "1.0 <= 2");
    parse_test "<= inequality of Int and Var" (Binop (LTE, Var "x", Int 1)) (Eval.parse "x <= 1");
    parse_test "<= inequality of Float and Var" (Binop (LTE, Var "x", Float 1.)) (Eval.parse "x <= 1.0");

    (* Vector tests *)
    parse_test "Int Vector in R1" (Vector [1.]) (Eval.parse "[1]");
    parse_test "Int Vector in R2" (Vector [1.; 2.]) (Eval.parse "[1, 2]");
    parse_test "Int Vector in R3" (Vector [1.; 2.; 3.]) (Eval.parse "[1, 2, 3]");
    parse_test "Float Vector in R1" (Vector [1.]) (Eval.parse "[1.0]");
    parse_test "Float Vector in R2" (Vector [1.; 2.]) (Eval.parse "[1.0, 2.0]");
    parse_test "Float Vector in R3" (Vector [1.; 2.; 3.]) (Eval.parse "[1.0, 2.0, 3.0]");
    parse_test "Mixed type Vector in R2" (Vector [0.5; 2.]) (Eval.parse "[0.5, 2]");

    (* Matrix tests *)
    parse_test "Int Matrix in R(2x2)" (Matrix [[1.;2.]; [3.;4.]]) (Eval.parse "[1, 2; 3, 4]");
    parse_test "Int Matrix in R(2x3)" (Matrix [[1.;2.;3.]; [4.;5.;6.]]) (Eval.parse "[1, 2, 3; 4, 5, 6]");
    parse_test "Int Matrix in R(3x2)" (Matrix [[1.;2.]; [3.;4.]; [5.;6.]]) (Eval.parse "[1, 2; 3, 4; 5, 6]");
    parse_test "Int Matrix in R(2x3)" (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]) (Eval.parse "[1, 2, 3; 4, 5, 6; 7, 8, 9]");
    parse_test "Int Matrix in R(2x2)" (Matrix [[1.;2.]; [3.;4.]]) (Eval.parse "[1.0, 2.0; 3.0, 4.0]");
    parse_test "Int Matrix in R(2x3)" (Matrix [[1.;2.;3.]; [4.;5.;6.]]) (Eval.parse "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0]");
    parse_test "Int Matrix in R(3x2)" (Matrix [[1.;2.]; [3.;4.]; [5.;6.]]) (Eval.parse "[1.0, 2.0; 3.0, 4.0; 5.0, 6.0]");
    parse_test "Int Matrix in R(2x3)" (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]) (Eval.parse "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]");
    parse_test "Mixed type Matrix in R(2x2)" (Matrix [[0.5;2.]; [1.;1.0]]) (Eval.parse "[0.5, 2; 1, 1.0]");
  ]

let modulo_tests = [
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
      (Inverse.inverse (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )) ("x")) Ast.string_of_expr;
    test "basic inverse for addition equation 4 + x = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (Inverse.inverse (Binop(Eq, Binop(Add, Int 4, Var "x"), Int 5 )) ("x")) Ast.string_of_expr;
    test "basic inverse for subtraction equation x - 4 = 5" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Int 4))) 
      (Inverse.inverse (Binop(Eq, Binop(Sub, Var "x", Int 4), Int 5 )) ("x")) Ast.string_of_expr;
    test "basic inverse for subtraction equation 4 - x = 5" 
      (Binop(Eq, Binop(Sub, Int 4, Int 5), Var "x")) 
      (Inverse.inverse (Binop(Eq, Binop(Sub, Int 4, Var "x"), Int 5 )) ("x")) Ast.string_of_expr;
  ]

let eval_tests = [] 

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    parse_tests;
    modulo_tests;
    var_present_tests;
    inverse_tests
  ]
let _ = run_test_tt_main suite