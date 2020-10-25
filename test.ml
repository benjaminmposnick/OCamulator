open OUnit2

(** [test name expected_output fn_output print_fn] is an OUnit test case named
    [name] that asserts equality between [expected_output] and [fn_output].
    [print_fn] is used to print the inputs when the assertion is false. *)
let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

(** [parse_test name expected_output input] is an OUnit test case named
    [name] that asserts equality between [expected_output] and the expression 
    derived from the result of [Eval.parse input].  *)
let parse_test name expected_output input =
  let parsed_input = Eval.parse input in
  let create_parse_test expr =
    test name expected_output expr Ast.string_of_expr in
  match parsed_input with
  | Command (_, e) -> create_parse_test e
  | Expression e -> create_parse_test e


let parse_tests = let open Ast in [
    (* Var tests *)
    parse_test "variable 1 charcter name" (Var "x") "x";
    parse_test "variable 2 charcter name" (Var "xi") "xi";
    parse_test "variable multi-charcter name" (Var "delta") "delta";

    (* Int tests *)
    parse_test "one digit int" (Int 1) "1";
    parse_test "two digit int" (Int 10) "10";
    parse_test "three digit int" (Int 100) "100";
    parse_test "one digit negative int" (Int ~-1) "-1";
    parse_test "two digit negative int" (Int ~-10) "-10";
    parse_test "three digit negative int" (Int ~-100) "-100";

    (* Float tests *)
    parse_test "1 with one decimal place" (Float 1.0) "1.0";
    parse_test "1 with two decimal places" (Float 1.0) "1.00";
    parse_test "10 with one decimal place" (Float 10.0) "10.0";
    parse_test "10 with two decimal places" (Float 10.0) "10.00";
    parse_test "100 with one decimal place" (Float 100.0) "100.0";
    parse_test "100 with two decimal places" (Float 100.0) "100.00";
    parse_test "float 0.1 with leading zero" (Float 0.1) "0.1";
    parse_test "float 0.1 without leading zero" (Float 0.1) ".1";
    parse_test "float 0.1 with trailing zero" (Float 0.1) "0.10";
    parse_test "float 0.01 with leading zero" (Float 0.01) "0.01";
    parse_test "float 0.01 without leading zero" (Float 0.01) ".01";
    parse_test "float 0.01 with trailing zero" (Float 0.01) "0.010";

    parse_test "-1 with one decimal place" (Float ~-.1.0) "-1.0";
    parse_test "-1 with two decimal places" (Float ~-.1.0) "-1.00";
    parse_test "-10 with one decimal place" (Float ~-.10.0) "-10.0";
    parse_test "-10 with two decimal places" (Float ~-.10.0) "-10.00";
    parse_test "-100 with one decimal place" (Float ~-.100.0) "-100.0";
    parse_test "-100 with two decimal places" (Float ~-.100.0) "-100.00";
    parse_test "float -0.1 with leading zero" (Float ~-.0.1) "-0.1";
    parse_test "float -0.1 without leading zero" (Float ~-.0.1) "-.1";
    parse_test "float -0.1 with trailing zero" (Float ~-.0.1) "-0.10";
    parse_test "float -0.01 with leading zero" (Float ~-.0.01) "-0.01";
    parse_test "float -0.01 without leading zero" (Float ~-.0.01) "-.01";
    parse_test "float -0.01 with trailing zero" (Float ~-.0.01) "-0.010";

    (* Space tests *)
    parse_test "Space left of operator" (Binop (Add, Int 1, Int 2)) "1 +2";
    parse_test "Space right of operator " (Binop (Add, Int 1, Int 2)) "1+ 2";
    parse_test "Space both sides of operator " (Binop (Add, Int 1, Int 2)) "1 + 2";

    (* Add tests *)
    parse_test "Add two Ints" (Binop (Add, Int 1, Int 2)) "1 + 2";
    parse_test "Add two Floats" (Binop (Add, Float 1., Float 2.)) "1.0 + 2.0";
    parse_test "Add two Vars" (Binop (Add, Var "x", Var "y")) "x + y";
    parse_test "Add one Int and one Float" (Binop (Add, Int 1, Float 2.)) "1 + 2.0";
    parse_test "Add one Int and one Var" (Binop (Add, Int 1, Var "x")) "1 + x";
    parse_test "Add one Float and one Var" (Binop (Add, Float 1.0, Var "x")) "1.0 + x";

    parse_test "Add an Int and Binop" (Binop (Add, Binop (Add, Int 1, Int 2), Int 3)) "1 + 2 + 3";
    parse_test "Add an Int and Binop, forced right assoc" (Binop (Add, Int 1, Binop (Add, Int 2, Int 3))) "1 + (2 + 3)";
    parse_test "Add an Float and Binop" (Binop (Add, Float 1.0, Binop (Add, Int 2, Int 3))) "1.0 + (2 + 3)";
    parse_test "Add an Var and Binop" (Binop (Add, Var "x", Binop (Add, Int 2, Int 3))) "x + (2 + 3)";
    parse_test "Add two Binops" (Binop (Add, Binop (Add, Int 2, Int 3), Binop (Add, Int 2, Int 3))) "(2 + 3) + (2 + 3)";
    parse_test "Add three Binops" (Binop (Add, Binop (Add, Binop (Add, Int 2, Int 3), Int 2), Int 3)) "2 + 3 + 2 + 3";

    parse_test "Add - and + Ints" (Binop (Add, Int ~-1, Int 2)) "-1 + 2";
    parse_test "Add + and - Ints" (Binop (Add, Int 1, Int ~-2)) "1 + -2";
    parse_test "Add + and - Ints with parens" (Binop (Add, Int 1, Int ~-2)) "1 + (-2)";
    parse_test "Add two - Ints" (Binop (Add, Int ~-1, Int ~-2)) "-1 + -2";
    parse_test "Add + and - Ints with parens" (Binop (Add, Int ~-1, Int ~-2)) "(-1) + (-2)";

    parse_test "Add - and + Floats" (Binop (Add, Float ~-.1., Float 2.)) "-1.0 + 2.0";
    parse_test "Add + and - Floats" (Binop (Add, Float 1., Float ~-.2.)) "1.0 + -2.0";
    parse_test "Add + and - Floats with parens" (Binop (Add, Float 1., Float ~-.2.)) "1.0 + (-2.0)";
    parse_test "Add two - Floats" (Binop (Add, Float ~-.1., Float ~-.2.)) "-1.0 + -2.0";
    parse_test "Add + and - Floats with parens" (Binop (Add, Float ~-.1., Float ~-.2.)) "(-1.0) + (-2.0)";

    (* Sub tests *)
    parse_test "Subtract two Ints" (Binop (Sub, Int 1, Int 2)) "1 - 2";
    parse_test "Subtract two Floats" (Binop (Sub, Float 1., Float 2.)) "1.0 - 2.0";
    parse_test "Subtract two Vars" (Binop (Sub, Var "x", Var "y")) "x - y";
    parse_test "Subtract two Binops" (Binop (Sub, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "(x + y) - (a + b)";
    parse_test "Subtract one Int and one Float" (Binop (Sub, Int 1, Float 2.)) "1 - 2.0";
    parse_test "Subtract one Int and one Var" (Binop (Sub, Int 1, Var "x")) "1 - x";
    parse_test "Subtract one Float and one Var" (Binop (Sub, Float 1.0, Var "x")) "1.0 - x";

    parse_test "Subtract - and + Ints" (Binop (Sub, Int ~-1, Int 2)) "-1 - 2";
    parse_test "Subtract + and - Ints" (Binop (Sub, Int 1, Int ~-2)) "1 - -2";
    parse_test "Subtract + and - Ints with parens" (Binop (Sub, Int 1, Int ~-2)) "1 - (-2)";
    parse_test "Subtract two - Ints" (Binop (Sub, Int ~-1, Int ~-2)) "-1 - -2";
    parse_test "Subtract + and - Ints with parens" (Binop (Sub, Int ~-1, Int ~-2)) "(-1) - (-2)";

    parse_test "Subtract - and + Floats" (Binop (Sub, Float ~-.1., Float 2.)) "-1.0 - 2.0";
    parse_test "Subtract + and - Floats" (Binop (Sub, Float 1., Float ~-.2.)) "1.0 - -2.0";
    parse_test "Subtract + and - Floats with parens" (Binop (Sub, Float 1., Float ~-.2.)) "1.0 - (-2.0)";
    parse_test "Subtract two - Floats" (Binop (Sub, Float ~-.1., Float ~-.2.)) "-1.0 - -2.0";
    parse_test "Subtract + and - Floats with parens" (Binop (Sub, Float ~-.1., Float ~-.2.)) "(-1.0) - (-2.0)";

    (* Mul tests *)
    parse_test "Multiply two Ints" (Binop (Mul, Int 1, Int 2)) "1 * 2";
    parse_test "Multiply two Floats" (Binop (Mul, Float 1., Float 2.)) "1.0 * 2.0";
    parse_test "Multiply two Vars" (Binop (Mul, Var "x", Var "y")) "x * y";
    parse_test "Multiply two Binops" (Binop (Mul, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "(x + y) * (a + b)";
    parse_test "Multiply one Int and one Float" (Binop (Mul, Int 1, Float 2.)) "1 * 2.0";
    parse_test "Multiply one Int and one Var" (Binop (Mul, Int 1, Var "x")) "1 * x";
    parse_test "Multiply one Float and one Var" (Binop (Mul, Float 1.0, Var "x")) "1.0 * x";
    parse_test "Multiply one Int and one Var shortcut" (Binop (Mul, Int 2, Var "x")) "2x";
    parse_test "Multiply one Float and one Var shortcut" (Binop (Mul, Float 2.0, Var "x")) "2.0x";
    parse_test "Multiply one - Int and one Var shortcut" (Binop (Mul, Int ~-2, Var "x")) "-2x";
    parse_test "Multiply one - Float and one Var shortcut" (Binop (Mul, Float ~-.2.0, Var "x")) "-2.0x";

    (* Div tests *)
    parse_test "Divide two Ints" (Binop (Div, Int 1, Int 2)) "1 / 2";
    parse_test "Divide two Floats" (Binop (Div, Float 1., Float 2.)) "1.0 / 2.0";
    parse_test "Divide two Vars" (Binop (Div, Var "x", Var "y")) "x / y";
    parse_test "Divide two Binops" (Binop (Div, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "(x + y) / (a + b)";
    parse_test "Divide one Int and one Float" (Binop (Div, Int 1, Float 2.)) "1 / 2.0";
    parse_test "Divide one Int and one Var" (Binop (Div, Int 1, Var "x")) "1 / x";
    parse_test "Divide one Float and one Var" (Binop (Div, Float 1.0, Var "x")) "1.0 /  x";

    (* Mod tests *)

    (* Pow tests *)
    parse_test "Exponentiate two Ints" (Binop (Pow, Int 1, Int 2)) "1 ^ 2";
    parse_test "Exponentiate two Floats" (Binop (Pow, Float 1., Float 2.)) "1.0 ^ 2.0";
    parse_test "Exponentiate two Vars" (Binop (Pow, Var "x", Var "y")) "x ^ y";
    parse_test "Exponentiate two Binops" (Binop (Pow, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "(x + y) ^ (a + b)";
    parse_test "Exponentiate one Int and one Float" (Binop (Pow, Int 1, Float 2.)) "1 ^ 2.0";
    parse_test "Exponentiate one Float and one Int" (Binop (Pow, Float 2., Int 1)) "2.0 ^ 1";
    parse_test "Exponentiate one Int and one Var" (Binop (Pow, Int 1, Var "x")) "1 ^ x";
    parse_test "Exponentiate one Var and one Int" (Binop (Pow, Var "x", Int 1)) "x ^ 1";
    parse_test "Exponentiate one Float and one Var" (Binop (Pow, Float 1.0, Var "x")) "1.0 ^ x";
    parse_test "Exponentiate one Var and one Float" (Binop (Pow, Var "x", Float 1.0)) "x ^ 1.0";

    (* Eq tests *)
    parse_test "Equation of Ints" (Binop (Eq, Int 1, Int 1)) "1 = 1";
    parse_test "Equation of Floats" (Binop (Eq, Float 1., Float 1.)) "1.0 = 1.0";
    parse_test "Equation of Vars" (Binop (Eq, Var "x", Var "y")) "x = y";
    parse_test "Equation two Binops" (Binop (Eq, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y = a + b";
    parse_test "Equation of Int and Float" (Binop (Eq, Int 1, Float 1.)) "1 = 1.0";
    parse_test "Equation of Int and Var" (Binop (Eq, Var "x", Int 1)) "x = 1";
    parse_test "Equation of Float and Var" (Binop (Eq, Var "x", Float 1.)) "x = 1.0";

    (* GT tests *)
    parse_test "> inequality of Ints" (Binop (GT, Int 2, Int 1)) "2 > 1";
    parse_test "> inequality of Floats" (Binop (GT, Float 2., Float 1.)) "2.0 > 1.0";
    parse_test "> inequality of Vars" (Binop (GT, Var "x", Var "y")) "x > y";
    parse_test "> inequality  two Binops" (Binop (GT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y > a + b";
    parse_test "> inequality of Int and Float" (Binop (GT, Int 2, Float 1.)) "2 > 1.0";
    parse_test "> inequality of Int and Var" (Binop (GT, Var "x", Int 1)) "x > 1";
    parse_test "> inequality of Float and Var" (Binop (GT, Var "x", Float 1.)) "x > 1.0";

    (* GTE tests *)
    parse_test ">= inequality of Ints" (Binop (GTE, Int 2, Int 1)) "2 >= 1";
    parse_test ">= inequality of Floats" (Binop (GTE, Float 2., Float 1.)) "2.0 >= 1.0";
    parse_test ">= inequality of Vars" (Binop (GTE, Var "x", Var "y")) "x >= y";
    parse_test ">= inequality two Binops" (Binop (GTE, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y >= a + b";
    parse_test ">= inequality of Int and Float" (Binop (GTE, Int 2, Float 1.)) "2 >= 1.0";
    parse_test ">= inequality of Int and Var" (Binop (GTE, Var "x", Int 1)) "x >= 1";
    parse_test ">= inequality of Float and Var" (Binop (GTE, Var "x", Float 1.)) "x >= 1.0";

    (* LT tests *)
    parse_test "< inequality of Ints" (Binop (LT, Int 1, Int 2)) "1 < 2";
    parse_test "< inequality of Floats" (Binop (LT, Float 1., Float 2.)) "1.0 < 2.0";
    parse_test "< inequality of Vars" (Binop (LT, Var "x", Var "y")) "x < y";
    parse_test "< inequality two Binops" (Binop (LT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y < a + b";
    parse_test "< inequality of Int and Float" (Binop (LT, Float 1., Int 2)) "1.0 < 2";
    parse_test "< inequality of Int and Var" (Binop (LT, Var "x", Int 1)) "x < 1";
    parse_test "< inequality of Float and Var" (Binop (LT, Var "x", Float 1.)) "x < 1.0";

    (* LTE tests *)
    parse_test "<= inequality of Ints" (Binop (LTE, Int 1, Int 2)) "1 <= 2";
    parse_test "<= inequality of Floats" (Binop (LTE, Float 1., Float 2.)) "1.0 <= 2.0";
    parse_test "<= inequality of Vars" (Binop (LTE, Var "x", Var "y")) "x <= y";
    parse_test "<= inequality two Binops" (Binop (LTE, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y <= a + b";
    parse_test "<= inequality of Int and Float" (Binop (LTE, Float 1., Int 2)) "1.0 <= 2";
    parse_test "<= inequality of Int and Var" (Binop (LTE, Var "x", Int 1)) "x <= 1";
    parse_test "<= inequality of Float and Var" (Binop (LTE, Var "x", Float 1.)) "x <= 1.0";

    (* Vector tests *)
    parse_test "Int RowVector in R1" (Array (RowVector [1.])) "[1]";
    parse_test "Int RowVector in R2" (Array (RowVector [1.; 2.])) "[1, 2]";
    parse_test "Int RowVector in R3" (Array (RowVector [1.; 2.; 3.])) "[1, 2, 3]";
    parse_test "Float RowVector in R1" (Array (RowVector [1.])) "[1.0]";
    parse_test "Float RowVector in R2" (Array (RowVector [1.; 2.])) "[1.0, 2.0]";
    parse_test "Float RowVector in R3" (Array (RowVector [1.; 2.; 3.])) "[1.0, 2.0, 3.0]";
    parse_test "Mixed type RowVector in R2" (Array (RowVector [0.5; 2.])) "[0.5, 2]";

    parse_test "Int ColumnVector in R2" (Array (ColumnVector [1.; 2.])) "[1; 2]";
    parse_test "Int ColumnVector in R3" (Array (ColumnVector [1.; 2.; 3.])) "[1; 2; 3]";
    parse_test "Float ColumnVector in R2" (Array (ColumnVector [1.; 2.])) "[1.0; 2.0]";
    parse_test "Float ColumnVector in R3" (Array (ColumnVector [1.; 2.; 3.])) "[1.0; 2.0; 3.0]";
    parse_test "Mixed type ColumnVector in R2" (Array (ColumnVector [0.5; 2.])) "[0.5; 2]";

    (* Matrix tests *)
    parse_test "Int Matrix in R(2x2)" (Array (Matrix [[1.;2.]; [3.;4.]])) "[1, 2; 3, 4]";
    parse_test "Int Matrix in R(2x3)" (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]])) "[1, 2, 3; 4, 5, 6]";
    parse_test "Int Matrix in R(3x2)" (Array (Matrix [[1.;2.]; [3.;4.]; [5.;6.]])) "[1, 2; 3, 4; 5, 6]";
    parse_test "Int Matrix in R(2x3)" (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]])) "[1, 2, 3; 4, 5, 6; 7, 8, 9]";
    parse_test "Float Matrix in R(2x2)" (Array (Matrix [[1.;2.]; [3.;4.]])) "[1.0, 2.0; 3.0, 4.0]";
    parse_test "Float Matrix in R(2x3)" (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]])) "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0]";
    parse_test "Float Matrix in R(3x2)" (Array (Matrix [[1.;2.]; [3.;4.]; [5.;6.]])) "[1.0, 2.0; 3.0, 4.0; 5.0, 6.0]";
    parse_test "Float Matrix in R(2x3)" (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]])) "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]";
    parse_test "Mixed type Matrix in R(2x2)" (Array (Matrix [[0.5;2.]; [1.;1.0]])) "[0.5, 2; 1, 1.0]";
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

let eval_tests = []

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    parse_tests;
    modulo_tests;
    var_present_tests
  ]

let _ = run_test_tt_main suite