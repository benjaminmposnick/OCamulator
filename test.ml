open OUnit2
open Ast

(** [test name expected_output fn_output print_fn] is an OUnit test case named
    [name] that asserts equality between [expected_output] and [fn_output].
    [print_fn] is used to print the inputs if the assertion is false. *)
let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

(** [parse str] is the abstract syntax tree that results from lexing and
    parsing [str]. *)
let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.read lexbuf

(** [parse_test name expected_output input] is an OUnit test case named
    [name] that asserts equality between [expected_output] and the expression 
    resulting from [parse input].  *)
let parse_test name expected_output input =
  let parsed_input = parse input in
  test name expected_output parsed_input Ast.string_of_expr 

let parse_tests = [
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
  parse_test "Space both sides of operator "
    (Binop (Add, Int 1, Int 2)) "1 + 2";

  (* Add tests *)
  parse_test "Add two Ints" (Binop (Add, Int 1, Int 2)) "1 + 2";
  parse_test "Add two Floats" (Binop (Add, Float 1., Float 2.)) "1.0 + 2.0";
  parse_test "Add two Vars" (Binop (Add, Var "x", Var "y")) "x + y";
  parse_test "Add one Int and one Float"
    (Binop (Add, Int 1, Float 2.)) "1 + 2.0";
  parse_test "Add one Int and one Var" (Binop (Add, Int 1, Var "x")) "1 + x";
  parse_test "Add one Float and one Var"
    (Binop (Add, Float 1.0, Var "x")) "1.0 + x";

  parse_test "Add an Int and Binop"
    (Binop (Add, Binop (Add, Int 1, Int 2), Int 3)) "1 + 2 + 3";
  parse_test "Add an Int and Binop, forced right assoc"
    (Binop (Add, Int 1, Binop (Add, Int 2, Int 3))) "1 + (2 + 3)";
  parse_test "Add an Float and Binop"
    (Binop (Add, Float 1.0, Binop (Add, Int 2, Int 3))) "1.0 + (2 + 3)";
  parse_test "Add an Var and Binop"
    (Binop (Add, Var "x", Binop (Add, Int 2, Int 3))) "x + (2 + 3)";
  parse_test "Add two Binops"
    (Binop (Add, Binop (Add, Int 2, Int 3), Binop (Add, Int 2, Int 3)))
    "(2 + 3) + (2 + 3)";
  parse_test "Add three Binops"
    (Binop (Add, Binop (Add, Binop (Add, Int 2, Int 3), Int 2), Int 3))
    "2 + 3 + 2 + 3";

  parse_test "Add - and + Ints" (Binop (Add, Int ~-1, Int 2)) "-1 + 2";
  parse_test "Add + and - Ints" (Binop (Add, Int 1, Int ~-2)) "1 + -2";
  parse_test "Add + and - Ints with parens"
    (Binop (Add, Int 1, Int ~-2)) "1 + (-2)";
  parse_test "Add two - Ints" (Binop (Add, Int ~-1, Int ~-2)) "-1 + -2";
  parse_test "Add + and - Ints with parens"
    (Binop (Add, Int ~-1, Int ~-2)) "(-1) + (-2)";

  parse_test "Add - and + Floats"
    (Binop (Add, Float ~-.1., Float 2.)) "-1.0 + 2.0";
  parse_test "Add + and - Floats"
    (Binop (Add, Float 1., Float ~-.2.)) "1.0 + -2.0";
  parse_test "Add + and - Floats with parens"
    (Binop (Add, Float 1., Float ~-.2.)) "1.0 + (-2.0)";
  parse_test "Add two - Floats"
    (Binop (Add, Float ~-.1., Float ~-.2.)) "-1.0 + -2.0";
  parse_test "Add + and - Floats with parens"
    (Binop (Add, Float ~-.1., Float ~-.2.)) "(-1.0) + (-2.0)";

  (* Sub tests *)
  parse_test "Subtract two Ints"
    (Binop (Sub, Int 1, Int 2)) "1 - 2";
  parse_test "Subtract two Floats"
    (Binop (Sub, Float 1., Float 2.)) "1.0 - 2.0";
  parse_test "Subtract two Vars"
    (Binop (Sub, Var "x", Var "y")) "x - y";
  parse_test "Subtract two Binops"
    (Binop (Sub, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "(x + y) - (a + b)";
  parse_test "Subtract one Int and one Float"
    (Binop (Sub, Int 1, Float 2.)) "1 - 2.0";
  parse_test "Subtract one Int and one Var"
    (Binop (Sub, Int 1, Var "x")) "1 - x";
  parse_test "Subtract one Float and one Var"
    (Binop (Sub, Float 1.0, Var "x")) "1.0 - x";

  parse_test "Subtract - and + Ints" (Binop (Sub, Int ~-1, Int 2)) "-1 - 2";
  parse_test "Subtract + and - Ints" (Binop (Sub, Int 1, Int ~-2)) "1 - -2";
  parse_test "Subtract + and - Ints with parens"
    (Binop (Sub, Int 1, Int ~-2)) "1 - (-2)";
  parse_test "Subtract two - Ints" (Binop (Sub, Int ~-1, Int ~-2)) "-1 - -2";
  parse_test "Subtract + and - Ints with parens"
    (Binop (Sub, Int ~-1, Int ~-2)) "(-1) - (-2)";

  parse_test "Subtract - and + Floats"
    (Binop (Sub, Float ~-.1., Float 2.)) "-1.0 - 2.0";
  parse_test "Subtract + and - Floats"
    (Binop (Sub, Float 1., Float ~-.2.)) "1.0 - -2.0";
  parse_test "Subtract + and - Floats with parens"
    (Binop (Sub, Float 1., Float ~-.2.)) "1.0 - (-2.0)";
  parse_test "Subtract two - Floats"
    (Binop (Sub, Float ~-.1., Float ~-.2.)) "-1.0 - -2.0";
  parse_test "Subtract + and - Floats with parens"
    (Binop (Sub, Float ~-.1., Float ~-.2.)) "(-1.0) - (-2.0)";

  (* Mul tests *)
  parse_test "Multiply two Ints" (Binop (Mul, Int 1, Int 2)) "1 * 2";
  parse_test "Multiply two Floats" (Binop (Mul, Float 1., Float 2.)) "1.0 * 2.0";
  parse_test "Multiply two Vars" (Binop (Mul, Var "x", Var "y")) "x * y";
  parse_test "Multiply two Binops"
    (Binop (Mul, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "(x + y) * (a + b)";
  parse_test "Multiply one Int and one Float"
    (Binop (Mul, Int 1, Float 2.)) "1 * 2.0";
  parse_test "Multiply one Int and one Var"
    (Binop (Mul, Int 1, Var "x")) "1 * x";
  parse_test "Multiply one Float and one Var"
    (Binop (Mul, Float 1.0, Var "x")) "1.0 * x";
  parse_test "Multiply one Int and one Var shortcut"
    (Binop (Mul, Int 2, Var "x")) "2x";
  parse_test "Multiply one Float and one Var shortcut"
    (Binop (Mul, Float 2.0, Var "x")) "2.0x";
  parse_test "Multiply one - Int and one Var shortcut"
    (Binop (Mul, Int ~-2, Var "x")) "-2x";
  parse_test "Multiply one - Float and one Var shortcut"
    (Binop (Mul, Float ~-.2.0, Var "x")) "-2.0x";

  (* Div tests *)
  parse_test "Divide two Ints" (Binop (Div, Int 1, Int 2)) "1 / 2";
  parse_test "Divide two Floats" (Binop (Div, Float 1., Float 2.)) "1.0 / 2.0";
  parse_test "Divide two Vars" (Binop (Div, Var "x", Var "y")) "x / y";
  parse_test "Divide two Binops"
    (Binop (Div, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "(x + y) / (a + b)";
  parse_test "Divide one Int and one Float"
    (Binop (Div, Int 1, Float 2.)) "1 / 2.0";
  parse_test "Divide one Int and one Var"(Binop (Div, Int 1, Var "x")) "1 / x";
  parse_test "Divide one Float and one Var"
    (Binop (Div, Float 1.0, Var "x")) "1.0 / x";

  (* Pow tests *)
  parse_test "Exponentiate two Ints" (Binop (Pow, Int 1, Int 2)) "1 ^ 2";
  parse_test "Exponentiate two Floats"
    (Binop (Pow, Float 1., Float 2.)) "1.0 ^ 2.0";
  parse_test "Exponentiate two Vars" (Binop (Pow, Var "x", Var "y")) "x ^ y";
  parse_test "Exponentiate two Binops"
    (Binop (Pow, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "(x + y) ^ (a + b)";
  parse_test "Exponentiate one Int and one Float"
    (Binop (Pow, Int 1, Float 2.)) "1 ^ 2.0";
  parse_test "Exponentiate one Float and one Int"
    (Binop (Pow, Float 2., Int 1)) "2.0 ^ 1";
  parse_test "Exponentiate one Int and one Var"
    (Binop (Pow, Int 1, Var "x")) "1 ^ x";
  parse_test "Exponentiate one Var and one Int"
    (Binop (Pow, Var "x", Int 1)) "x ^ 1";
  parse_test "Exponentiate one Float and one Var"
    (Binop (Pow, Float 1.0, Var "x")) "1.0 ^ x";
  parse_test "Exponentiate one Var and one Float"
    (Binop (Pow, Var "x", Float 1.0)) "x ^ 1.0";

  (* Eq tests *)
  parse_test "Equation of Ints" (Binop (Eq, Int 1, Int 1)) "1 = 1";
  parse_test "Equation of Floats" (Binop (Eq, Float 1., Float 1.)) "1.0 = 1.0";
  parse_test "Equation of Vars" (Binop (Eq, Var "x", Var "y")) "x = y";
  parse_test "Equation two Binops"
    (Binop (Eq, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "x + y = a + b";
  parse_test "Equation of Int and Float" (Binop (Eq, Int 1, Float 1.)) "1 = 1.0";
  parse_test "Equation of Int and Var" (Binop (Eq, Var "x", Int 1)) "x = 1";
  parse_test "Equation of Float and Var"
    (Binop (Eq, Var "x", Float 1.)) "x = 1.0";

  (* GT tests *)
  parse_test "> inequality of Ints" (Binop (GT, Int 2, Int 1)) "2 > 1";
  parse_test "> inequality of Floats"
    (Binop (GT, Float 2., Float 1.)) "2.0 > 1.0";
  parse_test "> inequality of Vars" (Binop (GT, Var "x", Var "y")) "x > y";
  parse_test "> inequality  two Binops"
    (Binop (GT, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "x + y > a + b";
  parse_test "> inequality of Int and Float"
    (Binop (GT, Int 2, Float 1.)) "2 > 1.0";
  parse_test "> inequality of Int and Var" (Binop (GT, Var "x", Int 1)) "x > 1";
  parse_test "> inequality of Float and Var"
    (Binop (GT, Var "x", Float 1.)) "x > 1.0";

  (* GTE tests *)
  parse_test ">= inequality of Ints" (Binop (GTE, Int 2, Int 1)) "2 >= 1";
  parse_test ">= inequality of Floats"
    (Binop (GTE, Float 2., Float 1.)) "2.0 >= 1.0";
  parse_test ">= inequality of Vars" (Binop (GTE, Var "x", Var "y")) "x >= y";
  parse_test ">= inequality two Binops"
    (Binop (GTE, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "x + y >= a + b";
  parse_test ">= inequality of Int and Float"
    (Binop (GTE, Int 2, Float 1.)) "2 >= 1.0";
  parse_test ">= inequality of Int and Var"
    (Binop (GTE, Var "x", Int 1)) "x >= 1";
  parse_test ">= inequality of Float and Var"
    (Binop (GTE, Var "x", Float 1.)) "x >= 1.0";

  (* LT tests *)
  parse_test "< inequality of Ints" (Binop (LT, Int 1, Int 2)) "1 < 2";
  parse_test "< inequality of Floats"
    (Binop (LT, Float 1., Float 2.)) "1.0 < 2.0";
  parse_test "< inequality of Vars" (Binop (LT, Var "x", Var "y")) "x < y";
  parse_test "< inequality two Binops"
    (Binop (LT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b"))) "x + y < a + b";
  parse_test "< inequality of Int and Float"
    (Binop (LT, Float 1., Int 2)) "1.0 < 2";
  parse_test "< inequality of Int and Var" (Binop (LT, Var "x", Int 1)) "x < 1";
  parse_test "< inequality of Float and Var"
    (Binop (LT, Var "x", Float 1.)) "x < 1.0";

  (* LTE tests *)
  parse_test "<= inequality of Ints" (Binop (LTE, Int 1, Int 2)) "1 <= 2";
  parse_test "<= inequality of Floats"
    (Binop (LTE, Float 1., Float 2.)) "1.0 <= 2.0";
  parse_test "<= inequality of Vars" (Binop (LTE, Var "x", Var "y")) "x <= y";
  parse_test "<= inequality two Binops"
    (Binop (LTE, Binop(Add, Var "x", Var "y"),
            Binop(Add, Var "a", Var "b"))) "x + y <= a + b";
  parse_test "<= inequality of Int and Float"
    (Binop (LTE, Float 1., Int 2)) "1.0 <= 2";
  parse_test "<= inequality of Int and Var"
    (Binop (LTE, Var "x", Int 1)) "x <= 1";
  parse_test "<= inequality of Float and Var"
    (Binop (LTE, Var "x", Float 1.)) "x <= 1.0";

  (* Vector tests *)
  parse_test "Int RowVector in R1" (Array (RowVector [1.])) "[1]";
  parse_test "Int RowVector in R2" (Array (RowVector [1.; 2.])) "[1, 2]";
  parse_test "Int RowVector in R3" (Array (RowVector [1.; 2.; 3.])) "[1, 2, 3]";
  parse_test "Float RowVector in R1" (Array (RowVector [1.])) "[1.0]";
  parse_test "Float RowVector in R2" (Array (RowVector [1.; 2.])) "[1.0, 2.0]";
  parse_test "Float RowVector in R3"
    (Array (RowVector [1.; 2.; 3.])) "[1.0, 2.0, 3.0]";
  parse_test "Mixed type RowVector in R2"
    (Array (RowVector [0.5; 2.])) "[0.5, 2]";

  parse_test "Int ColumnVector in R2" (Array (ColumnVector [1.; 2.])) "[1; 2]";
  parse_test "Int ColumnVector in R3"
    (Array (ColumnVector [1.; 2.; 3.])) "[1; 2; 3]";
  parse_test "Float ColumnVector in R2"
    (Array (ColumnVector [1.; 2.])) "[1.0; 2.0]";
  parse_test "Float ColumnVector in R3"
    (Array (ColumnVector [1.; 2.; 3.])) "[1.0; 2.0; 3.0]";
  parse_test "Mixed type ColumnVector in R2"
    (Array (ColumnVector [0.5; 2.])) "[0.5; 2]";

  (* Matrix tests *)
  parse_test "Int Matrix in R(2x2)"
    (Array (Matrix [[1.;2.]; [3.;4.]])) "[1, 2; 3, 4]";
  parse_test "Int Matrix in R(2x3)"
    (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]])) "[1, 2, 3; 4, 5, 6]";
  parse_test "Int Matrix in R(3x2)" 
    (Array (Matrix [[1.;2.]; [3.;4.]; [5.;6.]])) "[1, 2; 3, 4; 5, 6]";
  parse_test "Int Matrix in R(2x3)" 
    (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]))
    "[1, 2, 3; 4, 5, 6; 7, 8, 9]";
  parse_test "Float Matrix in R(2x2)" 
    (Array (Matrix [[1.;2.]; [3.;4.]])) "[1.0, 2.0; 3.0, 4.0]";
  parse_test "Float Matrix in R(2x3)" 
    (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]])) "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0]";
  parse_test "Float Matrix in R(3x2)" 
    (Array (Matrix [[1.;2.]; [3.;4.]; [5.;6.]])) "[1.0, 2.0; 3.0, 4.0; 5.0, 6.0]";
  parse_test "Float Matrix in R(2x3)" 
    (Array (Matrix [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]])) 
    "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]";
  parse_test "Mixed type Matrix in R(2x2)"
    (Array (Matrix [[0.5;2.]; [1.;1.0]])) "[0.5, 2; 1, 1.0]";
]

let parse_text_file filename =
  let ic = open_in filename in
  let line = input_line ic in
  parse line

let matrix_to_list = function
  | Array (Matrix lst) -> lst
  | _ -> failwith "Invalid input"

let matrix_tests = [
  test "row reduce 3x3 matrix with two pivot columns" 
    (Matrix [[1.;0.;~-.1.];[0.;1.;2.];[0.;0.;0.]])
    (Matrix (Linalg.rref [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]))
    (fun m -> string_of_expr (Array m));
  test "row reduce 3x4 matrix with three pivot columns" 
    (Matrix [[1.;0.;~-.1.;~-.0.];[0.;1.;2.;0.];[0.;0.;0.;1.]])
    (Matrix (Linalg.rref [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;~-.12.]]))
    (fun m -> string_of_expr (Array m));
  test "row reduce 3x4 matrix with two pivot columns" 
    (Matrix [[1.;0.;~-.1.;~-.2.];[0.;1.;2.;3.];[0.;0.;0.;0.]])
    (Matrix (Linalg.rref [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;12.]]))
    (fun m -> string_of_expr (Array m));
  test "row reduce 4x4 matrix with two pivot columns" 
    (Matrix [[1.;0.;~-.1.;~-.2.];[0.;1.;2.;3.];[0.;0.;0.;0.];[0.;0.;0.;0.]])
    (Matrix (Linalg.rref [[1.;2.;3.;4.];[5.;6.;7.;8.];
                          [9.;10.;11.;12.];[13.;14.;15.;16.]]))
    (fun m -> string_of_expr (Array m));
  test "row reduce 4x5 matrix with four pivot columns" 
    (Matrix [[1.;0.;~-.3.;0.;0.];[0.;1.;2.;0.;0.];
             [0.;0.;0.;1.;0.];[0.;0.;0.;0.;1.]])
    (Matrix (Linalg.rref [[0.;~-.3.;~-.6.;4.;9.];[~-.1.;~-.2.;~-.1.;3.;1.];
                          [~-.2.;~-.3.;0.;3.;~-.1.];[1.;4.;5.;~-.9.;~-.9.]]))
    (fun m -> string_of_expr (Array m));
  test "row reduce 10x10 random int matrix with 10 pivot columns" 
    (parse_text_file "./tests/rref/10x10_int_out.txt")
    (Array (Matrix (Linalg.rref (
         parse_text_file "./tests/rref/10x10_int_in.txt" |> matrix_to_list))))
    (fun m -> string_of_expr m);
  test "row reduce 5x7 random float matrix with 5 pivot columns" 
    (parse_text_file "./tests/rref/5x7_float_out.txt")
    (Array (Matrix (Linalg.rref (
         parse_text_file "./tests/rref/5x7_float_in.txt" |> matrix_to_list))))
    (fun m -> string_of_expr m);
  test "row reduce 25x50 random int matrix" 
    (parse_text_file "./tests/rref/25x50_int_out.txt")
    (Array (Matrix (Linalg.rref (
         parse_text_file "./tests/rref/25x50_int_in.txt" |> matrix_to_list))))
    (fun m -> string_of_expr m);
]

let lin_alg_tests =
  let open Linalg in [
    test "Symmetric matrix" true
      (is_symmetric [[1.;7.;3.];[7.;4.;~-.5.];[3.;~-.5.;6.]])
      string_of_bool;
    test "Multiply two square matrices" 
      (Matrix [[7.;7.;4.];[7.;7.;4.];[12.;9.;5.]])
      (Matrix (matrix_multiply [[1.;2.;1.];[1.;2.;1.];[1.;1.;3.]]
                 [[2.;1.;1.];[1.;2.;1.];[3.;2.;1.]]))
      (fun m -> string_of_expr (Array m));
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
    (* has_var tests *)
    test "has_var x: x" 
      true 
      (Inverse.has_var (Var "x") (Var "x"))
      string_of_bool; 
    test "has_var x: 5" 
      false 
      (Inverse.has_var (Int 5) (Var "x"))
      string_of_bool; 
    test "has_var x: x + 3 = 5" 
      true 
      (Inverse.has_var (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: y + 3 = 5" 
      false 
      (Inverse.has_var (Binop(Eq, Binop(Add, Var "y", Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: 4 + 4 = 5" 
      false 
      (Inverse.has_var (Binop(Eq, Binop(Add, Int 4, Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: 4 - 3 + 4 * x = 5" 
      true 
      (Inverse.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 3), 
                                        Binop(Mul, Int 4, Var "x")), Int 5 ))
         (Var "x"))
      string_of_bool;  
    test "has_var x: 4 - x + 4 * 3 = 5" 
      true 
      (Inverse.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Var "x"), 
                                        Binop(Mul, Int 4, Int 3)), Int 5 ))
         (Var "x"))
      string_of_bool;  
    test "has_var x: 4 - 6 + 4 * 3 = 5" 
      false
      (Inverse.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 6), 
                                        Binop(Mul, Int 4, Int 3)), Int 5 ))
         (Var "x"))
      string_of_bool;  


    (* main inverse tests *)
    test "basic inverse for addition equation x + 4 = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )))
      Ast.string_of_expr;
    test "basic inverse for addition equation 4 + x = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Add, Int 4, Var "x"), Int 5 )))
      Ast.string_of_expr;
    test "basic inverse for addition equation 4 = 5 + x" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 4, Binop(Add, Int 5, Var "x"))))
      Ast.string_of_expr;
    test "inverse for addition equation 4 + x + 7 = 5 + 2" 
      (Binop(Eq, Var "x", Binop (Sub, Binop (Sub, Binop (Add, Int 5, Int 2), 
                                             Int 4), Int 7)))
      (Inverse.inverse ("x") (Binop(Eq, Binop(Add, Int 4, 
                                              Binop(Add, Var "x", Int 7)),
                                    Binop(Add, Int 5, Int 2) )))
      Ast.string_of_expr;
    test "inverse for addition equation 4 + 7 = 5 + x + 2" 
      (Binop (Eq, Var "x", Binop (Sub, Binop (Sub, 
                                              Binop (Add, Int 4, Int 7), 
                                              Int 5), Int 2)))
      (Inverse.inverse ("x") (Binop(Eq, Binop(Add, Int 4, Int 7), 
                                    Binop(Add, Int 5, 
                                          Binop(Add, Var "x", Int 2)) ))) 
      Ast.string_of_expr;
    test "basic inverse for subtraction equation x - 4 = 5" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Sub, Var "x", Int 4), Int 5 )))
      Ast.string_of_expr;
    test "basic inverse for subtraction equation 4 - x = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Sub, Int 4, Var "x"), Int 5 )))
      Ast.string_of_expr;
    test "basic inverse for subtraction equation 5 = 4 - x" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5, Binop(Sub, Int 4, Var "x") )))
      Ast.string_of_expr;
    test "basic inverse for subtraction equation 5 = x - 4" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5, Binop(Sub, Var "x", Int 4) )))
      Ast.string_of_expr;
    test "inverse for subtraction equation 5 = x - 4 + 3" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Binop(Add, Int 4, Int 3)))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5, 
                                    Binop(Sub, Var "x", 
                                          Binop(Add, Int 4, Int 3)) )))
      Ast.string_of_expr;
    test "basic inverse for multiplication equation x * 4 = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 )) )
      Ast.string_of_expr;
    test "basic inverse for multiplication equation 4 * x = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Mul, Int 4, Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "basic inverse for multiplication equation 5 = 4 * x" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5, Binop(Mul, Int 4, Var "x") )) )
      Ast.string_of_expr;
    test "basic inverse for division equation x / 4 = 5" 
      (Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Div, Var "x", Int 4), Int 5 )) )
      Ast.string_of_expr;
    test "basic inverse for division equation 5 = x / 4" 
      (Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5,Binop(Div, Var "x", Int 4) )) )
      Ast.string_of_expr;
    test "basic inverse for division equation 4 / x = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 4, Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Div, Int 4, Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "basic inverse for division equation 5 = 4 / x" 
      (Binop(Eq, Var "x", Binop(Div, Int 4, Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Int 5, Binop(Div, Int 4, Var "x"))) )
      Ast.string_of_expr;
    test "basic inverse for division equation y / x = 5" 
      (Binop(Eq,  Var "x", Binop(Div, Var "y", Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Div, Var "y", Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "solve equation y / x = (5 + z)" 
      (Binop(Eq, Var "x", Binop(Div, Var "y", Binop (Add, Int 5, Var "z")))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Div, Var "y", Var "x"), 
                                    Binop (Add, Int 5, Var "z"))) )
      Ast.string_of_expr;
    test "basic inverse for division equation 4y / x = 5" 
      (Binop(Eq,  Var "x", Binop(Div, Binop(Mul, Int 4, Var "y"), Int 5))) 
      (Inverse.inverse ("x") (Binop(Eq, Binop(Div, Binop(Mul, Int 4, Var "y"),
                                              Var "x"), Int 5 )) )
      Ast.string_of_expr;

  ]

let prob_tests = let open Prob in [
    test "Factorial Base 0" 1 (Prob.factorial 0) string_of_int;
    test "Factorial Base 1" 1 (Prob.factorial 1) string_of_int;
    test "Factorial Rec 5" 120 (Prob.factorial 5) string_of_int;

    test "Choose 0 is 1" 1. (Prob.choose 10 0) string_of_float; 
    test "Choose n is 1" 1. (Prob.choose 10 10) string_of_float; 
    test "Choose 1 is n" 10. (Prob.choose 10 1) string_of_float; 
    test "10 Choose 5" 252. (Prob.choose 10 5) string_of_float; 

    test "Unif p in range" 1. (Prob.uniform_pmf 0. 1. 0.5) string_of_float;
    test "Unif p out of range ge" 0. (Prob.uniform_pmf 0. 1. 2.) string_of_float;
    test "Unif p out of range le" 0.
      (Prob.uniform_pmf 0. 1. (-1.)) string_of_float;
    test "Unif p on range ip" 1. (Prob.uniform_pmf 0. 1. 1.) string_of_float;
    test "Unif p on range low" 1. (Prob.uniform_pmf 0. 1. 0.) string_of_float;
    test "Unif c 0" 0. (Prob.uniform_cdf 0. 1. (-1.)) string_of_float;
    test "Unif c 1" 1. (Prob.uniform_cdf 0. 1. 2.) string_of_float;
    test "Unif c middle" 0.5 (Prob.uniform_cdf 0. 1. 0.5) string_of_float;

    test "Bern p 1" 0.8 (Prob.bernoulli_pmf 0.8 1) string_of_float;
    test "Bern p 0" (1. -. 0.8) (Prob.bernoulli_pmf 0.8 0) string_of_float;
    test "Bern c 0" 0. (Prob.bernoulli_cdf 0.8 (-1)) string_of_float;
    test "Bern c 1" 1. (Prob.bernoulli_cdf 0.8 (1)) string_of_float;

    test "Geo p 1" 0.5 (Prob.geometric_pmf 0.5 1) string_of_float;
    test "Geo p 3" 0.125 (Prob.geometric_pmf 0.5 3) string_of_float;
    test "Geo c 1" 0.8 (Prob.geometric_cdf 0.8 1) string_of_float;
    test "Geo c 1" 0.992 (Prob.geometric_cdf 0.8 3) string_of_float;

    test "Exp p 0" 0.5 (Prob.exponential_pmf 0.5 0.) string_of_float;
    test "Exp p 1" (exp (-1.)) (Prob.exponential_pmf 1. 1.) string_of_float;
    test "Exp c 0" 0. (Prob.exponential_cdf 1. 0.) string_of_float;
    test "Exp c 1" (1. -. exp (-1.))
      (Prob.exponential_cdf 1. 1.) string_of_float;

    test "Pois p 0" (exp (-1.)) (Prob.poisson_pmf 1. 0) string_of_float;
    test "Pois p 2" (exp (-1.) /. 2.) (Prob.poisson_pmf 1. 2) string_of_float;

    test "Pois c 0" (exp (-1.)) (Prob.poisson_cdf 1. 0) string_of_float;
    test "Pois c 2" (5. *. exp (-1.) /. 2.)
      (Prob.poisson_cdf 1. 2) string_of_float;

    test "Binom p 0" (0.5 ** 10.) (Prob.binomial_pmf 10 0.5 0) string_of_float;
    test "Binom p n" (0.5 ** 10.) (Prob.binomial_pmf 10 0.5 10) string_of_float;

    test "Binom c 0" (0.5 ** 10.) (Prob.binomial_cdf 10 0.5 0) string_of_float;
    test "Binom c n" (1.) (Prob.binomial_cdf 10 0.5 10) string_of_float;


  ]

let eval_tests = 
  let eval_expr e sigma = fst (Eval.eval_expr e sigma) in
  [
    test "Var x is float" (VFloat 0.) (eval_expr (Var "x") [("x", VFloat 0.)])
      string_of_value;
    test "Zero int evaluates to itself as a float" (VFloat 0.)
      (eval_expr (Int 0) []) string_of_value;
    test "Postive int evaluates to itself as a float" (VFloat 1.)
      (eval_expr (Int 1) []) string_of_value;
    test "Negative int evaluates to itself as a float" (VFloat ~-.1.)
      (eval_expr (Int ~-1) []) string_of_value;

    test "Zero float evaluates to itself as a float" (VFloat 0.)
      (eval_expr (Float 0.) []) string_of_value;
    test "Postive float evaluates to itself as a float" (VFloat 1.)
      (eval_expr (Float 1.) []) string_of_value;
    test "Negative float evaluates to itself as a float" (VFloat ~-.1.)
      (eval_expr (Float ~-.1.) []) string_of_value;

    test "Add two ints" (VFloat 3.)
      (eval_expr (Binop (Add, Int 1, Int 2)) []) string_of_value;
    test "Add two floats" (VFloat 3.)
      (eval_expr (Binop (Add, Float 1., Float 2.)) []) string_of_value;
    test "Add one int and one float" (VFloat 3.)
      (eval_expr (Binop (Add, Float 1., Int 2)) []) string_of_value;

    test "Subtract two ints" (VFloat 0.)
      (eval_expr (Binop (Sub, Int 1, Int 1)) []) string_of_value;
    test "Subtract two floats" (VFloat 0.)
      (eval_expr (Binop (Sub, Float 1., Float 1.)) []) string_of_value;
    test "Subtract one int and one float" (VFloat 0.)
      (eval_expr (Binop (Sub, Float 1., Int 1)) []) string_of_value;

    test "Multiply two ints" (VFloat 2.)
      (eval_expr (Binop (Mul, Int 1, Int 2)) []) string_of_value;
    test "Multiply two floats" (VFloat 2.)
      (eval_expr (Binop (Mul, Float 1., Float 2.)) []) string_of_value;
    test "Multiply one int and one float" (VFloat 2.)
      (eval_expr (Binop (Mul, Float 1., Int 2)) []) string_of_value;

    test "Divide two ints" (VFloat 0.5)
      (eval_expr (Binop (Div, Int 1, Int 2)) []) string_of_value;
    test "Divide two floats" (VFloat 0.5)
      (eval_expr (Binop (Div, Float 1., Float 2.)) []) string_of_value;
    test "Divide one int and one float" (VFloat 0.5)
      (eval_expr (Binop (Div, Float 1., Int 2)) []) string_of_value;

    test "Power two ints" (VFloat 8.)
      (eval_expr (Binop (Pow, Int 2, Int 3)) []) string_of_value;
    test "Power two floats" (VFloat 8.)
      (eval_expr (Binop (Pow, Float 2., Float 3.)) []) string_of_value;
    test "Power int power and float base" (VFloat 8.)
      (eval_expr (Binop (Pow, Float 2., Int 3)) []) string_of_value;
    test "Power int base and float power" (VFloat 8.)
      (eval_expr (Binop (Pow, Int 2, Float 3.)) []) string_of_value;

    test "modulo no remainder" (VFloat (3 mod 3 |> float_of_int))
      (eval_expr (Binop (Mod, Float 3., Float 3.)) []) string_of_value;
    test "modulo with p > q" (VFloat (4 mod 3 |> float_of_int))
      (eval_expr (Binop (Mod, Float 4., Float 3.)) []) string_of_value;
    test "modulo with p < q" (VFloat (3 mod 4 |> float_of_int))
      (eval_expr (Binop (Mod, Float 3., Float 4.)) []) string_of_value;
    test "modulo with -p" (VFloat (~-3 mod 4 |> float_of_int))
      (eval_expr (Binop (Mod, Float ~-.3., Float 4.)) []) string_of_value;
    test "modulo with -p no remainder" (VFloat 0.)
      (eval_expr (Binop (Mod, Float ~-.3., Float 3.)) []) string_of_value;

    test "Pythagorean theorem c^2" (VFloat 25.)
      (parse "3^2 + 4^2" |> fun inp -> Eval.eval_input inp [] |> fst)
      string_of_value;
    test "Pythagorean theorem c" (VFloat 5.)
      (parse "(3^2 + 4^2)^(1/2)"
       |> fun inp -> Eval.eval_input inp [] |> fst) string_of_value;
    test "Quadratic formula" (VFloat ~-.0.25)
      (parse "(-2 + (3^2 - 4 * 2 * 1)^(1/2)) / (2 * 2)"
       |> fun inp -> Eval.eval_input inp [] |> fst) string_of_value;
    test "PEMDAS test" (VFloat 4.)
      (parse "3 + 3 * 2 / 3 - 1"
       |> fun inp -> Eval.eval_input inp [] |> fst) string_of_value;

    test "Equal two ints" (VFloat (Bool.to_float true))
      (eval_expr (Binop (Eq, Int 2, Int 2)) []) string_of_value;
    test "Equal two floats" (VFloat (Bool.to_float true))
      (eval_expr (Binop (Eq, Float 2., Float 2.)) []) string_of_value;
    test "Equal one int and one float" (VFloat (Bool.to_float true))
      (eval_expr (Binop (Eq, Float 2., Int 2)) []) string_of_value;

    test "Greater than two ints" (VFloat (Bool.to_float true))
      (eval_expr (Binop (GT, Int 3, Int 2)) []) string_of_value;
    test "Greater than two floats" (VFloat (Bool.to_float true))
      (eval_expr (Binop (GT, Float 3., Float 2.)) []) string_of_value;
    test "Greater than one int and one float" (VFloat (Bool.to_float true))
      (eval_expr (Binop (GT, Float 3., Int 2)) []) string_of_value;

    test "Less than two ints" (VFloat (Bool.to_float false))
      (eval_expr (Binop (LT, Int 3, Int 2)) []) string_of_value;
    test "Less than two floats" (VFloat (Bool.to_float false))
      (eval_expr (Binop (LT, Float 3., Float 2.)) []) string_of_value;
    test "Less than one int and one float" (VFloat (Bool.to_float false))
      (eval_expr (Binop (LT, Float 3., Int 2)) []) string_of_value;

    test "Greater than or equal to two ints" (VFloat (Bool.to_float true))
      (eval_expr (Binop (GTE, Int 3, Int 2)) []) string_of_value;
    test "Greater than or equal to two floats" (VFloat (Bool.to_float true))
      (eval_expr (Binop (GTE, Float 3., Float 2.)) []) string_of_value;
    test "Greater than or equal to one int and one float"
      (VFloat (Bool.to_float true)) (eval_expr (Binop (GTE, Float 3., Int 2)) [])
      string_of_value;

    test "Less than or equal to two ints" (VFloat (Bool.to_float false))
      (eval_expr (Binop (LTE, Int 3, Int 2)) []) string_of_value;
    test "Less than or equal to two floats" (VFloat (Bool.to_float false))
      (eval_expr (Binop (LTE, Float 3., Float 2.)) []) string_of_value;
    test "Less than or equal to one int and one float"
      (VFloat (Bool.to_float false))
      (eval_expr (Binop (LTE, Float 3., Int 2)) []) string_of_value;
  ] 

let suite =
  "test suite for OCamulator"  >::: List.flatten [
    parse_tests;
    matrix_tests;
    lin_alg_tests;
    var_present_tests;
    inverse_tests;
    prob_tests;
    eval_tests
  ]

let _ = run_test_tt_main suite