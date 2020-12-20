open OUnit2
open Ast
open Vector
open Stat

(** [string_of_list lst] is a printer funtion for float lists *)
let string_of_list vec = 
  let string_of_list_aux sep vec =
    List.map string_of_float vec 
    |> String.concat sep
    |> (fun str -> "[" ^ str ^ "]") in
  string_of_list_aux "; " vec

(** [string_of_pair p] is a printer funtion for float pairs *)
let string_of_pair p = 
  "(" ^ string_of_float (fst p) ^ ", " ^ string_of_float (snd p) ^ ")"

(** [test name expected_output fn_output print_fn] is an OUnit test case named
    [name] that asserts equality between [expected_output] and [fn_output].
    [print_fn] is used to print the inputs if the assertion is false. *)
let test name expected_output fn_output print_fn =
  name >:: (fun _ -> assert_equal expected_output fn_output ~printer:print_fn)

let test_rand_1 name out sampler arg print_fn =
  Random.init 42;
  name >:: (fun _ -> assert_equal out (sampler arg) ~printer:print_fn)

let test_rand_2 name out sampler arg1 arg2 print_fn =
  Random.init 42;
  name >:: (fun _ -> assert_equal out (sampler arg1 arg2) ~printer:print_fn)

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

(** [read_matrix_from_text_file filename] is a matrix constructed from the 
    contents of the text file given by [filename]. *)
let read_matrix_from_text_file filename =
  let ic = open_in filename in
  let line = input_line ic in
  match parse line with
  | Matrix m -> m
  | _ -> failwith "Impossible"

(** [test_command name expected_output cmd e] is an OUnit test case named [name]
    that asserts equality between [expected_output] and the value that results 
    from evaluating the expression [Command (cmd, e)]. *)
let test_command name expected_output cmd e =
  test name expected_output
    (fst (Eval.eval_expr (Command (cmd, e)) [])) string_of_value

(** [test_binop name expected_output cmd e] is an OUnit test case named [name]
    that asserts equality between [expected_output] and the value that results 
    from evaluating the expression [Binop (op, e1, e2)]. *)
let test_binop name expected_output op e1 e2 =
  test name expected_output
    (fst (Eval.eval_expr (Binop (op, e1, e2)) [])) string_of_value

(** [check_lu_decomp l u] is [unit] if [l] is lower triangular and [u] is
    upper triangular; otherwise, [Failure] is raised. *)
let check_lu_decomp l u =
  let open Matrix in
  assert_bool ("L: " ^ string_of_matrix l) (is_lower_triangular l);
  assert_bool ("U: " ^ string_of_matrix u) (is_upper_triangular u)

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
  parse_test "Int RowVector in R1" (Vector (Vector.make_row_vec [1.])) "[1]";
  parse_test "Int RowVector in R2" (Vector (Vector.make_row_vec [1.; 2.])) "[1, 2]";
  parse_test "Int RowVector in R3" (Vector (Vector.make_row_vec [1.; 2.; 3.])) "[1, 2, 3]";
  parse_test "Float RowVector in R1" (Vector (Vector.make_row_vec [1.])) "[1.0]";
  parse_test "Float RowVector in R2" (Vector (Vector.make_row_vec [1.; 2.])) "[1.0, 2.0]";
  parse_test "Float RowVector in R3"
    (Vector (Vector.make_row_vec  [1.; 2.; 3.])) "[1.0, 2.0, 3.0]";
  parse_test "Mixed type RowVector in R2"
    (Vector (Vector.make_row_vec  [0.5; 2.])) "[0.5, 2]";

  parse_test "Int ColumnVector in R2" (Vector (Vector.make_col_vec [1.; 2.])) "[1; 2]";
  parse_test "Int ColumnVector in R3"
    (Vector (Vector.make_col_vec [1.; 2.; 3.])) "[1; 2; 3]";
  parse_test "Float ColumnVector in R2"
    (Vector (Vector.make_col_vec [1.; 2.])) "[1.0; 2.0]";
  parse_test "Float ColumnVector in R3"
    (Vector (Vector.make_col_vec [1.; 2.; 3.])) "[1.0; 2.0; 3.0]";
  parse_test "Mixed type ColumnVector in R2"
    (Vector (Vector.make_col_vec [0.5; 2.])) "[0.5; 2]";

  (* Matrix tests *)
  parse_test "Int Matrix in R(2x2)"
    (Matrix (Matrix.of_list [[1.;2.]; [3.;4.]])) "[1, 2; 3, 4]";
  parse_test "Int Matrix in R(2x3)"
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]])) "[1, 2, 3; 4, 5, 6]";
  parse_test "Int Matrix in R(3x2)" 
    (Matrix (Matrix.of_list[[1.;2.]; [3.;4.]; [5.;6.]])) "[1, 2; 3, 4; 5, 6]";
  parse_test "Int Matrix in R(2x3)" 
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]))
    "[1, 2, 3; 4, 5, 6; 7, 8, 9]";
  parse_test "Float Matrix in R(2x2)" 
    (Matrix (Matrix.of_list [[1.;2.]; [3.;4.]])) "[1.0, 2.0; 3.0, 4.0]";
  parse_test "Float Matrix in R(2x3)" 
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]])) "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0]";
  parse_test "Float Matrix in R(3x2)" 
    (Matrix (Matrix.of_list [[1.;2.]; [3.;4.]; [5.;6.]])) "[1.0, 2.0; 3.0, 4.0; 5.0, 6.0]";
  parse_test "Float Matrix in R(2x3)" 
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]))
    "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]";
  parse_test "Mixed type Matrix in R(2x2)"
    (Matrix (Matrix.of_list [[0.5;2.]; [1.;1.0]])) "[0.5, 2; 1, 1.0]";
]


let matrix_tests = [
  test_command "row reduce 3x3 matrix with two pivot columns" 
    (VMatrix (Matrix.of_list [[1.;0.;~-.1.];[0.;1.;2.];[0.;0.;0.]]))
    "rref" (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_command "row reduce 3x4 matrix with three pivot columns"
    (VMatrix (Matrix.of_list  [[1.;0.;~-.1.;~-.0.];[0.;1.;2.;0.];[0.;0.;0.;1.]]))
    "rref" (Matrix (Matrix.of_list
                      [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;~-.12.]]));
  test_command "row reduce 3x4 matrix with two pivot columns" 
    (VMatrix (Matrix.of_list [[1.;0.;~-.1.;~-.2.];[0.;1.;2.;3.];[0.;0.;0.;0.]]))
    "rref" (Matrix (Matrix.of_list
                      [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;12.]]));
  test_command "row reduce 4x4 matrix with two pivot columns" 
    (VMatrix (Matrix.of_list [[1.;0.;~-.1.;~-.2.];[0.;1.;2.;3.];
                              [0.;0.;0.;0.];[0.;0.;0.;0.]]))
    "rref" (Matrix (Matrix.of_list [[1.;2.;3.;4.];[5.;6.;7.;8.];
                                    [9.;10.;11.;12.];[13.;14.;15.;16.]]));
  test_command "row reduce 4x5 matrix with four pivot columns" 
    (VMatrix (Matrix.of_list [[1.;0.;~-.3.;0.;0.];[0.;1.;2.;0.;0.];
                              [0.;0.;0.;1.;0.];[0.;0.;0.;0.;1.]]))
    "rref" (Matrix (Matrix.of_list
                      [[0.;~-.3.;~-.6.;4.;9.];[~-.1.;~-.2.;~-.1.;3.;1.];
                       [~-.2.;~-.3.;0.;3.;~-.1.];[1.;4.;5.;~-.9.;~-.9.]]));
  test_command "row reduce 10x10 random int matrix with 10 pivot columns" 
    (VMatrix (read_matrix_from_text_file "./tests/rref/10x10_int_out.txt"))
    "rref" (Matrix
              (read_matrix_from_text_file "./tests/rref/10x10_int_in.txt"));
  test_command "row reduce 5x7 random float matrix with 5 pivot columns" 
    (VMatrix (read_matrix_from_text_file "./tests/rref/5x7_float_out.txt"))
    "rref" (Matrix 
              (read_matrix_from_text_file "./tests/rref/5x7_float_in.txt"));
  test_command "row reduce 25x50 random int matrix"
    (VMatrix (read_matrix_from_text_file "./tests/rref/25x50_int_out.txt"))
    "rref" (Matrix 
              (read_matrix_from_text_file "./tests/rref/25x50_int_in.txt"));
  test_binop "3x1 row vector times 3x3 matrix"
    (VVector (Vector.make_row_vec [30.;36.;42.]))
    Mul (Vector (Vector.make_row_vec [1.;2.;3.]))
    (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_binop "3x3 matrix times 1x3 column vector"
    (VVector (Vector.make_col_vec [14.;32.;50.]))
    Mul (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]))
    (Vector (Vector.make_col_vec [1.;2.;3.]));
]


let lin_alg_tests =
  let open Linalg in [
    test "Symmetric matrix" true
      Matrix.(is_symmetric (of_list [[1.;7.;3.];[7.;4.;~-.5.];[3.;~-.5.;6.]]))
      string_of_bool;
    test_binop "Multiply two square matrices" 
      (VMatrix (Matrix.of_list [[7.;7.;4.];[7.;7.;4.];[12.;9.;5.]]))
      Mul (Matrix (Matrix.of_list [[1.;2.;1.];[1.;2.;1.];[1.;1.;3.]]))
      (Matrix (Matrix.of_list [[2.;1.;1.];[1.;2.;1.];[3.;2.;1.]]));
    test "PLU decomposition of 3x3 matrix with a zero in a non-pivot position"
      (Matrix.of_list [[1.;0.;2.];[3.;4.;5.];[6.;7.;8.]])
      (let (p, l, u, _) = 
         plu_decomposition (Matrix.of_list [[1.;0.;2.];[3.;4.;5.];[6.;7.;8.]]) in
       check_lu_decomp l u;
       Matrix.(matrix_multiply (transpose p) (matrix_multiply l u)))
      (Matrix.string_of_matrix);
    test "PLU decomposition of 3x3 matrix with no zeros"
      (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])
      (let (p, l, u, _) = 
         plu_decomposition (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]) in
       check_lu_decomp l u;
       Matrix.(matrix_multiply (transpose p) (matrix_multiply l u)))
      (Matrix.string_of_matrix);
    test "PLU decomposition of 3x3 matrix with all zeros"
      (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]])
      (let (p, l, u, _) = 
         plu_decomposition (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]]) in
       check_lu_decomp l u;
       Matrix.(matrix_multiply (transpose p) (matrix_multiply l u)))
      (Matrix.string_of_matrix);
    test_command "Determinant of 4x4 matrix" (VFloat ~-.6.) "det"
      (Matrix (Matrix.of_list
                 [[2.;4.;1.;1.];[2.;1.;3.;4.];[2.;1.;2.;3.];[4.;2.;1.;2.]]));
    test_command "Determinant of 3x3 zeros matrix" (VFloat ~-.0.) "det"
      (Matrix (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]]));
    test_command "Determinant of 3x3 non-zero matrix" (VFloat ~-.0.) "det"
      (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
    test_command "Determinant of 5x5 random float matrix" (VFloat ~-.0.0293)
      "det" (Matrix (read_matrix_from_text_file "./tests/det/5x5_float_in.txt"));
    test_command "Inverse of 4x4 int matrix" 
      (VMatrix (Matrix.of_list [[~-.0.7857;0.3929;0.1071;0.4643];
                                [1.1429;~-.1.0714;0.0714;~-.0.3571];
                                [~-.0.5000;0.7500;~-.0.2500;0.2500];
                                [0.5714;~-.0.2857; 0.2857;~-.0.4286]])) "inv"
      (Matrix (Matrix.of_list [[2.;3.;4.;2.];[1.;1.;3.;2.];
                               [3.;1.;1.;3.];[4.;4.;4.;1.]]));
    test_command "Inverse of 3x3 float matrix"
      (VMatrix (Matrix.of_list
                  ([[0.75;0.5;0.25];[0.5;1.;0.5 ];[0.25;0.5;0.75]]))) "inv"
      (Matrix (Matrix.of_list [[2.;~-.1.;0.];[~-.1.;2.;~-.1.];[0.;~-.1.;2.]]));
  ]

let solve_tests = let open Solve in [
    (* has_var tests *)
    test "has_var x: x" 
      true 
      (Solve.has_var (Var "x") (Var "x"))
      string_of_bool; 
    test "has_var x: 5" 
      false 
      (Solve.has_var (Int 5) (Var "x"))
      string_of_bool; 
    test "has_var x: x + 3 = 5" 
      true 
      (Solve.has_var (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: y + 3 = 5" 
      false 
      (Solve.has_var (Binop(Eq, Binop(Add, Var "y", Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: 4 + 4 = 5" 
      false 
      (Solve.has_var (Binop(Eq, Binop(Add, Int 4, Int 4), Int 5 ))
         (Var "x"))
      string_of_bool; 
    test "has_var x: 4 - 3 + 4 * x = 5" 
      true 
      (Solve.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 3), 
                                      Binop(Mul, Int 4, Var "x")), Int 5 ))
         (Var "x"))
      string_of_bool;  
    test "has_var x: 4 - x + 4 * 3 = 5" 
      true 
      (Solve.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Var "x"), 
                                      Binop(Mul, Int 4, Int 3)), Int 5 ))
         (Var "x"))
      string_of_bool;  
    test "has_var x: 4 - 6 + 4 * 3 = 5" 
      false
      (Solve.has_var (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 6), 
                                      Binop(Mul, Int 4, Int 3)), Int 5 ))
         (Var "x"))
      string_of_bool;  


    (* main solve tests *)
    test "solve for var x = 5" 
      (Binop(Eq, Var "x", Int 5 ))
      (Solve.solve ("x") (Binop(Eq, Var "x", Int 5 )))
      Ast.string_of_expr;
    test "solve for var 5 = x" 
      (Binop(Eq, Int 5, Var "x"))
      (Solve.solve ("x") (Binop(Eq, Int 5, Var "x")))
      Ast.string_of_expr;
    test "basic solve for addition equation x + 4 = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )))
      Ast.string_of_expr;
    test "basic solve for addition equation 4 + x = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Add, Int 4, Var "x"), Int 5 )))
      Ast.string_of_expr;
    test "basic solve for addition equation 4 = 5 + x" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Int 4, Binop(Add, Int 5, Var "x"))))
      Ast.string_of_expr;
    test "solve for addition equation 4 + x + 7 = 5 + 2" 
      (Binop(Eq, Var "x", Binop (Sub, Binop (Sub, Binop (Add, Int 5, Int 2), 
                                             Int 4), Int 7)))
      (Solve.solve ("x") (Binop(Eq, Binop(Add, Int 4, 
                                          Binop(Add, Var "x", Int 7)),
                                Binop(Add, Int 5, Int 2) )))
      Ast.string_of_expr;
    test "solve for addition equation 4 + 7 = 5 + x + 2" 
      (Binop (Eq, Var "x", Binop (Sub, Binop (Sub, 
                                              Binop (Add, Int 4, Int 7), 
                                              Int 5), Int 2)))
      (Solve.solve ("x") (Binop(Eq, Binop(Add, Int 4, Int 7), 
                                Binop(Add, Int 5, 
                                      Binop(Add, Var "x", Int 2)) ))) 
      Ast.string_of_expr;
    test "basic solve for subtraction equation x - 4 = 5" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Sub, Var "x", Int 4), Int 5 )))
      Ast.string_of_expr;
    test "basic solve for subtraction equation 4 - x = 5" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Sub, Int 4, Var "x"), Int 5 )))
      Ast.string_of_expr;
    test "basic solve for subtraction equation 5 = 4 - x" 
      (Binop(Eq, Var "x", Binop(Sub, Int 4, Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Int 5, Binop(Sub, Int 4, Var "x") )))
      Ast.string_of_expr;
    test "basic solve for subtraction equation 5 = x - 4" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Int 5, Binop(Sub, Var "x", Int 4) )))
      Ast.string_of_expr;
    test "solve for subtraction equation 5 = x - 4 + 3" 
      (Binop(Eq, Var "x", Binop(Add, Int 5, Binop(Add, Int 4, Int 3)))) 
      (Solve.solve ("x") (Binop(Eq, Int 5, 
                                Binop(Sub, Var "x", 
                                      Binop(Add, Int 4, Int 3)) )))
      Ast.string_of_expr;
    test "basic solve for multiplication equation x * 4 = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 )) )
      Ast.string_of_expr;
    test "basic solve for multiplication equation 4 * x = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Mul, Int 4, Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "basic solve for multiplication equation 5 = 4 * x" 
      (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Int 5, Binop(Mul, Int 4, Var "x") )) )
      Ast.string_of_expr;
    test "basic solve for division equation x / 4 = 5" 
      (Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Var "x", Int 4), Int 5 )) )
      Ast.string_of_expr;
    test "basic solve for division equation 5 = x / 4" 
      (Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))) 
      (Solve.solve ("x") (Binop(Eq, Int 5,Binop(Div, Var "x", Int 4) )) )
      Ast.string_of_expr;
    test "basic solve for division equation 4 / x = 5" 
      (Binop(Eq, Var "x", Binop(Div, Int 4, Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Int 4, Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "basic solve for division equation 5 = 4 / x" 
      (Binop(Eq, Var "x", Binop(Div, Int 4, Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Int 5, Binop(Div, Int 4, Var "x"))) )
      Ast.string_of_expr;
    test "basic solve for division equation y / x = 5" 
      (Binop(Eq,  Var "x", Binop(Div, Var "y", Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Var "y", Var "x"), Int 5 )) )
      Ast.string_of_expr;
    test "solve equation y / x = (5 + z)" 
      (Binop(Eq, Var "x", Binop(Div, Var "y", Binop (Add, Int 5, Var "z")))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Var "y", Var "x"), 
                                Binop (Add, Int 5, Var "z"))) )
      Ast.string_of_expr;
    test "basic solve for division equation 4y / x = 5" 
      (Binop(Eq,  Var "x", Binop(Div, Binop(Mul, Int 4, Var "y"), Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Binop(Mul, Int 4, Var "y"),
                                          Var "x"), Int 5 )) )
      Ast.string_of_expr;


    (* GCD tests *)
    test "gcd of 0 and 0 is 0" 
      (0) (Solve.gcd 0 0) string_of_int;
    test "gcd of 42 and 0 is 43" 
      (42) (Solve.gcd 42 0) string_of_int;
    test "gcd of 0 and 42 is 42" 
      (42) (Solve.gcd 0 42) string_of_int;
    test "gcd of 45 and 365 is 5" 
      (5) (Solve.gcd 45 365) string_of_int;
    test "gcd of 365 and 45 is 5" 
      (5) (Solve.gcd 365 45) string_of_int;
    test "gcd of 6 and 642 is 5" 
      (6) (Solve.gcd 6 642) string_of_int;
    test "gcd of 1490 and 350 is 10" 
      (10) (Solve.gcd 1490 350) string_of_int;

    (* LCM tests *)
    test "lcm of 12 and 15 is 60" 
      (60) (Solve.lcm 12 15) string_of_int;
    test "lcm of 15 and 12 is 60" 
      (60) (Solve.lcm 15 12) string_of_int;
    test "lcm of 1491 and 250 is 372750" 
      (372750) (Solve.lcm 1491 250) string_of_int;
    test "lcm of 41352 and 25 is 1033800" 
      (1033800) (Solve.lcm 41352 25) string_of_int;
  ]

let prob_tests = let open Prob in [
    test "Factorial Base 0" 1 (factorial 0) string_of_int;
    test "Factorial Base 1" 1 (factorial 1) string_of_int;
    test "Factorial Rec 5" 120 (factorial 5) string_of_int;

    test "Choose 0 is 1" 1. (choose 10 0) string_of_float; 
    test "Choose n is 1" 1. (choose 10 10) string_of_float; 
    test "Choose 1 is n" 10. (choose 10 1) string_of_float; 
    test "10 Choose 5" 252. (choose 10 5) string_of_float;

    test "Perm 0 is 1" 1. (perm 10 0) string_of_float; 
    test "Perm n n" 6. (perm 3 3) string_of_float; 
    test "Perm 1 is n" 3. (perm 3 1) string_of_float; 
    test "4 Perm 2" 12. (perm 4 2) string_of_float; 

    test "Unif p in range" 1. (uniform_pmf 0. 1. 0.5) string_of_float;
    test "Unif p out of range ge" 0. (uniform_pmf 0. 1. 2.) string_of_float;
    test "Unif p out of range le" 0.
      (uniform_pmf 0. 1. (-1.)) string_of_float;
    test "Unif p on range ip" 1. (uniform_pmf 0. 1. 1.) string_of_float;
    test "Unif p on range low" 1. (uniform_pmf 0. 1. 0.) string_of_float;
    test "Unif c 0" 0. (uniform_cdf 0. 1. (-1.)) string_of_float;
    test "Unif c 1" 1. (uniform_cdf 0. 1. 2.) string_of_float;
    test "Unif c middle" 0.5 (uniform_cdf 0. 1. 0.5) string_of_float;

    test "Bern p 1" 0.8 (bernoulli_pmf 0.8 1) string_of_float;
    test "Bern p 0" (1. -. 0.8) (bernoulli_pmf 0.8 0) string_of_float;
    test "Bern c 0" 0. (bernoulli_cdf 0.8 (-1)) string_of_float;
    test "Bern c mid" (1. -. 0.8) (bernoulli_cdf 0.8 (0)) string_of_float;
    test "Bern c 1" 1. (bernoulli_cdf 0.8 (1)) string_of_float;

    test "Geo p 1" 0.5 (geometric_pmf 0.5 1) string_of_float;
    test "Geo p 3" 0.125 (geometric_pmf 0.5 3) string_of_float;
    test "Geo c 1" 0.8 (geometric_cdf 0.8 1) string_of_float;
    test "Geo c 1" 0.992 (geometric_cdf 0.8 3) string_of_float;

    test "Exp p 0" 0.5 (exponential_pmf 0.5 0.) string_of_float;
    test "Exp p 1" (exp (-1.)) (exponential_pmf 1. 1.) string_of_float;
    test "Exp c 0" 0. (exponential_cdf 1. 0.) string_of_float;
    test "Exp c 1" (1. -. exp (-1.))
      (exponential_cdf 1. 1.) string_of_float;

    test "Pois p 0" (exp (-1.)) (poisson_pmf 1. 0) string_of_float;
    test "Pois p 2" (exp (-1.) /. 2.) (poisson_pmf 1. 2) string_of_float;

    test "Pois c 0" (exp (-1.)) (poisson_cdf 1. 0) string_of_float;
    test "Pois c 2" (5. *. exp (-1.) /. 2.)
      (poisson_cdf 1. 2) string_of_float;

    test "Binom p 0" (0.5 ** 10.) (binomial_pmf 10 0.5 0) string_of_float;
    test "Binom p n" (0.5 ** 10.) (binomial_pmf 10 0.5 10) string_of_float;

    test "Binom c 0" (0.5 ** 10.) (binomial_cdf 10 0.5 0) string_of_float;
    test "Binom c n" (1.) (binomial_cdf 10 0.5 10) string_of_float;

    (** All random tests use the seed 42*)
    test_rand_1 "Bern sam 1" 0. bernoulli_sam 0.5 string_of_float;
    test_rand_1 "Bern sam 0" 1. bernoulli_sam 0.9 string_of_float;

    test_rand_1 "Geo sam 0.5" 2. geometric_sam 0.5 string_of_float;
    test_rand_1 "Geo sam 0.8" 1. geometric_sam 0.8 string_of_float;
    test_rand_1 "Geo sam 0.1" 4. geometric_sam 0.1 string_of_float;

    test_rand_2 "Bin sam 10 .5" 3. binomial_sam 10 0.5 string_of_float;
    test_rand_2 "Bin sam 10 .8" 7. binomial_sam 10 0.8 string_of_float;

    test_rand_2 "Pois sam 1 5" 0. poisson_sam 1. 0.5 string_of_float;
    test_rand_2 "Pois sam 1 5" 3. poisson_sam 1. 5.0 string_of_float;
    test_rand_2 "Pois sam 1 20" 25. poisson_sam 1. 20.0 string_of_float;

    parse_test "parse bern sam n" (Prob (Bernoulli (SAM,0.5,10.))) 
      "bern smpl 0.5 10";
    parse_test "parse bern sam 0" (Prob (Bernoulli (SAM,0.5,0.))) 
      "bern smpl 0.5";
    parse_test "parse bern pdf" (Prob (Bernoulli (PDF,0.5,2.))) 
      "bern pdf 0.5 2.";
    parse_test "parse bern cdf" (Prob (Bernoulli (CDF,0.5,2.))) 
      "bern cdf 0.5 2.";

    parse_test "parse binom sam n" (Prob (Binomial (SAM,5.,0.5,10.))) 
      "binom smpl 5 0.5 10";
    parse_test "parse binom sam 0" (Prob (Binomial (SAM,5.,0.5,0.))) 
      "binom smpl 5 0.5";
    parse_test "parse binom pdf" (Prob (Binomial (PDF,5.,0.5,2.))) 
      "binom pdf 5 0.5 2.";
    parse_test "parse binom cdf" (Prob (Binomial (CDF,5.,0.5,2.))) 
      "binom cdf 5 0.5 2.";

    parse_test "parse unif sam n" (Prob (Uniform (SAM,0.,1.,10.))) 
      "unif smpl 0. 1. 10";
    parse_test "parse unif sam 0" (Prob (Uniform  (SAM,0.,1.,0.))) 
      "unif smpl 0. 1.";
    parse_test "parse unif pdf" (Prob (Uniform  (PDF,0.,1.,0.5))) 
      "unif pdf 0. 1. 0.5";
    parse_test "parse unif cdf" (Prob (Uniform  (CDF,0.,1.,0.5))) 
      "unif cdf 0. 1. 0.5";

    parse_test "parse geo sam n" (Prob (Geometric (SAM,0.5,10.))) 
      "geo smpl 0.5 10";
    parse_test "parse geo sam 0" (Prob (Geometric  (SAM,0.5,0.))) 
      "geo smpl 0.5";
    parse_test "parse geo pdf" (Prob (Geometric  (PDF,0.5,3.))) 
      "geo pdf 0.5 3";
    parse_test "parse geo cdf" (Prob (Geometric (CDF,0.5,3.))) 
      "geo cdf 0.5 3";

    parse_test "parse exp sam n" (Prob (Exponential (SAM,0.5,10.))) 
      "exp smpl 0.5 10";
    parse_test "parse exp sam 0" (Prob (Exponential (SAM,0.5,0.))) 
      "exp smpl 0.5";
    parse_test "parse exp pdf" (Prob (Exponential  (PDF,0.5,3.))) 
      "exp pdf 0.5 3";
    parse_test "parse exp cdf" (Prob (Exponential (CDF,0.5,3.))) 
      "exp cdf 0.5 3";

    parse_test "parse pois sam n" (Prob (Poisson (SAM,0.5,10.))) 
      "pois smpl 0.5 10";
    parse_test "parse pois sam 0" (Prob (Poisson (SAM,0.5,0.))) 
      "pois smpl 0.5";
    parse_test "parse pois pdf" (Prob (Poisson (PDF,0.5,3.))) 
      "pois pdf 0.5 3";
    parse_test "parse pois cdf" (Prob (Poisson (CDF,0.5,3.))) 
      "pois cdf 0.5 3";

  ]

let stat_tests = let open Stat in
  [
    test "sort asc empty" [] (sort_asc []) string_of_list;
    test "sort asc 1" [1.] (sort_asc [1.]) string_of_list;
    test "sort asc nothing" [1.;2.;3.] (sort_asc [1.;2.;3.]) string_of_list;
    test "sort asc" [1.;2.;3.] (sort_asc [3.;2.;1.]) string_of_list;

    test "sort desc empty" [] (sort_desc[]) string_of_list;
    test "sort desc 1" [1.] (sort_desc [1.]) string_of_list;
    test "sort desc" [3.;2.;1.] (sort_desc [1.;2.;3.]) string_of_list;
    test "sort desc nothing" [3.;2.;1.] (sort_desc [3.;2.;1.]) string_of_list;

    test "sum empty" 0. (cum_sum []) string_of_float;
    test "sum 1" 1. (cum_sum [1.]) string_of_float;
    test "sum many" 6. (cum_sum [1.;2.;3.]) string_of_float;
    test "sum neg" 2. (cum_sum [1.;-2.;3.]) string_of_float;

    test "prod empty" 1. (cum_prod []) string_of_float;
    test "prod 1" 1. (cum_prod [1.]) string_of_float;
    test "prod many" 6. (cum_prod [1.;2.;3.]) string_of_float;
    test "prod neg" (-6.) (cum_prod [1.;-2.;3.]) string_of_float;

    test "mean empty" 0. (mean []) string_of_float;
    test "mean 1" 1. (mean [1.]) string_of_float;
    test "mean same" 2. (mean [2.;2.;2.;2.]) string_of_float;
    test "mean neg" 0. (mean [-1.;1.;-1.;1.]) string_of_float;

    test "median empty" 0. (median []) string_of_float;
    test "median 1" 1. (median [1.]) string_of_float;
    test "median same" 2. (median [2.;2.;2.;2.]) string_of_float;
    test "median even" 0. (median [-1.;1.;-1.;1.]) string_of_float;
    test "median odd" 2. (median [1.;2.;3.]) string_of_float;

    test "mode empty" 0. (mode []) string_of_float;
    test "mode 1" 1. (mode [1.]) string_of_float;
    test "mode tie" 2. (mode [2.;2.;3.;3.]) string_of_float;
    test "mode many" 1. (mode [1.;1.;1.;2.;2.]) string_of_float;
    test "mode rev" 2. (mode [1.;2.;2.;2.;2.]) string_of_float;

    test "max empty" 0. (max []) string_of_float;
    test "max empty" 1. (max [1.]) string_of_float;
    test "max many" 3. (max [1.;2.;3.]) string_of_float;
    test "max neg" 2. (max [1.;2.;-3.]) string_of_float;

    test "min empty" 0. (min []) string_of_float;
    test "min empty" 1. (min [1.]) string_of_float;
    test "min many" 1. (min [1.;2.;3.]) string_of_float;
    test "min neg" (-3.) (min [1.;2.;-3.]) string_of_float;

    test "range empty" 0. (range []) string_of_float;
    test "range none" 0. (range [1.;1.]) string_of_float;
    test "range pos" 7. (range [8.;1.]) string_of_float;

    test "var empty" 0. (smpl_var []) string_of_float;
    test "var same" 0. (smpl_var [1.;1.;1.]) string_of_float;
    test "var many" 2.5 (smpl_var [1.;2.;3.;4.;5.]) string_of_float;

    test "std empty" 0. (smpl_std []) string_of_float;
    test "std same" 0. (smpl_std [1.;1.;1.]) string_of_float;
    test "std many" (2.5 ** 0.5) (smpl_std [1.;2.;3.;4.;5.]) string_of_float;

    test "count empty" 0. (count 0. []) string_of_float;
    test "count 1" 1. (count 1. [1.]) string_of_float;
    test "count only" 3. (count 1. [1.;1.;1.]) string_of_float;
    test "count many" 3. (count 1. [1.;1.;1.;2.;2.]) string_of_float;

    test "unique empty" [] (unique []) string_of_list;
    test "unique 1" [1.] (unique [1.]) string_of_list;
    test "unique many" [1.;2.] (unique [1.;1.;1.;2.;2.]) string_of_list;

    test "quantile empty" 0. (quantile [] 0.5) string_of_float;
    test "quantile single" 1. (quantile [1.] 0.5) string_of_float;
    test "quantile 0.5" 4. (quantile [1.;2.;3.;4.;5.] 0.5) string_of_float;
    test "quantile 0.4" 3. (quantile [1.;2.;3.;4.;5.] 0.4) string_of_float;
    test "quantile 0.75" 5. (quantile [1.;2.;3.;4.;5.] 0.75) string_of_float;
    test "quantile 0.2" 2. (quantile [1.;2.;3.;4.;5.] 0.2) string_of_float;
    test "quantile 0.25" 2. (quantile [1.;2.;3.;4.;5.] 0.25) string_of_float;

    test "linreg 1 0" (2.,0.) 
      (linear_regression [(1.,2.);(2.,4.);(3.,6.)]) string_of_pair;
    test "linreg 1 0" (1.,3.) 
      (linear_regression [(1.,4.);(2.,5.);(3.,6.)]) string_of_pair;
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
    lin_alg_tests;
    solve_tests;
    prob_tests;
    stat_tests;
    eval_tests;
    matrix_tests;
  ]

let _ = run_test_tt_main suite