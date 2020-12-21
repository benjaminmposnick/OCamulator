(** TEST PLAN:
    All of the following Compilation units are tested automatically via OUnit.
    Projection was the only functionality that was tested manually which is
    a utility found in Eval.

    Eval: The Eval compilation unit is essentially a wrapper that applies
    functions from the other compilation units. Because of this, much of the
    testing for eval was done through testing the other files; instead of
    directly calling the functions in the other files, we would call them via
    the eval functions that handle their cases when the actual program is being
    used. For the functions in eval that were not directly tested via testing
    for other files (for example, trig functions, which were evaluated directly
    in Eval), we used black box testing to compare expected outputs against the
    outputs given  by function calls. Finally, we used Bisect to ensure that an
    appropriate amount of the compilation unit was being tested. We did choose
    to exclude Bisect coverage of specific functions that could not be directly
    tested, as they require user input.

    Linalg: The [Linalg] module was tested using both glass box and black box
    testing. Glass boxing testing was done using Bisect as an aid to ensure
    nearly 100% code coverage for this module. Black box testing was done by
    comparing the results of various computations (e.g. row reduction, inverse
    determinants, solve systems of equations) to the results outputted by 
    Matlab. By doing this, we were also able to ensure that such functions
    could be accurately expressed to the same number of digits as Matlab --
    something that helped us greatly to find and reduce roundoff errors.
    Property-based testing was employed to test the PLU decomposition, which 
    ensured that for a matrix A that is factorized, (1) A = (P^T)LU, (2)
    L is lower triangular, and (3) U is triangular. Furthermore, when choosing
    matrices for tests, we aimed to find matrices that were square and 
    non-square, singular and non-singular, full rank and deficient rank,
    large and small. The "./tests" folder contains larger matrices which are
    read into this test suite to ensure efficient computation even for massive
    inputs.

    Solve: The main [solve] function was tested via black box testing comparing 
    known hand-solved outputs of equations with the function outputs. The
    functions that check for variables in equations, [has_var] and [has_var_any]
    were similarly tested via black box testing on equations with and without
    variables nested at various levels in the ast representation. LCM and GCD
    were tested with black box testing to compare known LCMs and GCDs, solved
    by hand and verified with online calculators. Finally, all of the functions
    in the compilation unit were checked with Bisect to ensure that all branches
    of the functions were accounted for in testing.

    Matrix: Matrix was tested using both black box and glass box testing, with
    the latter being done using Bisect as an aid. Much of the module's 
    functionality is utilized by the [Linalg] module, which allowed for "free"
    black box testing -- as the [Linalg] module is concerned only with the
    output. A [rep_ok] function was developed to ensure that no matter what,
    the representation invariants always hold.

    Vector:

    Prob: Probalistic functions expect sampling were tested via black box 
    testing comparing true distribution values to the function outputs. 
    Random variable generators were tested also with balck box testing, but due 
    to the nature of the functions property based testing was done. For each
    distribution the mean and variance of the random vector was compared to
    the true values for 1000 samples to ensure a tight confidence interval.
    All the tests use a set random seed to ensure consistency and repeatability.

    Stat: All statistical functions were tested via black box testing comparing
    the expected output of statisitical functions with their expected output on
    known input vector with the given parameters.

    Demonstrates correctness because ...
*)

open OUnit2
open Ast
open Vector
open Stat

(* ===========================================================================
    UTILITY FUNCTIONS
   ===========================================================================*)

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
  | _ -> failwith "Impossible" [@coverage off]

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

(** [failure_test name expected_output fn_output print_fn] is an OUnit test
    case named [name] that asserts that [expected_output] is raised when [fn]
    is called. *)
let failure_test name expected_output fn =
  name >:: fun _ -> assert_raises (Failure expected_output) (fun () -> fn ())

(** [eval_error_test name expected_output fn] is an OUnit test
    case named [name] that asserts that an Eval.ComputationError.EvalError
    with string [expected_output] when [fn] is run*)
let eval_error_test name expected_output fn =
  name >:: fun _ -> assert_raises
      (Eval.ComputationError.EvalError expected_output) (fun () -> fn ())

(** [test_prob name expected_output dist] is an OUnit test case named [name]
    that asserts that the resulting value after evaluating [dist] is equal to
    [expected_output]
*)
let test_prob name expected_output dist = 
  test name expected_output
    (fst (Eval.eval_expr (Prob dist) [])) string_of_value

(** [test_smpl_min name max dist ] is an OUnit test case is an OUnit test case
    named [name] that asserts that the the random variable generated by evaluting 
    [dist] is greater than [min]*)
let test_smpl_min name min dist =
  Random.init 42;
  let out = fst (Eval.eval_expr (Prob dist) []) in
  let value =
    match out with
    | VFloat f -> f 
    | _ -> failwith "Not even a float" [@coverage off]
  in 
  name >:: (fun _ -> assert_equal true (value >= min)  ~printer:string_of_bool)

(** [test_smpl_max name max dist ] is an OUnit test case is an OUnit test case
    named [name] that asserts that the the random variable generated by 
    evaluting [dist] is less than [max]*)
let test_smpl_max name max dist =
  Random.init 42;
  let out = fst (Eval.eval_expr (Prob dist) []) in
  let value =
    match out with
    | VFloat f -> f 
    | _ -> failwith "Not even a float" [@coverage off]
  in 
  name >:: (fun _ -> assert_equal true (value <= max)  ~printer:string_of_bool)

(** [test_smpl_size name k dist] is an OUnit test case named [name]
    that asserts that the size of the vectors outputed by [dist] being 
    evaluated is correct*)
let test_smpl_size name k dist =
  Random.init 42;
  let out = fst (Eval.eval_expr (Prob dist) []) in
  let value =
    match out with
    | VVector vec -> to_list vec
    | _ -> failwith "Not even a vec" [@coverage off]
  in 
  name >:: (fun _ -> assert_equal k (List.length value)  ~printer:string_of_int)

(** [test_rand name dist metric expected_output dif] is an OUnit test case 
    named [name] that asserts that the difference between a given [metric] 
    in [dist] and [expected_output] is less than dif on the random seed 42 *)
let test_rand name dist metric expected_output dif =
  Random.init 42;
  let out = fst (Eval.eval_expr (Prob dist) []) in
  let vec =
    match out with
    | VVector vec -> Vector vec
    | _ -> failwith "Not even a vec"
  in
  let point_est = fst (Eval.eval_expr (Command (metric, vec)) []) in
  let f =
    match point_est with
    | VFloat f -> f
    | _ -> failwith "Not even a float"
  in
  name >:: (fun _ -> assert_equal true (Float.abs (f -. expected_output) < dif)  
               ~printer:string_of_bool)


(** [check_lu_decomp l u] is [unit] if [l] is lower triangular and [u] is
    upper triangular; otherwise, [Failure] is raised. *)
let check_lu_decomp l u =
  let open Matrix in
  assert_bool ("L: " ^ string_of_matrix l) (is_lower_triangular l);
  assert_bool ("U: " ^ string_of_matrix u) (is_upper_triangular u)

(* ===========================================================================
   TESTS
   ===========================================================================*)

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
    (Binop (LT, Binop(Add, Var "x", Var "y"), Binop(Add, Var "a", Var "b")))
    "x + y < a + b";
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
  parse_test "Int RowVector in R2"
    (Vector (Vector.make_row_vec [1.; 2.])) "[1, 2]";
  parse_test "Int RowVector in R3"
    (Vector (Vector.make_row_vec [1.; 2.; 3.])) "[1, 2, 3]";
  parse_test "Float RowVector in R1" 
    (Vector (Vector.make_row_vec [1.])) "[1.0]";
  parse_test "Float RowVector in R2" 
    (Vector (Vector.make_row_vec [1.; 2.])) "[1.0, 2.0]";
  parse_test "Float RowVector in R3"
    (Vector (Vector.make_row_vec  [1.; 2.; 3.])) "[1.0, 2.0, 3.0]";
  parse_test "Mixed type RowVector in R2"
    (Vector (Vector.make_row_vec  [0.5; 2.])) "[0.5, 2]";
  parse_test "Int ColVector in R2"
    (Vector (Vector.make_col_vec [1.; 2.])) "[1; 2]";
  parse_test "Int ColVector in R3"
    (Vector (Vector.make_col_vec [1.; 2.; 3.])) "[1; 2; 3]";
  parse_test "Float ColVector in R2"
    (Vector (Vector.make_col_vec [1.; 2.])) "[1.0; 2.0]";
  parse_test "Float ColVector in R3"
    (Vector (Vector.make_col_vec [1.; 2.; 3.])) "[1.0; 2.0; 3.0]";
  parse_test "Mixed type ColVector in R2"
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
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]]))
    "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0]";
  parse_test "Float Matrix in R(3x2)" 
    (Matrix (Matrix.of_list [[1.;2.]; [3.;4.]; [5.;6.]]))
    "[1.0, 2.0; 3.0, 4.0; 5.0, 6.0]";
  parse_test "Float Matrix in R(2x3)" 
    (Matrix (Matrix.of_list [[1.;2.;3.]; [4.;5.;6.]; [7.;8.;9.]]))
    "[1.0, 2.0, 3.0; 4.0, 5.0, 6.0; 7.0, 8.0, 9.0]";
  parse_test "Mixed type Matrix in R(2x2)"
    (Matrix (Matrix.of_list [[0.5;2.]; [1.;1.0]])) "[0.5, 2; 1, 1.0]";
]

let linalg_matrix_vector_tests = [
  (* Row reduction and pivot column tests *)
  test_command "row reduce 3x3 matrix with two pivot columns" 
    (VMatrix (Matrix.of_list [[1.;0.;~-.1.];[0.;1.;2.];[0.;0.;0.]]))
    "rref" (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_command "row reduce 3x4 matrix with three pivot columns"
    (VMatrix (Matrix.of_list [[1.;0.;~-.1.;~-.0.];[0.;1.;2.;0.];[0.;0.;0.;1.]]))
    "rref" (Matrix (Matrix.of_list
                      [[1.;2.;3.;4.];[5.;6.;7.;8.];[9.;10.;11.;~-.12.]]));
  test_command "3x3 matrix with two pivot columns" 
    (VList [VFloat 0.; VFloat 1.])
    "pivots" (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_command "3x4 matrix with three pivot columns"
    (VList [VFloat 0.; VFloat 1.; VFloat 3.])
    "pivots" (Matrix (Matrix.of_list
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

  (* Vector and matrix multiplication *)
  test_binop "3x1 row vector times 3x3 matrix"
    (VVector (Vector.make_row_vec [30.;36.;42.]))
    Mul (Vector (Vector.make_row_vec [1.;2.;3.]))
    (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_binop "3x3 matrix times 1x3 column vector"
    (VVector (Vector.make_col_vec [14.;32.;50.]))
    Mul (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]))
    (Vector (Vector.make_col_vec [1.;2.;3.]));
  failure_test "Multiply column vector times matrix"
    "Shape error: first argument should be a row vector"
    (fun () -> Matrix.matrix_vector_product 
        (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])
        (Vector.make_col_vec [1.;2.;3.]) true);
  failure_test "Multiply matrix times row vector"
    "Shape error: second argument should be a column vector"
    (fun () -> Matrix.matrix_vector_product 
        (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])
        (Vector.make_row_vec [1.;2.;3.]) false);

  (* Vector and matrix arithmetic tests *)
  test_binop "add two row vectors"
    (VVector (Vector.make_row_vec [2.;4.;6.]))
    Add (Vector (Vector.make_row_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "add two column vectors"
    (VVector (Vector.make_row_vec [2.;4.;6.]))
    Add (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "add a row and column vector"
    (VVector (Vector.make_row_vec [2.;4.;6.]))
    Add (Vector (Vector.make_row_vec [1.;2.;3.]))
    (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "add a row and column vector"
    (VVector (Vector.make_row_vec [2.;4.;6.]))
    Add (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "subtract two vectors"
    (VVector (Vector.make_row_vec [0.;0.;0.]))
    Sub (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "multiply two vectors"
    (VVector (Vector.make_row_vec [1.;4.;9.]))
    Mul (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "dot two vectors"
    (VFloat 14.)
    Dot (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "divide two vectors"
    (VVector (Vector.make_row_vec [1.;1.;1.]))
    Div (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "exponentiate two vectors"
    (VVector (Vector.make_row_vec [1.;4.;27.]))
    Pow (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "two row vectors are equal" (VFloat 1.)
    Eq (Vector (Vector.make_row_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "two column vectors are equal" (VFloat 1.)
    Eq (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "two vectors with same contents but different orientations"
    (VFloat 0.)
    Eq (Vector (Vector.make_col_vec [1.;2.;3.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  test_binop "two vectors not equal" (VFloat 0.)
    Eq (Vector (Vector.make_col_vec [3.;2.;1.]))
    (Vector (Vector.make_row_vec [1.;2.;3.]));
  failure_test "Cannot apply binop to two vectors of different lengths"
    "Vectors must be the same length"
    (fun () -> Vector.dot_product (Vector.make_col_vec [1.;2.;3.]) 
        (Vector.make_col_vec [1.;2.]));
  test_binop "Add two matrices"
    (VMatrix (Matrix.of_list [[2.;4.;6.];[2.;4.;6.];[2.;4.;6.]]))
    Add (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))
    (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]));
  test_binop "Subtract two matrices"
    (VMatrix (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]]))
    Sub (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))
    (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]));
  test_binop "Two matrices are equal"
    (VFloat 1.0) Eq (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))
    (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]));
  test_binop "Two matrices are not equal"
    (VFloat 0.0) Eq (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))
    (Matrix (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]]));

  (* Vector and Matrix module tests *)
  test "Apply map to column vector" (ColVector [0.;0.;0.])
    (Vector.(map (fun x -> x -. x) (ColVector [1.;2.;3.]))) string_of_vector;
  test "3x4 zeros matrix"
    (Matrix.of_list [[0.;0.;0.;0.;];[0.;0.;0.;0.;];[0.;0.;0.;0.;]]) 
    (Matrix.zeros ~n:3 4) Matrix.string_of_matrix;
  test "drop last row" (Matrix.of_list [[1.;2.;3.];[4.;5.;6.]])
    (Matrix.drop_row (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]) 2) 
    Matrix.string_of_matrix;
  test "drop non-existent row" (Matrix.of_list [[1.;2.;3.];[4.;5.;6.]])
    (Matrix.drop_row (Matrix.of_list [[1.;2.;3.];[4.;5.;6.]]) 3) 
    Matrix.string_of_matrix;
  failure_test "Cannot create an empty matrix"
    "Cannot create empty matrix"
    (fun () -> Matrix.of_vectors []);
  test "Create matrix out of row vectors"
    (Matrix.of_list [[1.;2.;3.];[4.;5.;6.]])
    (Matrix.of_vectors
       [Vector.make_row_vec [1.;2.;3.]; Vector.make_row_vec [4.;5.;6.]]) 
    Matrix.string_of_matrix;
  test "Create matrix out of column vectors"
    (Matrix.of_list [[1.;2.;3.];[4.;5.;6.]])
    (Matrix.of_vectors
       [Vector.make_col_vec [1.;4.]; Vector.make_col_vec [2.;5.]; 
        Vector.make_row_vec [3.;6.]]) 
    Matrix.string_of_matrix;
  test "Map on a matrix" (Matrix.of_list [[1.;4.;9.];[1.;4.;9.]])
    (Matrix.map (fun vec -> Vector.map (fun x -> x ** 2.) vec)
       (Matrix.of_list [[1.;2.;3.];[1.;2.;3.]]))
    Matrix.string_of_matrix;
  test "Map2 with two matrices" (Matrix.of_list [[1.;4.;9.];[1.;4.;9.]])
    (Matrix.map2 (fun vec1 vec2 -> Vector.component_wise_multiply vec1 vec2)
       (Matrix.of_list [[1.;2.;3.];[1.;2.;3.]])
       (Matrix.of_list [[1.;2.;3.];[1.;2.;3.]]))
    Matrix.string_of_matrix;
  test "Not upper triangular" false
    (Matrix.(is_upper_triangular (of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])))
    string_of_bool;
  test "Not lower triangular" false
    (Matrix.(is_lower_triangular (of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])))
    string_of_bool;
  test_command "Transpose a column vector"
    (VVector (Vector.RowVector [1.;2.;3.])) "transpose"
    (Vector (Vector.ColVector [1.;2.;3.]));
  test_command "Transpose a matrix"
    (VMatrix (Matrix.of_list [[1.;4.;7.];[2.;5.;8.];[3.;6.;9.]])) "transpose"
    (Matrix (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]));
  test_binop "Multiply vector by 2" (VVector (Vector.make_col_vec [2.;4.;6.]))
    Mul (Float 2.0) (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "Square a vector" (VVector (Vector.make_col_vec [1.;4.;9.]))
    Pow (Float 2.0) (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "Multiply matrix by 2" 
    (VMatrix (Matrix.of_list [[2.;4.;6.];[2.;4.;6.];[2.;4.;6.]]))
    Mul (Float 2.0) (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]));
  test_binop "Square a matrix" 
    (VMatrix (Matrix.of_list [[1.;4.;9.];[1.;4.;9.];[1.;4.;9.]]))
    Pow (Float 2.0) (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]));

  eval_error_test "Solve system with two matrices"
    "Right arg to \\ must be a vector"
    (fun () -> Eval.eval_expr 
        (Binop (SolveSys,
                (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]])),
                (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))))
        []);
  eval_error_test "Divide two matrices"
    "Invalid operation between two matrices: Div"
    (fun () -> Eval.eval_expr 
        (Binop (Div,
                (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]])),
                (Matrix (Matrix.of_list [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))))
        []);
  eval_error_test "Add a float to a matrix" 
    "Invalid operation between scalar and matrix: Add"
    (fun () -> Eval.eval_expr 
        (Binop (Add, (Float 2.0),
                (Matrix (Matrix.of_list 
                           [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]])))) []);
  eval_error_test "Add a float to a vector" 
    "Invalid operation between scalar and vector: Add"
    (fun () -> Eval.eval_expr 
        (Binop (Add, (Float 2.0),
                (Vector (Vector.make_col_vec [1.;2.;3.])))) []);
  eval_error_test "Add a matrix to a vector" 
    "Invalid operation between matrix and vector: Add"
    (fun () -> Eval.eval_expr 
        (Binop (Add, (Matrix (Matrix.of_list 
                                [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]])),
                (Vector (Vector.make_col_vec [1.;2.;3.])))) []);
  eval_error_test "Solve system with matrix and row vector" 
    "Shape error: second argument to \\ should be a column vector"
    (fun () -> Eval.eval_expr 
        (Binop (SolveSys, (Matrix (Matrix.of_list 
                                     [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]])),
                (Vector (Vector.make_row_vec [1.;2.;3.])))) []);
  eval_error_test "Row reduce a float" 
    "Cannot row reduce a non-matrix"
    (fun () -> Eval.eval_expr (Command ("rref", (Float 2.0))) []);
  eval_error_test "Transpose a float" 
    "Cannot transpose a non-matrix / non-vector"
    (fun () -> Eval.eval_expr (Command ("transpose", (Float 2.0))) []);
  eval_error_test "Determinant of a float" 
    "Cannot calculate determinant of a non-matrix"
    (fun () -> Eval.eval_expr (Command ("det", (Float 2.0))) []);
  eval_error_test "Pivot columns of a float" 
    "Cannot calculate pivots of a non-matrix"
    (fun () -> Eval.eval_expr (Command ("pivots", (Float 2.0))) []);
  eval_error_test "Inverse of a float" 
    "Cannot calculate inverse of a non-matrix"
    (fun () -> Eval.eval_expr (Command ("inv", (Float 2.0))) []);
  eval_error_test "QR factorization of a matrix (which isn't supported)" 
    "No such command: qr"
    (fun () -> Eval.eval_expr
        (Command ("qr", Matrix (Matrix.of_list 
                                  [[1.;2.;3.];[1.;2.;3.];[1.;2.;3.]]))) []);

  test "Symmetric matrix" true
    Matrix.(is_symmetric (of_list [[1.;7.;3.];[7.;4.;~-.5.];[3.;~-.5.;6.]]))
    string_of_bool;
  test_binop "Multiply two square matrices" 
    (VMatrix (Matrix.of_list [[7.;7.;4.];[7.;7.;4.];[12.;9.;5.]]))
    Mul (Matrix (Matrix.of_list [[1.;2.;1.];[1.;2.;1.];[1.;1.;3.]]))
    (Matrix (Matrix.of_list [[2.;1.;1.];[1.;2.;1.];[3.;2.;1.]]));
  test "PLU decomposition of 3x3 matrix with a zero in a non-pivot position"
    (Matrix.of_list [[1.;0.;2.];[3.;4.;5.];[6.;7.;8.]])
    (let (p, l, u, _) = Linalg.plu_decomposition
         (Matrix.of_list [[1.;0.;2.];[3.;4.;5.];[6.;7.;8.]])
     in
     check_lu_decomp l u;
     Matrix.(matrix_multiply (transpose p) (matrix_multiply l u)))
    (Matrix.string_of_matrix);
  test "PLU decomposition of 3x3 matrix with no zeros"
    (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])
    (let (p, l, u, _) = Linalg.plu_decomposition
         (Matrix.of_list [[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]])
     in
     check_lu_decomp l u;
     Matrix.(matrix_multiply (transpose p) (matrix_multiply l u)))
    (Matrix.string_of_matrix);
  test "PLU decomposition of 3x3 matrix with all zeros"
    (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]])
    (let (p, l, u, _) = Linalg.plu_decomposition
         (Matrix.of_list [[0.;0.;0.];[0.;0.;0.];[0.;0.;0.]])
     in
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
  test_binop "Solve Ax=b for 3x3 int matrix A"
    (VVector (Vector.make_col_vec [0.;0.;1.])) SolveSys
    (Matrix (Matrix.of_list ([[3.;3.;1.];[1.;2.;2.];[3.;3.;3.]]))) 
    (Vector (Vector.make_col_vec [1.;2.;3.]));
  test_binop "Solve Ax=b for 3x3 int matrix A with b vector of zeros"
    (VVector (Vector.make_col_vec [0.;0.;0.])) SolveSys
    (Matrix (Matrix.of_list ([[3.;2.;1.];[1.;3.;2.];[2.;3.;2.]]))) 
    (Vector (Vector.make_col_vec [0.;0.;0.]));
  failure_test "Solve non-singular system" 
    "Matrix is not singular -- cannot solve numerically"
    (fun () -> Eval.eval_expr 
        (Binop (SolveSys,
                (Matrix (Matrix.of_list ([[1.;2.;3.];[4.;5.;6.];[7.;8.;9.]]))),
                (Vector (Vector.make_col_vec [1.;2.;3.])))) []);
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

    (* has_var_any tests *)
    test "has_var_any: x" 
      true (Solve.has_var_any (Var "x")) string_of_bool; 
    test "has_var_any: 5" 
      false (Solve.has_var_any (Int 5)) string_of_bool; 
    test "has_var_any: x + 3 = 5" 
      true 
      (Solve.has_var_any (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )))
      string_of_bool; 
    test "has_var_any: y + 3 = 5" 
      true
      (Solve.has_var_any (Binop(Eq, Binop(Add, Var "y", Int 4), Int 5 )))
      string_of_bool; 
    test "has_var_any: 4 + 4 = 5" 
      false 
      (Solve.has_var_any (Binop(Eq, Binop(Add, Int 4, Int 4), Int 5 )))
      string_of_bool; 
    test "has_var_any: 4 - 3 + 4 * x = 5" 
      true 
      (Solve.has_var_any (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 3), 
                                          Binop(Mul, Int 4, Var "x")), Int 5 )))
      string_of_bool;  
    test "has_var_any: 4 - x + 4 * 3 = 5" 
      true 
      (Solve.has_var_any (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Var "x"), 
                                          Binop(Mul, Int 4, Int 3)), Int 5 )))
      string_of_bool;  
    test "has_var_any: 4 - 6 + 4 * 3 = 5" 
      false
      (Solve.has_var_any (Binop(Eq, Binop(Add, Binop(Sub, Int 4, Int 6), 
                                          Binop(Mul, Int 4, Int 3)), Int 5 )))
      string_of_bool;  

    (* solve function tests *)
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
                                Binop (Add, Int 5, Var "z"))))
      Ast.string_of_expr;
    test "basic solve for division equation 4y / x = 5" 
      (Binop(Eq,  Var "x", Binop(Div, Binop(Mul, Int 4, Var "y"), Int 5))) 
      (Solve.solve ("x") (Binop(Eq, Binop(Div, Binop(Mul, Int 4, Var "y"),
                                          Var "x"), Int 5 )) )
      Ast.string_of_expr;
    "No var given Failure" >:: 
    (fun _ -> assert_raises (Failure "No variable given") 
        (fun () -> Solve.solve ("x") (Binop(Eq, Int 3, Int 9)) ));        
    "Wrong var given Failure" >:: 
    (fun _ -> assert_raises (Failure "No variable given") 
        (fun () -> Solve.solve ("x") (Binop(Eq, Binop(Add, Int 3, Var "y"), 
                                            Int 9)) ));

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
    "Trying to get LCM with zero" >:: 
    (fun _ -> assert_raises (Failure "LCM of zero does not exist") 
        (fun () -> Solve.lcm 0 25));
    "Trying to get LCM with zero other argument" >:: 
    (fun _ -> assert_raises (Failure "LCM of zero does not exist") 
        (fun () -> Solve.lcm 25 0));

    (* Root tests *)
    (* test "root of x^2 + x + 1 = 0" 
       () (Solve.lcm 12 15) string_of_int; *)
  ]

let prob_tests = let open Prob in [
    test "Factorial Base 0" 1 (factorial 0) string_of_int;
    test "Factorial Base 1" 1 (factorial 1) string_of_int;
    test "Factorial Rec 5" 120 (factorial 5) string_of_int;

    test_command "cmd fac" (VFloat 6.)
      "fac" (Float 3.);

    test "Choose 0 is 1" 1. (choose 10 0) string_of_float; 
    test "Choose n is 1" 1. (choose 10 10) string_of_float; 
    test "Choose 1 is n" 10. (choose 10 1) string_of_float; 
    test "10 Choose 5" 252. (choose 10 5) string_of_float;

    test_command "cmd choose" (VFloat 252.)
      "choose" (Tuple (Float 10., Float 5.));
    test_command "cmd comb" (VFloat 252.)
      "comb" (Tuple (Float 10., Float 5.));

    test "Perm 0 is 1" 1. (perm 10 0) string_of_float; 
    test "Perm n n" 6. (perm 3 3) string_of_float; 
    test "Perm 1 is n" 3. (perm 3 1) string_of_float; 
    test "4 Perm 2" 12. (perm 4 2) string_of_float; 

    test_command "cmd perm" (VFloat 12.)
      "perm" (Tuple (Float 4., Float 2.));

    test "Unif p in range" 1. (uniform_pmf 0. 1. 0.5) string_of_float;
    test "Unif p out of range ge" 0. (uniform_pmf 0. 1. 2.) string_of_float;
    test "Unif p out of range le" 0.
      (uniform_pmf 0. 1. (-1.)) string_of_float;
    test "Unif p on range ip" 1. (uniform_pmf 0. 1. 1.) string_of_float;
    test "Unif p on range low" 1. (uniform_pmf 0. 1. 0.) string_of_float;
    test "Unif c 0" 0. (uniform_cdf 0. 1. (-1.)) string_of_float;
    test "Unif c 1" 1. (uniform_cdf 0. 1. 2.) string_of_float;
    test "Unif c middle" 0.5 (uniform_cdf 0. 1. 0.5) string_of_float;

    test_prob "Unif pdf eval" (VFloat 1.) (Uniform (PDF,0.,1.,0.5));
    test_prob "Unif cdf eval" (VFloat 1.) (Uniform (CDF,0.,1.,2.));
    test_smpl_min "Unif smpl eval min" 0. (Uniform (SAM,0.,1.,0.));
    test_smpl_max "Unif smpl eval max" 1. (Uniform (SAM,0.,1.,0.));
    test_smpl_size "Unif smpl eval size" 10 (Uniform (SAM,0.,1.,10.));

    test_rand "Unif 1000 mu" (Uniform (SAM,0.,1.,1000.)) "mean" 0.5 0.1;
    test_rand "Unif 1000 v" (Uniform (SAM,0.,1.,1000.)) "variance" 0.0833333 0.1;

    eval_error_test "Unif a>b" "a must be < b" 
      (fun () -> Eval.eval_expr (Prob (Uniform (PDF,5.,0.,0.5))) []);
    eval_error_test "k exp" "x must be int for sampling"
      (fun () -> Eval.eval_expr (Prob (Uniform (SAM,1.,3.,0.5))) []);
    eval_error_test "k neg exp" "Need positive k for sampling"
      (fun () -> Eval.eval_expr (Prob (Uniform (SAM,1.,3.,-1.))) []);

    test "Bern p 1" 0.8 (bernoulli_pmf 0.8 1) string_of_float;
    test "Bern p 0" (1. -. 0.8) (bernoulli_pmf 0.8 0) string_of_float;
    test "Bern c 0" 0. (bernoulli_cdf 0.8 (-1)) string_of_float;
    test "Bern c mid" (1. -. 0.8) (bernoulli_cdf 0.8 (0)) string_of_float;
    test "Bern c 1" 1. (bernoulli_cdf 0.8 (1)) string_of_float;

    test_prob "bern pdf eval" (VFloat 0.5) (Bernoulli (PDF,0.5,0.));
    test_prob "bern cdf eval" (VFloat 1.) (Bernoulli(CDF,0.5,1.));
    test_smpl_min "bern smpl eval min" 0. (Bernoulli (SAM,0.5,0.));
    test_smpl_max "bern smpl eval max" 1. (Bernoulli(SAM,0.5,0.));
    test_smpl_size "bern smpl eval size" 10 (Bernoulli(SAM,0.5,10.));

    test_rand "Bern 1000 mu" (Bernoulli (SAM,0.5,1000.)) "mean" 0.5 0.1;
    test_rand "Bern 1000 v" (Bernoulli (SAM,0.5,1000.)) "variance" 0.25 0.1;

    eval_error_test "bern p exc" "p must be between 0 and 1 inclusive"
      (fun () -> Eval.eval_expr (Prob (Bernoulli (PDF,4.,0.))) []);
    eval_error_test "bern k exc" 
      "k value of Bernoulli distribution must be an integer"
      (fun () -> Eval.eval_expr (Prob (Bernoulli (SAM,0.5,0.5))) []);
    eval_error_test "bern 0-1 exc" 
      "Bernoulli RV's can only be 0 or 1"
      (fun () -> Eval.eval_expr (Prob (Bernoulli (PDF,0.5,2.))) []);

    test "Geo p 1" 0.5 (geometric_pmf 0.5 1) string_of_float;
    test "Geo p 3" 0.125 (geometric_pmf 0.5 3) string_of_float;
    test "Geo c 1" 0.8 (geometric_cdf 0.8 1) string_of_float;
    test "Geo c 1" 0.992 (geometric_cdf 0.8 3) string_of_float;

    test_prob "geo pdf eval" (VFloat 0.25) (Geometric (PDF,0.5,2.));
    test_prob "geo cdf eval" (VFloat 0.992) (Geometric (CDF,0.8,3.));
    test_smpl_min "geo smpl eval min" 0. (Geometric (SAM,0.5,0.));
    test_smpl_size "geo smpl eval size" 10 (Geometric (SAM,0.5,10.));

    test_rand "Geo 1000 mu " (Geometric (SAM,0.5,1000.)) "mean" 2. 0.1;
    test_rand "Geo 1000 v" (Geometric (SAM,0.5,1000.)) "variance" 2. 0.1;

    eval_error_test "geo p exc" "p must be between 0 and 1 inclusive"
      (fun () -> Eval.eval_expr (Prob (Geometric (PDF,4.,0.))) []);
    eval_error_test "geo k float exc" 
      "k value of Geometric distribution must be an integer"
      (fun () -> Eval.eval_expr (Prob (Geometric(PDF,0.5,0.5))) []);
    eval_error_test "geo k neg exc" 
      "Input value must be >= 0"
      (fun () -> Eval.eval_expr (Prob (Geometric (PDF,0.5,-2.))) []);

    test "Exp p 0" 0.5 (exponential_pmf 0.5 0.) string_of_float;
    test "Exp p 1" (exp (-1.)) (exponential_pmf 1. 1.) string_of_float;
    test "Exp c 0" 0. (exponential_cdf 1. 0.) string_of_float;
    test "Exp c 1" (1. -. exp (-1.))
      (exponential_cdf 1. 1.) string_of_float;

    test_prob "exp pdf eval" (VFloat 0.5) (Exponential (PDF,0.5,0.));
    test_prob "exp cdf eval" (VFloat 0.) (Exponential (CDF,1.,0.));
    test_smpl_min "exp smpl eval min" 0. (Exponential (SAM,0.5,0.));
    test_smpl_size "exp smpl eval max" 10 (Exponential (SAM,0.5,10.));

    test_rand "exp 1000 mu " (Exponential (SAM,0.5,1000.)) "mean" 2. 0.1;
    test_rand "exp 1000 v" (Exponential (SAM,0.5,1000.)) "variance" 4. 0.1;

    eval_error_test "exp l exc" "Lambda must be > 0"
      (fun () -> Eval.eval_expr (Prob (Exponential (PDF,-1.,1.))) []);
    eval_error_test "exp x neg exc" "Input value must be >= 0"
      (fun () -> Eval.eval_expr (Prob (Exponential (PDF,1.,-1.))) []);
    eval_error_test "exp x float exc" "x must be int for sampling"
      (fun () -> Eval.eval_expr (Prob (Exponential (SAM,1.,1.4))) []);

    test "Pois p 0" (exp (-1.)) (poisson_pmf 1. 0) string_of_float;
    test "Pois p 2" (exp (-1.) /. 2.) (poisson_pmf 1. 2) string_of_float;
    test "Pois c 0" (exp (-1.)) (poisson_cdf 1. 0) string_of_float;
    test "Pois c 2" (5. *. exp (-1.) /. 2.)
      (poisson_cdf 1. 2) string_of_float;

    test_prob "pois pdf eval" (VFloat (exp (-1.))) (Poisson (PDF,1.,0.));
    test_prob "pois cdf eval" (VFloat (exp (-1.))) (Poisson (CDF,1.,0.));
    test_smpl_min "exp smpl eval min" 0. (Poisson (SAM,0.5,0.));
    test_smpl_size "exp smpl eval max" 10 (Poisson (SAM,0.5,10.));

    test_rand "pois 1000 mu " (Poisson (SAM,0.5,1000.)) "mean" 0.5 0.1;
    test_rand "pois 1000 v" (Poisson (SAM,0.5,1000.)) "variance" 0.5 0.1;

    eval_error_test "pois l exc" "Lambda must be > 0"
      (fun () -> Eval.eval_expr (Prob (Poisson (PDF,-1.,1.))) []);
    eval_error_test "pois x neg exc" "Input value must be >= 0"
      (fun () -> Eval.eval_expr (Prob (Poisson (PDF,1.,-1.))) []);
    eval_error_test "pois x float sam exc" "x must be int for sampling"
      (fun () -> Eval.eval_expr (Prob (Poisson (SAM,1.,1.4))) []);
    eval_error_test "pois x float dist exc" 
      "x value of Poisson distribution must be an integer"
      (fun () -> Eval.eval_expr (Prob (Poisson (PDF,1.,1.4))) []);

    test "Binom p 0" (0.5 ** 10.) (binomial_pmf 10 0.5 0) string_of_float;
    test "Binom p n" (0.5 ** 10.) (binomial_pmf 10 0.5 10) string_of_float;
    test "Binom c 0" (0.5 ** 10.) (binomial_cdf 10 0.5 0) string_of_float;
    test "Binom c n" (1.) (binomial_cdf 10 0.5 10) string_of_float;

    test_prob "binom pdf eval" (VFloat (0.5 ** 10.)) 
      (Binomial (PDF,10.,0.5,0.));
    test_prob "binom cdf eval" (VFloat (0.5 ** 10.)) 
      (Binomial (CDF,10.,0.5,0.));
    test_smpl_min "binom smpl eval min" 0. (Binomial (SAM,5.,0.5,0.));
    test_smpl_max "binom smpl eval max" 5. (Binomial (SAM,5.,0.5,0.));
    test_smpl_size "binom smpl eval size" 10 (Binomial (SAM,5.,0.5,10.));

    test_rand "binom 1000 mu " (Binomial (SAM,5.,0.5,1000.)) "mean" 2.5 0.1;
    test_rand "binom 1000 v" (Binomial (SAM,5.,0.5,1000.)) "variance" 1.25 0.1;

    eval_error_test "binom p exc" "p must be between 0 and 1 inclusive"
      (fun () -> Eval.eval_expr (Prob (Binomial (PDF,4.,8.,2.))) []);
    eval_error_test "binom k neg exc" 
      "Input value must be >= 0"
      (fun () -> Eval.eval_expr (Prob (Binomial (PDF,4.,0.5,-2.))) []);
    eval_error_test "binom n k int exc" 
      "n and k values of Binomial distribution must be ints"
      (fun () -> Eval.eval_expr (Prob (Binomial (PDF,3.5,0.5,2.))) []);
    eval_error_test "binom k > n exc" 
      "k must be <= n"
      (fun () -> Eval.eval_expr (Prob (Binomial (PDF,3.,0.5,5.))) []);

    eval_error_test "Solve system on floats"
      "Left arg to \\ must be a matrix, right arg must be a vector"
      (fun () -> Eval.eval_expr (Binop (SolveSys, Float 0., Float 1.)) []);
    eval_error_test "Assign float to a float"
      "Invalid operation between two floats: Assign"
      (fun () -> Eval.eval_expr (Binop (Assign, Float 0., Float 1.)) []);
    eval_error_test "Dot two floats"
      "Invalid operation between two floats: Dot"
      (fun () -> Eval.eval_expr (Binop (Dot, Float 0., Float 1.)) []);
    eval_error_test "Solve system on vectors"
      "Left arg to \\ must be a matrix"
      (fun () -> Eval.eval_expr
          (Binop (SolveSys,
                  Vector (Vector.make_col_vec [1.;2.;3.]),
                  Vector (Vector.make_col_vec [1.;2.;3.]))) []);
    eval_error_test "Assign vectors to a vectors"
      "Invalid operation between two vectors: Assign"
      (fun () -> Eval.eval_expr
          (Binop (Assign,
                  Vector (Vector.make_col_vec [1.;2.;3.]),
                  Vector (Vector.make_col_vec [1.;2.;3.]))) []);
    eval_error_test "Mod two vectors"
      "Invalid operation between two vectors: Mod"
      (fun () -> Eval.eval_expr
          (Binop (Mod,
                  Vector (Vector.make_col_vec [1.;2.;3.]),
                  Vector (Vector.make_col_vec [1.;2.;3.]))) []);

    test "norm p" (exp ( 0.) /. ((acos (-1.) *. 2.) ** (0.5))) 
      (normal_pmf 0. 1. 0.) string_of_float;
    test "norm c" (0.5) (normal_cdf 0. 1. 0.) string_of_float;

    test_prob "normal pdf eval" 
      (VFloat (exp ( 0.) /. ((acos (-1.) *. 2.) ** (0.5)))) 
      (Normal (PDF,0.,1.,0.));
    test_prob "normal cdf eval" (VFloat (0.5)) 
      (Normal (CDF,0.,1.,0.));
    test_smpl_min "normal smpl eval min" (-1000.) (Normal (SAM,100.,0.1,0.));
    test_smpl_max "normal smpl eval max" (2000.) (Normal (SAM,100.,0.1,0.));
    test_smpl_size "normal smpl eval size"  10 (Normal (SAM,100.,0.1,10.));

    test_rand "norm std 1000 mu" (Normal (SAM,0.,1.,1000.)) "mean" 0. 0.1;
    test_rand "norm std 1000 v" (Normal (SAM,0.,1.,1000.)) "variance" 1. 0.1;
    test_rand "norm dif 1000 mu" (Normal (SAM,9.,5.,1000.)) "mean" 9. 1.;
    test_rand "norm dif 1000 v" (Normal (SAM,9.,5.,1000.)) "variance" 25. 2.;

    eval_error_test "norm s neg exc" "Input value must be >= 0"
      (fun () -> Eval.eval_expr (Prob (Normal (PDF,1.,-1.,2.))) []);
    eval_error_test "norm sam float exc" "x must be int for sampling"
      (fun () -> Eval.eval_expr (Prob (Normal (SAM,1.,1.,2.5))) []);
    eval_error_test "norm sam neg neg exc" "Need positive k for sampling"
      (fun () -> Eval.eval_expr (Prob (Normal (SAM,1.,1.,-2.))) []);

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

    test_command "cmd sort_asc empty" (VVector (make_row_vec [])) "sort_asc" 
      (Vector (make_row_vec []));
    test_command "cmd sort_asc 1" (VVector (make_row_vec [1.])) "sort_asc" 
      (Vector (make_row_vec [1.]));
    test_command "cmd sort_asc n" (VVector (make_row_vec [1.;2.;3.])) "sort_asc" 
      (Vector (make_row_vec [1.;2.;3.]));
    test_command "cmd sort_asc" (VVector (make_row_vec [1.;2.;3.])) "sort_asc" 
      (Vector (make_row_vec [1.;2.;3.]));

    test "sort desc empty" [] (sort_desc[]) string_of_list;
    test "sort desc 1" [1.] (sort_desc [1.]) string_of_list;
    test "sort desc" [3.;2.;1.] (sort_desc [1.;2.;3.]) string_of_list;
    test "sort desc nothing" [3.;2.;1.] (sort_desc [3.;2.;1.]) string_of_list;

    test_command "cmd sort_desc empty" (VVector (make_row_vec [])) "sort_desc" 
      (Vector (make_row_vec []));
    test_command "cmd sort_desc 1" (VVector (make_row_vec [1.])) "sort_desc" 
      (Vector (make_row_vec [1.]));
    test_command "cmd sort_desc n" (VVector (make_row_vec [3.;2.;1.])) 
      "sort_desc" (Vector (make_row_vec [1.;2.;3.]));
    test_command "cmd sort_desc" (VVector (make_row_vec [3.;2.;1.])) 
      "sort_desc" (Vector (make_row_vec [3.;2.;1.]));

    test "sum empty" 0. (cum_sum []) string_of_float;
    test "sum 1" 1. (cum_sum [1.]) string_of_float;
    test "sum many" 6. (cum_sum [1.;2.;3.]) string_of_float;
    test "sum neg" 2. (cum_sum [1.;-2.;3.]) string_of_float;

    test_command "cmd sum" (VFloat 6.)
      "sum" (Vector (make_row_vec [3.;2.;1.]));

    test "prod empty" 1. (cum_prod []) string_of_float;
    test "prod 1" 1. (cum_prod [1.]) string_of_float;
    test "prod many" 6. (cum_prod [1.;2.;3.]) string_of_float;
    test "prod neg" (-6.) (cum_prod [1.;-2.;3.]) string_of_float;

    test_command "cmd prod" (VFloat 6.)
      "product" (Vector (make_row_vec [3.;2.;1.]));

    test "mean empty" 0. (mean []) string_of_float;
    test "mean 1" 1. (mean [1.]) string_of_float;
    test "mean same" 2. (mean [2.;2.;2.;2.]) string_of_float;
    test "mean neg" 0. (mean [-1.;1.;-1.;1.]) string_of_float;

    test_command "cmd mean" (VFloat 2.)
      "mean" (Vector (make_row_vec [3.;2.;1.]));

    test "median empty" 0. (median []) string_of_float;
    test "median 1" 1. (median [1.]) string_of_float;
    test "median same" 2. (median [2.;2.;2.;2.]) string_of_float;
    test "median even" 0. (median [-1.;1.;-1.;1.]) string_of_float;
    test "median odd" 2. (median [1.;2.;3.]) string_of_float;

    test_command "cmd median" (VFloat 2.)
      "median" (Vector (make_row_vec [3.;2.;1.]));

    test "mode empty" 0. (mode []) string_of_float;
    test "mode 1" 1. (mode [1.]) string_of_float;
    test "mode tie" 2. (mode [2.;2.;3.;3.]) string_of_float;
    test "mode many" 1. (mode [1.;1.;1.;2.;2.]) string_of_float;
    test "mode rev" 2. (mode [1.;2.;2.;2.;2.]) string_of_float;

    test_command "cmd mode" (VFloat 2.)
      "mode" (Vector (make_row_vec [1.;2.;2.;2.;2.]));

    test "max empty" 0. (max []) string_of_float;
    test "max empty" 1. (max [1.]) string_of_float;
    test "max many" 3. (max [1.;2.;3.]) string_of_float;
    test "max neg" 2. (max [1.;2.;-3.]) string_of_float;

    test_command "cmd max" (VFloat 3.)
      "max" (Vector (make_row_vec [3.;2.;1.]));

    test "min empty" 0. (min []) string_of_float;
    test "min empty" 1. (min [1.]) string_of_float;
    test "min many" 1. (min [1.;2.;3.]) string_of_float;
    test "min neg" (-3.) (min [1.;2.;-3.]) string_of_float;

    test_command "cmd min" (VFloat 1.)
      "min" (Vector (make_row_vec [3.;2.;1.]));

    test "range empty" 0. (range []) string_of_float;
    test "range none" 0. (range [1.;1.]) string_of_float;
    test "range pos" 7. (range [8.;1.]) string_of_float;

    test_command "cmd range" (VFloat 2.)
      "range" (Vector (make_row_vec [3.;2.;1.]));

    test "var empty" 0. (smpl_var []) string_of_float;
    test "var same" 0. (smpl_var [1.;1.;1.]) string_of_float;
    test "var many" 2.5 (smpl_var [1.;2.;3.;4.;5.]) string_of_float;

    test_command "cmd var" (VFloat 2.5)
      "variance" (Vector (make_row_vec [1.;2.;3.;4.;5.]));

    test "std empty" 0. (smpl_std []) string_of_float;
    test "std same" 0. (smpl_std [1.;1.;1.]) string_of_float;
    test "std many" (2.5 ** 0.5) (smpl_std [1.;2.;3.;4.;5.]) string_of_float;

    test_command "cmd std" (VFloat 0.)
      "std" (Vector (make_row_vec [1.;1.;1.]));

    test "count empty" 0. (count 0. []) string_of_float;
    test "count 1" 1. (count 1. [1.]) string_of_float;
    test "count only" 3. (count 1. [1.;1.;1.]) string_of_float;
    test "count many" 3. (count 1. [1.;1.;1.;2.;2.]) string_of_float;

    test_command "cmd count" (VFloat 2.) "count" 
      (Tuple (Float 3., Vector (make_row_vec [1.;2.;3.;3.])));

    test "rms 1" 1. (rms [1.;1.;1.;1.]) string_of_float;

    test_command "cmsd rms" (VFloat 1.) "rms" 
      (Vector (make_row_vec [1.;1.;1.;1.]));

    test "unique empty" [] (unique []) string_of_list;
    test "unique 1" [1.] (unique [1.]) string_of_list;
    test "unique many" [1.;2.] (unique [1.;1.;1.;2.;2.]) string_of_list;

    test_command "cmd unique n" (VVector (make_row_vec [1.;2.;3.])) "unique" 
      (Vector (make_row_vec [1.;2.;3.;3.]));

    test "quantile empty" 0. (quantile [] 0.5) string_of_float;
    test "quantile single" 1. (quantile [1.] 0.5) string_of_float;
    test "quantile 0.5" 4. (quantile [1.;2.;3.;4.;5.] 0.5) string_of_float;
    test "quantile 0.4" 3. (quantile [1.;2.;3.;4.;5.] 0.4) string_of_float;
    test "quantile 0.75" 5. (quantile [1.;2.;3.;4.;5.] 0.75) string_of_float;
    test "quantile 0.2" 2. (quantile [1.;2.;3.;4.;5.] 0.2) string_of_float;
    test "quantile 0.25" 2. (quantile [1.;2.;3.;4.;5.] 0.25) string_of_float;

    test_command "cmd quantile" (VFloat 2.) "quantile" 
      (Tuple (Float 0.25, Vector (make_row_vec [1.;2.;3.;4.;5.])));

    test "linreg 1 0" (2.,0.) 
      (linear_regression [(1.,2.);(2.,4.);(3.,6.)]) string_of_pair;
    test "linreg 1 0" (1.,3.) 
      (linear_regression [(1.,4.);(2.,5.);(3.,6.)]) string_of_pair;

    test_command "cmd bestfit" (VTuple (VFloat 2., VFloat 0.)) "bestfit" 
      (Tuple (Vector (make_row_vec [1.;2.;3.]), 
              Vector (make_row_vec [2.;4.;6.])));

    test_command "cmd linreg" (VTuple (VFloat 2., VFloat 0.)) "linreg" 
      (Tuple (Vector (make_row_vec [1.;2.;3.]), 
              Vector (make_row_vec [2.;4.;6.])));

    eval_error_test "comb float err"
      "Both arguements must be integer"
      (fun () -> Eval.eval_expr 
          (Command ("comb", Tuple (Float 1.5, Float 2.5))) []);

    eval_error_test "choose float err"
      "Both arguements must be integer"
      (fun () -> Eval.eval_expr 
          (Command ("choose", Tuple (Float 1.5, Float 2.5))) []);

    eval_error_test "perm float err"
      "Both arguements must be integer"
      (fun () -> Eval.eval_expr 
          (Command ("perm", Tuple (Float 1.5, Float 2.5))) []);

    eval_error_test "fac float err"
      "Factorial requires integer input"
      (fun () -> Eval.eval_expr 
          (Command ("fac",  Float 1.5)) []);
  ]

let eval_tests = 
  let eval_expr e sigma = fst (Eval.eval_expr e sigma) in
  [
    test "Var x is float" (VFloat 0.) (eval_expr (Var "x") [("x", VFloat 0.)])
      string_of_value;
    test "Assign to var x" (VFloat 0.)
      (eval_expr (Binop (Assign, Var "x", Int 0)) [("x", VFloat 0.)])
      string_of_value;
    test "Negate var y" (VFloat (-5.)) (eval_expr (Negate "y")
                                          [("y", VFloat 5.)]) string_of_value;
    test "Negate var y" (VFloat (5.)) (eval_expr (Negate "y")
                                         [("y", VFloat (-5.))]) string_of_value;
    "Unassigned var given Failure" >:: 
    (fun _ -> assert_raises 
        (Eval.ComputationError.EvalError
           "Variable x is undefined in current context") 
        (fun () -> (eval_expr (Negate "x") [("y", VFloat (-5.))]) ));
    eval_error_test "Variable x not defined"
      "Variable x is undefined in current context"
      (fun () -> Eval.eval_expr (Var "x") []);

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
    eval_error_test "Modulo when p and q are floats not representing ints" 
      "Cannot apply modulo operator to non-integer values"
      (fun () -> Eval.eval_expr (Binop (Mod, Float 0.1, Float 0.0)) []);
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

    test_command "Eval sin is equivelant to OCaml sin" (VFloat (sin 0.))
      "sin" (Float(0.));
    test_command "Eval cos is equivelant to OCaml cos" (VFloat (cos 0.))
      "cos" (Float(0.));
    test_command "Eval tan is equivelant to OCaml tan" (VFloat (tan 0.))
      "tan" (Float(0.));
    test_command "Eval sin is equivelant to OCaml sin" (VFloat (sin Float.pi))
      "sin" (Float(Float.pi));
    test_command "Eval cos is equivelant to OCaml cos" (VFloat (cos Float.pi))
      "cos" (Float(Float.pi));
    test_command "Eval tan is equivelant to OCaml tan" (VFloat (tan Float.pi))
      "tan" (Float(Float.pi));
    test_command "Eval arcsin is equivelant to OCaml arcsin" (VFloat (asin 0.))
      "arcsin" (Float(0.));
    test_command "Eval arccos is equivelant to OCaml arccos" (VFloat (acos 0.))
      "arccos" (Float(0.));
    test_command "Eval arctan is equivelant to OCaml arctan" (VFloat (atan 0.))
      "arctan" (Float(0.));
    test_command "Eval arcsin is equivelant to OCaml arcsin" 
      (VFloat (asin (-1.))) "arcsin" (Float(-1.));
    test_command "Eval arccos is equivelant to OCaml arccos"
      (VFloat (acos (-1.))) "arccos" (Float(-1.));
    test_command "Eval arctan is equivelant to OCaml arctan"
      (VFloat (atan (-1.))) "arctan" (Float(-1.));

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
      (VFloat (Bool.to_float true)) (eval_expr (Binop (GTE, Float 3., Int 2))
                                       []) string_of_value;

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
    linalg_matrix_vector_tests;
    solve_tests;
    prob_tests;
    stat_tests;
    eval_tests;
  ]

let _ = run_test_tt_main suite