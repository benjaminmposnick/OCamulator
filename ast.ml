type op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Pow
  | Eq
  | LT
  | GT
  | LTE
  | GTE
  | Assign

type array = 
  | RowVector of float list
  | ColumnVector of float list
  | Matrix of float list list  (* Treated as list of row vectors *)

type prob_func =
  | PDF
  | CDF

type expr = 
  | Var of string
  | Int of int
  | Float of float
  | NumArray of array
  | Binop of op * expr * expr
  | Binomial of prob_func * int * float * int
  | Bernoulli of prob_func * float * int
  | Uniform of prob_func * float * float * float
  | Poisson of prob_func * float * int
  | Geometric of prob_func * float * int
  | Exponential of prob_func * float * float
  | Normal of prob_func * float * float * float
  | Ans

type parsed_input =
  | Command of string * expr
  | Expression of expr

(** [string_of_prob_func prob_func] is the string respresentation of
    [prob_func]. *)
let string_of_prob_func = function
  | PDF -> "Probability Density"
  | CDF -> "Cumulative Density"

(** [string_of_binop binop] is the string respresentation of [binop]. *)
let string_of_binop = function
  | Add -> "Add"
  | Sub -> "Sub"
  | Mul -> "Mul"
  | Div -> "Div"
  | Mod -> "Mod"
  | Pow -> "Pow"  
  | Eq -> "Eq"
  | LT -> "LT"
  | GT -> "GT"
  | LTE -> "LTE"
  | GTE -> "GTE"
  | Assign -> "Assign"

(** [string_of_vector_contents sep vec] is the string respresentation of the
    contents of [vec], where each entry is separated by [sep]. *)
let string_of_vector_contents sep vec =
  List.map string_of_float vec |> String.concat sep

(** [string_of_matrix mat] is the string respresentation of [mat]. *)
let string_of_matrix mat =
  List.map (string_of_vector_contents ", ") mat |> String.concat ";\n"

(** [string_of_expr expr] is the string respresentation of [expr]. *)
let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Binop (op, e1, e2) -> 
    "Binop (" ^ (string_of_binop op) ^ ", "  ^ (string_of_expr e1) ^
    ", " ^ (string_of_expr e2) ^ ")"
  | Binomial (func, n, p, k) -> "Binomial Distrubution " ^ string_of_prob_func func 
  | Bernoulli (func, p, k) -> "Bernoulli Distrubution " ^ string_of_prob_func func 
  | Uniform (func, a, b, x) -> "Uniform Distrubution " ^ string_of_prob_func func 
  | Poisson (func, l, k) -> "Poisson Distrubution " ^ string_of_prob_func func 
  | Geometric (func, p, k) -> "Geometric Distrubution " ^ string_of_prob_func func 
  | Exponential (func, l, x) -> "Expontential Distrubution " ^ string_of_prob_func func 
  | Normal (func, m, s, x) -> "Normal Distrubution " ^ string_of_prob_func func 
  | NumArray arr -> begin
      match arr with
      | RowVector vec -> "RowVector [" ^ (string_of_vector_contents ", " vec) ^ "]"
      | ColumnVector vec -> "ColVector [" ^ (string_of_vector_contents "; " vec) ^ "]"
      | Matrix mat -> "Matrix \n" ^ (string_of_matrix mat)
    end
  | Ans -> "Ans"

(** [string_of_input inp] is the string respresentation of [inp]. *)
let string_of_input = function
  | Command (cmd, e) -> cmd ^ ": " ^ string_of_expr e
  | Expression e -> string_of_expr e
