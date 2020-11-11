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

type array = 
  | RowVector of float list
  | ColumnVector of float list
  | Matrix of float list list  (* Treated as list of row vectors *)

type prob_func =
  | PDF
  | CDF

let string_of_prob_func = function
  | PDF -> "Probability Density"
  | CDF -> "Cumulative Density"

type expr = 
  | Var of string
  | Int of int
  | Float of float
  | NumArray of array
  | Binop of op * expr * expr
  | Vector of vector
  | Matrix of vector list 
  | Binomial of prob_func * int * float * int
  | Bernoulli of prob_func * float * int
  | Uniform of prob_func * float * float * float
  | Poisson of prob_func * float * int
  | Geometric of prob_func * float * int
  | Exponential of prob_func * float * float
  | Normal of prob_func * float * float * float

type parsed_input =
  | Command of string * expr
  | Expression of expr

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

let string_of_vector_contents sep vec =
  (* (Printf.sprintf "%.16f") *)
  List.map string_of_float vec |> String.concat sep

let string_of_matrix mat =
  List.map (string_of_vector_contents ", ") mat |> String.concat ";\n"

let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Binop (op, e1, e2) -> 
    "Binop (" ^ (string_of_binop op) ^ ", "  ^ (string_of_expr e1) ^
    ", " ^ (string_of_expr e2) ^ ")"
  | Vector vec -> "Vector [" ^ (String.concat ", " (List.map string_of_float vec))  ^ "]"
  | Matrix mat -> "Matrix"
  | Binomial (func, n, p, k) -> "Binomial Distrubution " ^ string_of_prob_func func 
  | Bernoulli (func, p, k) -> "Bernoulli Distrubution " ^ string_of_prob_func func 
  | Uniform (func, a, b, x) -> "Uniform Distrubution " ^ string_of_prob_func func 
  | Poisson (func, l, k) -> "Poisson Distrubution " ^ string_of_prob_func func 
  | Geometric (func, p, k) -> "Geometric Distrubution " ^ string_of_prob_func func 
  | Exponential (func, l, x) -> "Expontential Distrubution " ^ string_of_prob_func func 
  | Normal (func, m, s, x) -> "Normal Distrubution " ^ string_of_prob_func func 
  | Binop (op, e1, e2) -> "Binop (" ^ (string_of_binop op) ^ ", " ^
                          (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | NumArray arr -> begin
      match arr with
      | RowVector vec -> "RowVector [" ^ (string_of_vector_contents ", " vec) ^ "]"
      | ColumnVector vec -> "ColVector [" ^ (string_of_vector_contents "; " vec) ^ "]"
      | Matrix mat -> "Matrix \n" ^ (string_of_matrix mat)
    end

let string_of_input = function
  | Command (cmd, e) -> cmd ^ ": " ^ string_of_expr e
  | Expression e -> string_of_expr e
