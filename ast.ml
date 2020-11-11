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

type vector = float list

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