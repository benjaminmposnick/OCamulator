(* ===========================================================================
    TYPE DEFINITIONS
   ===========================================================================*)

(** [binop] is the type of binary operators. *)
type binop =
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
  | Dot

(** [array] is the type of vectors and matrices. *)
type array = 
  | RowVector of float list
  | ColumnVector of float list
  (** [Matrix] representation invariant: treated as a list of row vectors *)
  | Matrix of float list list 

(** [prob_func] is the type of density functions for distributions. *)
type prob_func =
  | PDF
  | CDF

(** [distribution] is the type of probability distributions. *)
type distribution =
  | Binomial of prob_func * float * float * float
  | Bernoulli of prob_func * float * float
  | Uniform of prob_func * float * float * float
  | Poisson of prob_func * float * float
  | Geometric of prob_func * float * float
  | Exponential of prob_func * float * float
  | Normal of prob_func * float * float * float

(** [expr] is the type of input expressions to be evaluated. *)
type expr = 
  | Var of string
  | Int of int
  | Float of float
  | Array of array
  | Binop of binop * expr * expr
  | Prob of distribution
  | Command of string * expr

(** [value] is the type of values which result from evaluating expressions
    under the big-step relation/. *)
type value =
  | VFloat of float
  | VArray of array

(* ===========================================================================
    TO STRING FUNCTIONS
   ===========================================================================*)

(** [string_of_prob_func prob_func] is the string respresentation of
    [prob_func]. *)
let string_of_prob_func = function
  | PDF -> "PDF"
  | CDF -> "CDF"

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
  | Dot -> "Dot"

(** [string_of_vector_contents sep vec] is the string respresentation of the
    contents of vector [vec], where each entry is separated by [sep]. *)
let string_of_vector_contents sep vec =
  List.map string_of_float vec |> String.concat sep

(** [string_of_matrix mat] is the string respresentation of matrix [mat]. *)
let string_of_matrix mat =
  List.map (string_of_vector_contents ", ") mat |> String.concat ";\n"

(** [string_of_distribution dis] is the string respresentation of the
    probability distribution [dist]. *)
let string_of_distribution = function
  | Binomial (func, n, p, k) ->
    "Binomial " ^ string_of_prob_func func 
  | Bernoulli (func, p, k) ->
    "Bernoulli " ^ string_of_prob_func func 
  | Uniform (func, a, b, x) ->
    "Uniform " ^ string_of_prob_func func 
  | Poisson (func, l, k) ->
    "Poisson " ^ string_of_prob_func func 
  | Geometric (func, p, k) ->
    "Geometric " ^ string_of_prob_func func 
  | Exponential (func, l, x) ->
    "Expontential " ^ string_of_prob_func func 
  | Normal (func, m, s, x) ->
    "Normal " ^ string_of_prob_func func 

(** [string_of_array arr] is the string representation of array [arr]*)
let string_of_array = function
  | RowVector vec -> "RowVector [" ^ (string_of_vector_contents ", " vec) ^ "]"
  | ColumnVector vec -> "ColVector [" ^ (string_of_vector_contents "; " vec) ^ "]"
  | Matrix mat -> "Matrix " ^ (string_of_matrix mat)

(** [string_of_expr expr] is the string respresentation of expression [expr]. *)
let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Binop (op, e1, e2) -> "Binop (" ^ (string_of_binop op) ^ ", " ^
                          (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Prob dist -> string_of_distribution dist
  | Array arr -> string_of_array arr
  | Command (cmd, e) -> "Command (" ^ cmd ^ ", " ^ (string_of_expr e) ^ ")"

(** [string_of_value val] is the string respresentation of value [val]. *)
let string_of_value = function
  | VFloat f -> "Float " ^ string_of_float f
  | VArray arr -> string_of_array arr