open Vector

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
  | SolveSys

(** [prob_func] is the type of operators on distributions. *)
type prob_func =
  | PDF
  | CDF
  | SAM

(** [stat_func] is the type of the statistical function to be executed. *)
type stat_func =
  | SORT_ASC
  | SORT_DESC
  | UNIQUE
  | QUANTILE

(** [stat_equ] is the type of the statistical equation. *)
type stat_equ = 
  | NoArg of stat_func
  | OneArg of stat_func * float

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
  | Binop of binop * expr * expr
  | Command of string * expr
  | Vector of Vector.t
  | Matrix of Matrix.t
  | Prob of distribution

(** [value] is the type of values which result from evaluating expressions
    under the big-step relation. *)
type value =
  | VFloat of float
  | VVector of Vector.t
  | VMatrix of Matrix.t
  | VList of value list
  | VEquation of binop * expr * expr

(* ===========================================================================
    TO STRING FUNCTIONS
   ===========================================================================*)

(** [string_of_prob_func prob_func] is the string respresentation of
    [prob_func]. *)
let string_of_prob_func = function
  | PDF -> "PDF"
  | CDF -> "CDF"
  | SAM -> "SAM"

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
  | SolveSys -> "SolveSys"

(** [string_of_distribution dist] is the string respresentation of the
    probability distribution [dist]. *)
let string_of_distribution = function
  | Binomial (func, _, _, _) -> "Binomial " ^ string_of_prob_func func 
  | Bernoulli (func, _, _) -> "Bernoulli " ^ string_of_prob_func func 
  | Uniform (func, _, _, _) -> "Uniform " ^ string_of_prob_func func 
  | Poisson (func, _, _) -> "Poisson " ^ string_of_prob_func func 
  | Geometric (func, _, _) -> "Geometric " ^ string_of_prob_func func 
  | Exponential (func, _, _) -> "Expontential " ^ string_of_prob_func func 
  | Normal (func, _, _, _) -> "Normal " ^ string_of_prob_func func 

(** [string_of_expr expr] is the string respresentation of expression [expr]. *)
let rec string_of_expr = function
  | Var x -> "Var " ^ x
  | Int i -> "Int " ^ string_of_int i
  | Float f -> "Float " ^ string_of_float f
  | Binop (op, e1, e2) -> "Binop (" ^ (string_of_binop op) ^ ", " ^
                          (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
  | Prob dist -> string_of_distribution dist
  | Command (cmd, e) -> "Command (" ^ cmd ^ ", " ^ (string_of_expr e) ^ ")"
  | Vector vec -> "Vector \n" ^ Vector.string_of_vector vec
  | Matrix mat -> "Matrix \n" ^ Matrix.string_of_matrix mat

(** [string_of_value value] is the string respresentation of [value]. *)
let rec string_of_value = function
  | VFloat f -> "Float " ^ string_of_float f
  | VVector vec -> "Vector \n" ^ Vector.string_of_vector vec
  | VMatrix mat -> "Matrix \n" ^ Matrix.string_of_matrix mat
  | VEquation (op, e1, e2) -> string_of_expr (Binop (op, e1, e2))
  | VList lst -> 
    let rec string_of_value_list lst i acc =
      match lst with
      | [] -> acc
      | h :: t ->
        let acc' =
          acc ^ "\n[Entry " ^ (string_of_int i) ^ "]  " ^ (string_of_value h) in
        string_of_value_list t (i + 1) acc'
    in
    string_of_value_list lst 1 "Value List:"
