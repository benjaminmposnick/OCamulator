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
  | Matrix of Matrix.t
  | Vector of Vector.t
  | Binop of binop * expr * expr
  | Prob of distribution
  | Command of string * expr

(** [value] is the type of values which result from evaluating expressions
    under the big-step relation/. *)
type value =
  | VFloat of float
  | VMatrix of Matrix.t
  | VVector of Vector.t
  | VEquation of binop * expr * expr
  | VInt of int
  | VList of value list

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

(** [string_of_value val] is the string respresentation of value [val]. *)
let rec string_of_value = function
  | VInt f -> "Int " ^ string_of_int f
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
          acc ^ "\n[Entry " ^ (string_of_int i) ^ "] " ^ (string_of_value h) in
        string_of_value_list t (i + 1) acc'
    in
    string_of_value_list lst 1 "Value List:"
