open Ast

(* ===========================================================================
    EXCEPTIONS
   ===========================================================================*)
module EvalError = struct
  let unbound_variable_msg =
    "UnboundVariable Error: Variable not bound in current store: "

  let non_numeric_value_msg =
    "Type Error: Cannot use non-numeric values in arithmetic operation"

  let non_vector_value_msg =
    "Type Error: Cannot use non-vector values in vector operation"

  let modulo_msg =
    "Type Error: Cannot apply modulo operator to non-integer values"

  let assign_msg = "Type Error: Cannot assign to non-variable: "

  exception UnboundVariable of string

  exception TypeError of string
end

(* ===========================================================================
    EVALUATION
   ===========================================================================*)

let modulo p q =
  if not (Float.(is_integer p && is_integer q)) then
    raise (EvalError.(TypeError modulo_msg))
  else
    let p' = int_of_float p in
    let q' = int_of_float q in
    p' mod q' |> float_of_int 

let rec var_present = function
  | Binop (op, e1, e2) -> var_present e1 || var_present e2
  | Var x -> true
  | _ -> false

let eval_var x sigma =
  match List.assoc_opt x sigma with
  | None -> raise (EvalError.(UnboundVariable (unbound_variable_msg ^ x)))
  | Some value -> value, sigma

let eval_prob dist sigma = 
  let value = 
    let open Prob in
    match dist with
    | Binomial (PDF, n, p, k) -> binomial_pmf n p k
    | Binomial (CDF, n, p, k) -> binomial_cdf n p k
    | Bernoulli (PDF, p, k) -> bernoulli_pmf p k
    | Bernoulli (CDF, p, k) -> bernoulli_cdf p k
    | Uniform (PDF, a, b, x) -> uniform_pmf a b x
    | Uniform (CDF, a, b, x) -> uniform_cdf a b x
    | Poisson (PDF, l, x) -> poisson_pmf l x
    | Poisson (CDF, l, x) -> poisson_cdf l x
    | Geometric (PDF, p, k) -> geometric_pmf p k
    | Geometric (CDF, p, k) -> geometric_cdf p k
    | Exponential (PDF, l, x) -> exponential_pmf l x
    | Exponential (CDF, l, x) -> exponential_cdf l x
    | Normal (PDF, m, s, x) -> normal_pmf m s x
    | Normal (CDF, m, s, x) -> normal_cdf m s x
  in 
  VFloat value, sigma

let eval_assign x v sigma =
  List.remove_assoc x sigma
  |> List.cons (x, v) 
  |> fun sigma' -> v, sigma'

let float_list_from_vec vec =
  match vec with
  | RowVector lst -> lst
  | ColumnVector lst -> lst
  | Matrix _ -> failwith "Not a vector"

let list_of_float_lists_from_matrix mat =
  match mat with
  | Matrix lst -> lst
  | RowVector _ | ColumnVector _ -> failwith "Not a matrix"

let eval_binop_on_floats op f1 f2 sigma =
  let result = match op with
    | Add -> f1 +. f2
    | Sub -> f1 -. f2
    | Mul -> f1 *. f2
    | Div -> f1 /. f2
    | Mod -> modulo f1 f2
    | Pow -> Float.pow f1 f2
    | Eq -> Bool.to_float (f1 = f2)
    | LT -> Bool.to_float (f1 < f2)
    | GT -> Bool.to_float (f1 > f2)
    | LTE -> Bool.to_float (f1 <= f2)
    | GTE -> Bool.to_float (f1 >= f2)
    | Assign -> raise (EvalError.(TypeError assign_msg))
    | Dot -> raise (EvalError.(TypeError non_vector_value_msg))
  in
  (VFloat result, sigma)

let eval_binop_on_vectors op v1 v2 sigma =
  let result = match op with
    | Add -> VArray (RowVector (Linalg.component_wise_add v1 v2))
    | Sub -> VArray (RowVector (Linalg.component_wise_subtract v1 v2))
    | Mul -> VArray (RowVector (Linalg.component_wise_multiply v1 v2))
    | Dot -> VFloat (Linalg.dot_product v1 v2)
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let eval_binop_on_matrix op m1 m2 sigma =
  let result = match op with
    | Mul -> VArray (Matrix (Linalg.matrix_multiply m1 m2))
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let rec eval_binop op e1 e2 sigma  =
  let (v1, sigma') = eval_expr e1 sigma in
  let (v2, sigma'') = eval_expr e2 sigma in
  match v1, v2 with
  | VFloat f1, VFloat f2 -> eval_binop_on_floats op f1 f2 sigma''
  | VArray (Matrix m1), VArray (Matrix m2) -> eval_binop_on_matrix op m1 m2 sigma 
  | VArray (Matrix _), _ -> failwith "TODO: Add more functionality"
  | _, VArray (Matrix _) -> failwith "TODO: Add more functionality"
  | VArray vec1, VArray vec2 -> begin
      let vec1' = float_list_from_vec vec1 in
      let vec2' = float_list_from_vec vec2 in
      eval_binop_on_vectors op vec1' vec2' sigma''
    end
  | _ -> failwith "TODO"

and evaluate_command cmd e sigma =
  let (value, sigma') = eval_expr e sigma in
  let result = match cmd, value with
    | "rref", VArray (Matrix m) -> VArray (Matrix (Linalg.rref m))
    | "rref", _ -> failwith "Cannot row reduce a vector"
    | "transpose", VArray arr -> VArray (Linalg.transpose arr)
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma')

and eval_expr e sigma =
  match e with
  | Var x -> eval_var x sigma
  | Int i -> VFloat (float_of_int i), sigma
  | Float f -> VFloat f, sigma
  | Binop (Assign, Var x, e) ->
    let (v, sigma') = eval_expr e sigma in 
    eval_assign x v sigma'
  | Binop (op, e1, e2) -> eval_binop op e1 e2 sigma
  | Prob dist -> eval_prob dist sigma
  | Array arr -> VArray arr, sigma
  | Command (cmd, e) -> 
    let cmd' = String.lowercase_ascii cmd in
    evaluate_command cmd' e sigma 

let rec eval_input e sigma = 
  let (value, sigma') = eval_expr e sigma in
  eval_assign "ans" value sigma'
