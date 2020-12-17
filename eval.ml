open Ast
open Prob
open Solve

module ComputationError = struct
  exception EvalError of string
  exception TypeError of string
end

let modulo p q =
  if not (Float.(is_integer p && is_integer q)) then
    let error_msg = "Cannot apply modulo operator to non-integer values" in
    raise (ComputationError.(TypeError error_msg))
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
  | None -> 
    let error_msg = "Variable is undefined in current context: " ^ x in
    raise (ComputationError.(EvalError error_msg))
  | Some value -> value, sigma

let eval_prob dist sigma = 
  let value = 
    let open Prob in
    let open Float in
    let open ComputationError in
    match dist with
    | Binomial (PDF, n, p, k) when is_integer n && is_integer k ->
      binomial_pmf (int_of_float n) p (int_of_float k)
    | Binomial (CDF, n, p, k) when is_integer n && is_integer k ->
      binomial_cdf (int_of_float n) p (int_of_float k)
    | Binomial (PDF, n, p, k) | Binomial (CDF, n, p, k) ->
      raise (EvalError "n and k values of Binomial distribution must be integers")
    | Bernoulli (PDF, p, k) when is_integer k -> bernoulli_pmf p (int_of_float k)
    | Bernoulli (CDF, p, k) when is_integer k -> bernoulli_cdf p (int_of_float k)
    | Bernoulli (PDF, p, k) | Bernoulli (CDF, p, k) ->
      raise (EvalError "k value of Bernoulli distribution must be an integer")
    | Uniform (PDF, a, b, x) -> uniform_pmf a b x
    | Uniform (CDF, a, b, x) -> uniform_cdf a b x
    | Poisson (PDF, l, x) when is_integer x -> poisson_pmf l (int_of_float x)
    | Poisson (CDF, l, x) when is_integer x -> poisson_cdf l (int_of_float x)
    | Poisson (PDF, l, x) | Poisson (CDF, l, x) -> 
      raise (EvalError "x value of Poisson distribution must be an integer")
    | Geometric (PDF, p, k) when is_integer k -> geometric_pmf p (int_of_float k)
    | Geometric (CDF, p, k) when is_integer k -> geometric_cdf p (int_of_float k)
    | Geometric (PDF, p, k) | Geometric (CDF, p, k) ->
      raise (EvalError "k value of Geometric distribution must be an integer")
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
  | Matrix _ -> raise (ComputationError.EvalError ("Not a vector"))

let list_of_float_lists_from_matrix mat =
  let open List in
  match mat with
  | RowVector _ | ColumnVector _ -> 
    raise (ComputationError.EvalError ("Not a matrix"))
  | Matrix m -> (* Matrix cannot be empty based off lexer/parser *)
    let n_cols = length (hd m) in
    if fold_left (fun acc lst -> acc && (length lst = n_cols)) true m then m
    else 
      let error_msg = "Each row must have the same number of columns" in
      raise (ComputationError.EvalError (error_msg))

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
    | Assign ->
      let error_msg = "Cannot assign to non-variable: " ^ string_of_float f1 in
      raise (ComputationError.(TypeError error_msg))
    | Dot ->
      let error_msg = "Cannot use non-vector values in vector operation" in
      raise (ComputationError.(TypeError error_msg))
  in
  (VFloat result, sigma)

let eval_binop_on_vectors op v1 v2 sigma =
  let open Linalg in
  let result = match op with
    | Add -> VArray (RowVector (component_wise_add v1 v2))
    | Sub -> VArray (RowVector (component_wise_subtract v1 v2))
    | Mul -> VArray (RowVector (component_wise_multiply v1 v2))
    | Dot -> VFloat (Linalg.dot_product v1 v2)
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let eval_binop_on_matrices op m1 m2 sigma =
  let open Linalg in
  let result = match op with
    | Add -> VArray (Matrix (List.map2 component_wise_add m1 m2))
    | Sub -> VArray (Matrix (List.map2 component_wise_subtract m1 m2))
    | Mul -> VArray (Matrix (Linalg.matrix_multiply m1 m2))
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let eval_binop_btwn_scalar_and_array op k arr sigma =
  let open List in
  let multiply = fun x -> k *. x in
  let exponentiate = fun x -> Float.pow x k in
  let value = match arr, op with
    | RowVector vec, Mul -> VArray (RowVector (map multiply vec))
    | RowVector vec, Pow -> VArray (RowVector (map exponentiate vec))
    | ColumnVector vec, Mul -> VArray (ColumnVector (map multiply vec))
    | ColumnVector vec, Pow -> VArray (ColumnVector (map exponentiate vec))
    | Matrix mat, Mul -> VArray (Matrix (map (map multiply) mat))
    | Matrix mat, Pow -> VArray (Matrix (map (map exponentiate) mat))
    | _, _ ->
      raise (ComputationError.EvalError "Invalid operation between scalar and array")
  in
  (value, sigma)

let eval_binop_btwn_matrix_and_vector op arr1 arr2 sigma =
  let open Linalg in
  let value = match arr1, arr2, op with
    | VArray (RowVector vec), VArray (Matrix mat), Mul ->
      RowVector (List.hd (matrix_multiply [vec] mat))
    | VArray (Matrix mat), VArray (ColumnVector vec), Mul ->
      let cvec = List.map (fun elem -> [elem]) vec in
      ColumnVector (List.flatten (matrix_multiply mat cvec))
    | VArray (ColumnVector vec), VArray (Matrix mat), Mul ->
      raise (ComputationError.EvalError "Shape error: first argument should be a row vector")
    | VArray (Matrix mat), VArray (RowVector vec), Mul ->
      raise (ComputationError.EvalError "Shape error: second argument should be a column vector")
    | _, _, _ ->
      raise (ComputationError.EvalError "Invalid operation between matrix and vector")
  in
  (VArray value, sigma)

let rec eval_binop op e1 e2 sigma  =
  let (v1, sigma') = eval_expr e1 sigma in
  let (v2, sigma'') = eval_expr e2 sigma in
  match v1, v2 with
  | VFloat f1, VFloat f2 -> eval_binop_on_floats op f1 f2 sigma''
  | VArray (Matrix m1), VArray (Matrix m2) ->
    eval_binop_on_matrices op m1 m2 sigma 
  | VFloat f, VArray arr -> 
    eval_binop_btwn_scalar_and_array op f arr sigma
  | VArray arr, VFloat f -> 
    eval_binop_btwn_scalar_and_array op f arr sigma
  | VArray (Matrix _), VArray _ | VArray _, VArray (Matrix _) -> 
    eval_binop_btwn_matrix_and_vector op v1 v2 sigma
  | VArray vec1, VArray vec2 -> begin
      let vec1' = float_list_from_vec vec1 in
      let vec2' = float_list_from_vec vec2 in
      eval_binop_on_vectors op vec1' vec2' sigma''
    end
  | _ -> failwith "Error evaluating binop"

and evaluate_command cmd e sigma =
  let open Linalg in
  let (value, sigma') = if cmd <> "solve" then eval_expr e sigma
    else
      match e with
      | Binop(op, e1, e2) -> (VEquation (op, e1, e2)), sigma
      | _ -> failwith "Invalid operation on a non-equation"
  in
  let result = match cmd, value with
    | "rref", VArray (Matrix m) -> VArray (Matrix (Linalg.rref m))
    | "rref", _ ->
      raise (ComputationError.EvalError "Cannot row reduce a vector")
    | "transpose", VArray arr -> VArray (Linalg.transpose arr)
    | "pivots", VArray (Matrix m) -> VArray (RowVector (pivot_cols m))
    | "pivots", _ ->
      raise (ComputationError.EvalError "Cannot calculate pivots of a vector")
    | "solve", VEquation (op, e1, e2) -> begin
      print_endline "What variable would you like to solve for? ";
      let input = read_line () in 
      let solve_output = solve input (Binop(op, e1, e2)) in
          match solve_output with
          |Binop (op, e1, e2) -> VEquation (op, e1, e2)
          |_ -> failwith "Error solving equation"
      end
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
  | Array arr -> begin match arr with 
      | Matrix m -> VArray (Matrix (list_of_float_lists_from_matrix arr)), sigma
      | _ -> VArray arr, sigma
    end
  | Command (cmd, e) -> 
    let cmd' = String.lowercase_ascii cmd in
    evaluate_command cmd' e sigma 

let rec eval_input e sigma = 
  let (value, sigma') = eval_expr e sigma in
  eval_assign "ans" value sigma'
