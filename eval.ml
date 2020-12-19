open Ast
open Prob
open Solve
open Vector

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

let prob_check p = 
  if p >= 0. && p <= 1. then ()
  else raise (ComputationError.EvalError "p is not a valid probability")

let nk_check n k = 
  if k <= n then ()
  else raise (ComputationError.EvalError "k > n (Binomial) or a > b (Uniform)")

let lmbda_check l = 
  if l > 0. then ()
  else raise (ComputationError.EvalError "Lambda must be > 0")

let neg_check x =
  if x >= 0. then ()
  else raise (ComputationError.EvalError "Input value must be >= 0")

let bern_check x = 
  if x = 0. || x = 1. then ()
  else raise (ComputationError.EvalError "Bernoulli RV's can only be 0 or 1")

let eval_prob dist sigma = 
  let value = 
    let open Prob in
    let open Float in
    let open ComputationError in
    match dist with
    | Binomial (PDF, n, p, k) when is_integer n && is_integer k ->
      prob_check p;
      neg_check k;
      nk_check n k;
      binomial_pmf (int_of_float n) p (int_of_float k)
    | Binomial (CDF, n, p, k) when is_integer n && is_integer k ->
      prob_check p;
      neg_check k;
      nk_check n k;
      binomial_cdf (int_of_float n) p (int_of_float k)
    | Binomial (SAM, n, p, k) when is_integer n -> 
      prob_check p;
      binomial_sam (int_of_float n) p
    | Binomial (f, n, p, k) ->
      raise (EvalError "n and k values of Binomial distribution must be integers")
    | Bernoulli (PDF, p, k) when is_integer k -> 
      prob_check p;
      bern_check k;
      bernoulli_pmf p (int_of_float k)
    | Bernoulli (CDF, p, k) when is_integer k -> 
      prob_check p;
      bernoulli_cdf p (int_of_float k)
    | Bernoulli (SAM, p, k) -> 
      prob_check p;
      bernoulli_sam p
    | Bernoulli (f, p, k) ->
      raise (EvalError "k value of Bernoulli distribution must be an integer")
    | Uniform (PDF, a, b, x) -> 
      nk_check b a;
      uniform_pmf a b x
    | Uniform (CDF, a, b, x) -> 
      nk_check b a;
      uniform_cdf a b x
    | Uniform (SAM, a, b, x) ->
      nk_check b a;
      uniform_sam a b
    | Poisson (PDF, l, x) when is_integer x ->
      lmbda_check l;
      neg_check x;
      poisson_pmf l (int_of_float x)
    | Poisson (CDF, l, x) when is_integer x -> 
      lmbda_check l;
      neg_check x;
      poisson_cdf l (int_of_float x)
    | Poisson (SAM, l, t) -> 
      lmbda_check l;
      neg_check t;
      poisson_sam l t
    | Poisson (f, l, x) -> 
      raise (EvalError "x value of Poisson distribution must be an integer")
    | Geometric (PDF, p, k) when is_integer k -> 
      neg_check k;
      prob_check p;
      geometric_pmf p (int_of_float k)
    | Geometric (CDF, p, k) when is_integer k ->
      neg_check k;
      prob_check p;
      geometric_cdf p (int_of_float k)
    | Geometric (SAM, p, k) -> 
      neg_check k;
      prob_check p;
      geometric_sam p
    | Geometric (f, p, k) ->
      raise (EvalError "k value of Geometric distribution must be an integer")
    | Exponential (PDF, l, x) -> 
      neg_check x;
      lmbda_check l;
      exponential_pmf l x
    | Exponential (CDF, l, x) -> 
      neg_check x;
      lmbda_check l;
      exponential_cdf l x
    | Exponential (SAM, l, x) -> 
      lmbda_check l;
      exponential_sam l
    | Normal (PDF, m, s, x) -> 
      neg_check s;
      normal_pmf m s x
    | Normal (CDF, m, s, x) -> 
      neg_check s;
      normal_cdf m s x
    | Normal (SAM, m, s, x) -> 
      neg_check s;
      normal_sam m s 
  in 
  VFloat value, sigma

let rand_vector dist i = 
  let open Prob in
  let open Vector in
  let rec rand_helper acc_list acc_i i f =
    if acc_i > i then acc_i
    else rand_helper (f:: acc_list) (acc_i + 1) i f
  in rand_helper [] 0 i dist

let stats_noargs_vec f vec = 
  let open Vector in
  let open Stat in
  let value = 
    vec
    |> to_list
    |> f
    |> make_row_vec
  in VVector value

let stats_noargs_float f vec = 
  let open Vector in
  let open Stat in
  let value =
    vec 
    |> to_list
    |> f
  in VFloat value

let eval_assign x v sigma =
  List.remove_assoc x sigma
  |> List.cons (x, v) 
  |> fun sigma' -> v, sigma'

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
    | SolveSys -> 
      let error_msg = "Left arg must be a matrix and right arg must be a vector" in
      raise (ComputationError.(TypeError error_msg))
  in
  (VFloat result, sigma)

let eval_binop_on_vectors op (v1 : Vector.t) (v2 : Vector.t) sigma =
  let open Vector in
  let result = match op with
    | Add -> VVector (component_wise_add v1 v2)
    | Sub -> VVector (component_wise_subtract v1 v2)
    | Mul -> VVector (component_wise_multiply v1 v2)
    | Dot -> VFloat (dot_product v1 v2)
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let eval_binop_on_matrices op (m1 : Matrix.t) (m2 : Matrix.t) sigma =
  let open Matrix in
  let rvecs1 = map Vector.make_row_vec m1 in
  let rvecs2 = map Vector.make_row_vec m2 in
  let result = match op with
    | Add -> VMatrix (of_vectors (List.map2 Vector.component_wise_add rvecs1 rvecs2))
    | Sub -> VMatrix (of_vectors (List.map2 Vector.component_wise_subtract rvecs1 rvecs2))
    | Mul -> VMatrix (multiply m1 m2)
    | _ -> failwith "TODO: Add more functionality"
  in
  (result, sigma)

let eval_binop_btwn_scalar_and_array op k arr sigma =
  let multiply = fun x -> k *. x in
  let exponentiate = fun x -> Float.pow x k in
  let apply_scalar_op_to_matrix mat =
    let rvecs = Matrix.map Vector.make_row_vec mat in
    VMatrix (Matrix.of_vectors (List.map (Vector.map multiply) rvecs)) in
  let value = match arr, op with
    | VVector vec, Mul -> VVector (Vector.map multiply vec)
    | VVector vec, Pow -> VVector (Vector.map exponentiate vec)
    | VMatrix mat, Mul -> apply_scalar_op_to_matrix mat
    | VMatrix mat, Pow -> apply_scalar_op_to_matrix mat
    | _, _ ->
      raise (ComputationError.EvalError "Invalid operation between scalar and array")
  in
  (value, sigma)

let eval_binop_btwn_matrix_and_vector op arr1 arr2 sigma =
  let open Linalg in
  let value = match arr1, arr2, op with
    | VVector (RowVector _ as vec), VMatrix mat, Mul ->
      VMatrix (Matrix.(multiply (of_vectors [vec]) mat)) (* Row vector *)
    | VMatrix mat, VVector (ColVector _ as vec), Mul ->
      VMatrix (Matrix.(multiply mat (of_vectors [vec]))) (* Column vector *)
    | VMatrix mat, VVector (ColVector _ as vec), SolveSys ->
      VVector (Linalg.(solve_system mat vec)) (* Column vector *)
    | VVector (ColVector vec), VMatrix mat, Mul ->
      raise (ComputationError.EvalError "Shape error: first argument should be a row vector")
    | VMatrix mat, VVector (RowVector vec), Mul ->
      raise (ComputationError.EvalError "Shape error: second argument should be a column vector")
    | _, _, _ ->
      raise (ComputationError.EvalError "Invalid operation between matrix and vector")
  in
  (value, sigma)

let rec eval_binop op e1 e2 sigma  =
  let (v1, sigma') = eval_expr e1 sigma in
  let (v2, sigma'') = eval_expr e2 sigma in
  match v1, v2 with
  | VFloat f1, VFloat f2 -> eval_binop_on_floats op f1 f2 sigma''
  | VMatrix m1, VMatrix m2 ->
    eval_binop_on_matrices op m1 m2 sigma 
  | VFloat f, (_ as arr) -> eval_binop_btwn_scalar_and_array op f arr sigma
  | (_ as arr), VFloat f -> eval_binop_btwn_scalar_and_array op f arr sigma
  | VMatrix _, VVector _ | VVector _, VMatrix _  -> 
    eval_binop_btwn_matrix_and_vector op v1 v2 sigma
  | VVector vec1, VVector vec2 -> eval_binop_on_vectors op vec1 vec2 sigma''
  | _ -> failwith "Error evaluating binop"

and evaluate_command cmd e sigma = 
  let open Linalg in
  let open Stat in
  let (value, sigma') =
    if cmd <> "solve" then eval_expr e sigma
    else
      match e with
      | Binop(op, e1, e2) -> (VEquation (op, e1, e2)), sigma
      | _ -> failwith "Invalid operation on a non-equation" in
  let result = match cmd, value with
    | "rref", VMatrix m -> VMatrix (Linalg.rref m)
    | "rref", _ ->
      raise (ComputationError.EvalError "Cannot row reduce a non-matrix")
    | "transpose", VMatrix mat -> VMatrix (Matrix.transpose mat)
    | "transpose", VVector vec -> VVector (Vector.transpose vec)
    | "transpose", _ ->
      raise (ComputationError.EvalError "Cannot transpose non-matrix / non-vector")
    | "pivots", VMatrix m -> VList (pivot_cols m)
    | "pivots", _ ->
      raise (ComputationError.EvalError "Cannot calculate pivots of a non-matrix")
    | "det", VMatrix mat -> VFloat (Linalg.determinant mat)
    | "det", _ -> raise (ComputationError.EvalError "Cannot calculate determinant of non-matrix")
    | "inv", VMatrix mat -> VMatrix (Linalg.inverse mat)
    | "inv", _ -> raise (ComputationError.EvalError "Cannot calculate inverse of non-matrix")
    | "plu",  VMatrix mat ->
      let (p, l, u, _) = Linalg.plu_decomposition mat in
      VList [VMatrix p; VMatrix l; VMatrix u]
    | _, VList lst when String.(length cmd > 0 && get cmd 0 = '#')-> begin
        match int_of_string_opt (String.(sub cmd 1 (length cmd - 1))) with
        | None -> raise (ComputationError.EvalError "Cannot index list with non-integer")
        | Some idx -> begin
            match List.nth_opt lst (idx - 1) with
            | None -> raise (ComputationError.EvalError "Index out of bounds")
            | Some v -> v
          end
      end
    | "solve", VEquation (op, e1, e2) -> begin
        print_endline "What variable would you like to solve for? ";
        let input = read_line () in 
        let solve_output = solve input (Binop(op, e1, e2)) in
        match solve_output with
        |Binop (op, e1, e2) -> VEquation (op, e1, e2)
        |_ -> failwith "Error solving equation"
      end
    | "mean", VVector vec -> stats_noargs_float mean vec
    | "median", VVector vec -> stats_noargs_float median vec
    | "sort_asc", VVector vec -> stats_noargs_vec sort_asc vec
    | "sort_desc", VVector vec -> stats_noargs_vec sort_desc vec
    | "min", VVector vec -> stats_noargs_float min vec
    | "max", VVector vec -> stats_noargs_float max vec
    | "variance", VVector vec -> stats_noargs_float smpl_var vec
    | "std", VVector vec -> stats_noargs_float smpl_std vec
    | "sum", VVector vec -> stats_noargs_float cum_sum vec
    | "product", VVector vec -> stats_noargs_float cum_prod vec
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
  | Matrix mat ->  VMatrix mat, sigma
  | Vector vec -> VVector vec, sigma
  | Command (cmd, e) -> 
    let cmd' = String.lowercase_ascii cmd in
    evaluate_command cmd' e sigma 

let rec eval_input e sigma = 
  let (value, sigma') = eval_expr e sigma in
  eval_assign "ans" value sigma'
