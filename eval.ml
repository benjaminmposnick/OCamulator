open Ast
open Prob
open Solve
open Vector
open Matrix
open Linalg

(** [store] is the type of the variable store, which maps variable identifiers
    to their most recently bound value. *)
type store = (string * value) list

(** [result] is the type of the result from evaluation, which contains both
    the value resulting from evaluation under the big-step relation as well
    as the store. *)
type result = value * store

module ComputationError = struct
  exception EvalError of string
end

(** [modulo p q] is [p] mod [q] if [p] and [q] are both floats representing
    integers (e.g. 1.0 for 1) and raises [EvalError] otherwise. *)
let modulo p q =
  if not (Float.is_integer p && Float.is_integer q) then
    let error_msg = "Cannot apply modulo operator to non-integer values" in
    raise (ComputationError.(EvalError error_msg))
  else
    let p' = int_of_float p in
    let q' = int_of_float q in
    p' mod q' |> float_of_int

let rec var_present = function
  | Binop (op, e1, e2) -> var_present e1 || var_present e2
  | Var x -> true
  | _ -> false

(** [eval_var x sigma] is the value that is bound to [x] in store [sigma] if
    [x] is in [sigma]; otherwise, [EvalError] is raised. *)
let eval_var x sigma =
  match List.assoc_opt x sigma with
  | None -> 
    let error_msg = "Variable " ^ x ^ " is undefined in current context" in
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

(** [eval_binomial func n p k] is result of [binomial_cdf n p k] if [func]
    = [CDF] and is the result of [binomial_pmf n p k] if [func] = [PDF], so long
    as [n] and [k] are floats representing integers; otherwise, [EvalError]
    is raised. *)
let eval_binomial func n p k =
  let open Prob in
  prob_check p;
  neg_check k;
  nk_check n k;
  if Float.is_integer n && Float.is_integer k then
    if func = SAM then binomial_sam (int_of_float n) p
    else if func = PDF then binomial_pmf (int_of_float n) p (int_of_float k)
    else (* func = CDF *) binomial_cdf (int_of_float n) p (int_of_float k)
  else    
    let error_msg = "n and k values of Binomial distribution must be ints" in
    raise (ComputationError.EvalError error_msg)

(** [eval_bernoulli func p k] is result of [bernoulli_cdf p k] if [func] = [CDF]
    and is the result of [bernoulli_pmf p k] if [func] = [PDF], so long as [k]
    is a float representing an integer; otherwise, [EvalError] is raised. *)
let eval_bernoulli func p k =
  prob_check p;
  bern_check k;
  if Float.is_integer k then
    if func = SAM then bernoulli_sam p
    else if func = PDF then bernoulli_pmf p (int_of_float k)
    else (* func = CDF *) bernoulli_cdf p (int_of_float k)
  else
    let error_msg = "k value of Bernoulli distribution must be an integer" in
    raise (ComputationError.EvalError error_msg)

let eval_uniform func a b x =
  nk_check b a;
  if func = SAM then uniform_sam a b
  else if func = PDF then uniform_pmf a b x
  else (* func = CDF *) uniform_cdf a b x

(** [eval_poisson func l x] is result of [poisson_cdf l x] if [func] = [CDF]
    and is the result of [poisson_pmf l x] if [func] = [PDF], so long as [x]
    is a float representing an integer; otherwise, [EvalError] is raised. *)
let eval_poisson func l x =
  lmbda_check l;
  neg_check x;
  if func = SAM then poisson_sam l x
  else if Float.is_integer x then
    if func = PDF then poisson_pmf l (int_of_float x)
    else (* func = CDF *) poisson_cdf l (int_of_float x)
  else
    let error_msg = "x value of Poisson distribution must be an integer" in
    raise (ComputationError.EvalError error_msg)

(** [eval_geometric func p k] is result of [geometric_cdf p k] if [func] = [CDF]
    and is the result of [geometric_pmf p k] if [func] = [PDF], so long as [k]
    is a float representing an integer; otherwise, [EvalError] is raised. *)
let eval_geometric func p k =
  neg_check k;
  prob_check p;
  if func = SAM then geometric_sam p
  else if Float.is_integer k then
    if func = PDF then geometric_pmf p (int_of_float k)
    else geometric_cdf p (int_of_float k)
  else
    let error_msg = "k value of Geometric distribution must be an integer" in
    raise (ComputationError.EvalError error_msg)

let eval_exponential func l x =
  neg_check x;
  lmbda_check l;
  if func = SAM then exponential_sam l
  else if func = PDF then exponential_pmf l x
  else (* func = CDF *) exponential_cdf l x

let eval_normal func m s x = 
  neg_check s;
  if func = SAM then normal_pmf m s x
  else if func = PDF then normal_sam m s
  else (* func = CDF *) normal_cdf m s x

(** [eval_prob dist sigma] is the result of evaluating the probability
    distribution [dist] in store [sigma]. *)
let eval_prob dist sigma = 
  let open Prob in
  let value = 
    match dist with
    | Binomial (func, n, p, k) -> eval_binomial func n p k
    | Bernoulli (func, p, k) -> eval_bernoulli func p k
    | Uniform (func, a, b, x) -> eval_uniform func a b x
    | Poisson (func, l, x) -> eval_poisson func l x
    | Geometric (func, p, k) -> eval_geometric func p k
    | Exponential (func, l, x) -> eval_exponential func l x
    | Normal (func, m, s, x) -> eval_normal func m s x
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

(** [eval_assign x v sigma] is the result of binding [v] to [x] in store
    [sigma]. *)
let eval_assign x v sigma =
  List.remove_assoc x sigma
  |> List.cons (x, v) 
  |> fun sigma' -> v, sigma'

let raise_exn error_msg = raise (ComputationError.(EvalError error_msg))

(** [eval_binop_on_floats op f1 f2 sigma] is the result of evaluating 
    [f1 op f2], where [op] is some binary operator and both [f1] and [f2] are
    floats, in store [sigma]. If [op] is [Assign], [Dot], or [SolveSys], then
    [EvalError] is raised. *)
let eval_binop_on_floats op f1 f2 sigma =
  let value = match op with
    | Add -> VFloat (f1 +. f2)
    | Sub -> VFloat (f1 -. f2)
    | Mul -> VFloat (f1 *. f2)
    | Div -> VFloat (f1 /. f2)
    | Mod -> VFloat (modulo f1 f2)
    | Pow -> VFloat(f1 ** f2)
    | Eq -> VFloat (Bool.to_float (f1 = f2))
    | LT -> VFloat (Bool.to_float (f1 < f2))
    | GT -> VFloat (Bool.to_float (f1 > f2))
    | LTE -> VFloat (Bool.to_float (f1 <= f2))
    | GTE -> VFloat (Bool.to_float (f1 >= f2))
    | Assign -> raise_exn ("Cannot assign to a float")
    | Dot -> raise_exn ("Cannot apply dot product to non-vectors")
    | SolveSys ->
      raise_exn ("Left arg must be a matrix and right arg must be a vector")
  in
  (value, sigma)

(** [eval_binop_on_vectors op v1 v2 sigma] is the result of evaluating 
    [v1 op v2], where [op] is some binary operator and both [v1] and [v2] are
    vectors, in store [sigma]. If [op] is [Assign], [Mod], or [SolveSys], then
    [EvalError] is raised. *)
let eval_binop_on_vectors op v1 v2 sigma =
  let open Vector in
  let result = match op with
    | Add -> VVector (component_wise_add v1 v2)
    | Sub -> VVector (component_wise_subtract v1 v2)
    | Mul -> VVector (component_wise_multiply v1 v2)
    | Dot -> VFloat (dot_product v1 v2)
    | Div -> VVector (RowVector (component_wise v1 v2 ( /. )))
    | Pow -> VVector (RowVector (component_wise v1 v2 ( ** )))
    | Eq -> VFloat (Bool.to_float (v1 = v2))
    | SolveSys -> raise_exn "Left arg must be a matrix"
    | _ -> raise_exn "Invalid operation between two vectors"
  in
  (result, sigma)

(** [eval_binop_on_matrices op m1 m2 sigma] is the result of evaluating 
    [m1 op m2], where [op] is some binary operator and both [m1] and [m2] are
    matrices, in store [sigma]. If [op] is not [Add], [Sub], [Mul], or [Eq],
    then [EvalError] is raised. *)
let eval_binop_on_matrices op m1 m2 sigma =
  let open Vector in
  let result = match op with
    | Add -> VMatrix (map2 component_wise_add m1 m2)
    | Sub -> VMatrix (map2 component_wise_subtract m1 m2)
    | Mul -> VMatrix (matrix_multiply m1 m2)
    | Eq -> VFloat (Bool.to_float (m1 = m2))
    | SolveSys -> raise_exn "Right arg must be a vector"
    | _ -> raise_exn "Invalid operation between two matrices"
  in
  (result, sigma)

(** [eval_binop_btwn_scalar_and_array op k arr sigma] is the result of evaluating 
    [k op arr], where [op] is some binary operator, [k] is a scalar, and [arr]
    is either a matrix or a vector, in store [sigma]. If [op] is not [Mul] or
    [Pow], then [EvalError] is raised. *)
let eval_binop_btwn_scalar_and_array op k arr sigma =
  let open Matrix in
  let open Vector in
  let multiply = fun x -> k *. x in
  let exponentiate = fun x -> Float.pow x k in
  let value = match arr, op with
    | VVector vec, Mul -> VVector (map multiply vec)
    | VVector vec, Pow -> VVector (map exponentiate vec)
    | VMatrix mat, Mul -> VMatrix (apply_to_all multiply mat)
    | VMatrix mat, Pow -> VMatrix (apply_to_all exponentiate mat)
    | _, _ -> raise_exn "Invalid operation between scalar and array"
  in
  (value, sigma)

(** [eval_binop_btwn_matrix_and_vector op arr1 arr2 sigma] is the result of
    evaluating [arr1 op arr2], where [op] is some binary operator and exactly
    one of [arr1] and [arr2] is a vector and exactly one is a matrix, in store
    [sigma]. If [op] is not [Mul] or [SolveSys], then [EvalError] is raised. *)
let eval_binop_btwn_matrix_and_vector op arr1 arr2 sigma =
  let open Matrix in
  let open Linalg in
  let value = match arr1, arr2, op with
    | VVector vec, VMatrix mat, Mul ->
      VVector (matrix_vector_product mat vec true)
    | VMatrix mat, VVector vec, Mul ->
      VVector (matrix_vector_product mat vec false)
    | VMatrix mat, VVector (ColVector _ as vec), SolveSys ->
      VVector (solve_system mat vec)
    | VMatrix mat, VVector (RowVector _), SolveSys ->
      raise_exn "Shape error: second argument should be a column vector"
    | _ -> raise_exn "Invalid operation between matrix and vector"
  in
  (value, sigma)

let rec eval_binop op e1 e2 sigma  =
  let (v1, sigma') = eval_expr e1 sigma in
  let (v2, sigma'') = eval_expr e2 sigma in
  match v1, v2 with
  | VFloat f1, VFloat f2 -> eval_binop_on_floats op f1 f2 sigma''
  | VMatrix m1, VMatrix m2 -> eval_binop_on_matrices op m1 m2 sigma 
  | VFloat f, (_ as arr)| (_ as arr), VFloat f ->
    eval_binop_btwn_scalar_and_array op f arr sigma
  | VMatrix _, VVector _ | VVector _, VMatrix _  -> 
    eval_binop_btwn_matrix_and_vector op v1 v2 sigma
  | VVector vec1, VVector vec2 -> eval_binop_on_vectors op vec1 vec2 sigma''
  | _ -> raise_exn "Invalid binary operation"

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
