open Ast
open Vector

(* ===========================================================================
    TYPE AND EXCEPTION DEFINITIONS
   ===========================================================================*)

type store = (string * value) list

type result = value * store

module ComputationError = struct
  exception EvalError of string
end

(* ===========================================================================
    GENERAL UTILITY FUNCTIONS
   ===========================================================================*)

(** [raise_exn msg] raises [ComputationError.EvalError] with [msg] as the
    error message. *)
let raise_exn msg =
  raise (ComputationError.EvalError msg)

(** [modulo p q] is [p] mod [q] if [p] and [q] are both floats representing
    integers (e.g. 1.0 for 1); otherwise, [ComputationError.EvalError] is
    raised. *)
let modulo p q =
  if not (Float.is_integer p && Float.is_integer q) then
    raise_exn "Cannot apply modulo operator to non-integer values"
  else
    let p' = int_of_float p in
    let q' = int_of_float q in
    p' mod q'
    |> float_of_int

(* ===========================================================================
    PROBABILITY AND STATISTICS UTILITY FUNCTIONS
   ===========================================================================*)

(** [prob_check p] is [unit] if [p] is a valid probability, i.e. [p] is between
    0 and 1, inclusive; otherwise, [ComputationError.EvalError] is
    raised.*)
let prob_check p = 
  if p >= 0. && p <= 1. then ()
  else raise_exn "p must be between 0 and 1 inclusive"

(** [nk_check n k] is [unit] if [k <= n]; otherwise,
    [ComputationError.EvalError] is raised.*)
let nk_check n k = 
  if k <= n then ()
  else raise_exn "k must be <= n"

(** [unif_check a b] is [unit] if [a < b]; otherwise,
    [ComputationError.EvalError] is raised.*)
let unif_check a b = 
  if a < b then ()
  else raise_exn "a must be < b"

(** [lmbda_check l] is [unit] if [l > 0]; otherwise,
    [ComputationError.EvalError] is raised. *)
let lmbda_check l = 
  if l > 0. then ()
  else raise_exn "Lambda must be > 0"

(** [neg_check x] is [unit] if [x >= 0]; otherwise, [ComputationError.EvalError]
    is raised. *)
let neg_check x =
  if x >= 0. then ()
  else raise_exn "Input value must be >= 0"

(** [bern_check x] is [unit] if [x] is either 0 or 1; otherwise,
    [ComputationError.EvalError] is raised. *)
let bern_check x = 
  if x = 0 || x = 1 then ()
  else raise_exn "Bernoulli RV's can only be 0 or 1"

let rand_vector dist i = 
  let open Prob in
  let open Vector in
  let rec rand_helper acc_list acc_i i f =
    if acc_i > i then acc_i
    else rand_helper (f :: acc_list) (acc_i + 1) i f
  in
  rand_helper [] 0 i dist

let stats_noargs_vec f vec = 
  let open Vector in
  let open Stat in
  let value = 
    vec 
    |> to_list
    |> f
    |> make_row_vec
  in
  VVector value

let stats_noargs_float f vec = 
  let open Vector in
  let open Stat in
  let value =
    vec 
    |> to_list
    |> f
  in VFloat value

(* ===========================================================================
   PROBABILITY AND STATISTICS EVALUATION
   ===========================================================================*)

let smpl_many smpler arg count = 
  let rec smpl_helper acc smpler arg count =
    if List.length acc = count then acc
    else smpl_helper ((smpler arg)::acc) smpler arg count
  in VVector (make_row_vec (smpl_helper [] smpler arg count))

let smpl_helper smpler arg k =
  if k >= 0 then 
    if k = 0 then VFloat (smpler arg)
    else smpl_many smpler arg k
  else raise_exn "Need positive k for sampling"

(** [eval_binomial func n p k] is result of evaluating [func] for the Binomial
    distribution with parameters [n], [p], and [k]. If the parameters are 
    invalid, then [ComputationError.EvalError] is raised. *)
let eval_binomial func n p k =
  let open Prob in
  prob_check p;
  neg_check k;
  if Float.is_integer n && Float.is_integer k then
    let n = int_of_float n in 
    let k = int_of_float k in
    if func = SAM then smpl_helper (binomial_sam n) p k
    else 
    if nk_check n k; func = PDF then 
      VFloat (binomial_pmf  n p k)
    else (* func = CDF *) 
      VFloat (binomial_cdf n p k)
  else raise_exn "n and k values of Binomial distribution must be ints"

(** [eval_bernoulli func p k] is result of evaluating [func] for the Bernoulli
    distribution with parameters [p] and [k]. If the parameters are invalid,
    then [ComputationError.EvalError] is raised. *)
let eval_bernoulli func p k =
  let open Prob in
  prob_check p;
  if Float.is_integer k then
    let k = int_of_float k in
    if func = SAM then smpl_helper bernoulli_sam p k
    else if bern_check k; func = PDF 
    then VFloat (bernoulli_pmf p k)
    else (* func = CDF *) VFloat (bernoulli_cdf p k)
  else raise_exn "k value of Bernoulli distribution must be an integer"

(** [eval_uniform func a b x] is result of evaluating [func] for the Uniform
    distribution with parameters [a], [b], and [x]. If the parameters are 
    invalid, then [ComputationError.EvalError] is raised. *)
let eval_uniform func a b x =
  let open Prob in
  unif_check a b;
  if func = SAM then 
    if Float.is_integer x then
      smpl_helper (uniform_sam a) b (int_of_float x)
    else raise_exn "x must be int for smpling"
  else if func = PDF then VFloat (uniform_pmf a b x)
  else (* func = CDF *) VFloat (uniform_cdf a b x)

(** [eval_poisson func l x] is result of evaluating [func] for the Poisson
    distribution with parameters [l] and [x]. If the parameters are invalid,
    then [ComputationError.EvalError] is raised. *)
let eval_poisson func l x =
  let open Prob in
  lmbda_check l;
  neg_check x;
  if func = SAM then 
    if Float.is_integer x then
      smpl_helper (poisson_sam l) 1. (int_of_float x)
    else raise_exn "x must be int for smpling"
  else if Float.is_integer x then
    if func = PDF then VFloat (poisson_pmf l (int_of_float x))
    else (* func = CDF *) VFloat (poisson_cdf l (int_of_float x))
  else raise_exn "x value of Poisson distribution must be an integer"

(** [eval_geometric func p k] is result of evaluating [func] for the Geometric
    distribution with parameters [p] and [k]. If the parameters are invalid,
    then [ComputationError.EvalError] is raised. *)
let eval_geometric func p k =
  let open Prob in
  neg_check k;
  prob_check p;
  if Float.is_integer k then
    let k = int_of_float k in
    if func = SAM then smpl_helper geometric_sam p k 
    else if func = PDF then VFloat (geometric_pmf p  k)
    else VFloat (geometric_cdf p k)
  else raise_exn "k value of Geometric distribution must be an integer"

(** [eval_exponential func l x] is result of evaluating [func] for the
    Exponential distribution with parameters [l] and [x]. If the parameters are
    invalid, then [ComputationError.EvalError] is raised. *)
let eval_exponential func l x =
  let open Prob in
  neg_check x;
  lmbda_check l;
  if func = SAM then 
    if Float.is_integer x then
      smpl_helper (exponential_sam) l (int_of_float x)
    else raise_exn "x must be int for smpling"
  else if func = PDF then VFloat (exponential_pmf l x)
  else (* func = CDF *) VFloat (exponential_cdf l x)

(** [eval_normal func m s x] is result of evaluating [func] for the Normal
    distribution with parameters [m], [s], and [x]. If the parameters are
    invalid, then [ComputationError.EvalError] is raised. *)
let eval_normal func m s x = 
  let open Prob in
  neg_check s;
  if func = SAM then 
    if Float.is_integer x then
      smpl_helper (normal_sam m) s (int_of_float x)
    else raise_exn "x must be int for smpling"
  else if func = PDF then VFloat (normal_pmf m s x)
  else (* func = CDF *) VFloat (normal_cdf m s x)

let eval_prob dist sigma = 
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
  value, sigma

(* ===========================================================================
    BINOP EVALUATION UTILITY FUNCTIONS
   ===========================================================================*)

(** [eval_var x sigma] is the value that is bound to [x] in store [sigma] if
    [x] is in [sigma]; otherwise, [ComputationError.EvalError] is raised. *)
let eval_var x sigma =
  match List.assoc_opt x sigma with
  | None -> raise_exn ("Variable " ^ x ^ " is undefined in current context")
  | Some value -> value, sigma

(** [eval_assign x v sigma] is the result of binding [v] to [x] in store
    [sigma]. If [x] is already in [sigma], its previous binding is removed
    before binding [v] to [x]. *)
let eval_assign x v sigma =
  List.remove_assoc x sigma
  |> List.cons (x, v) 
  |> fun sigma' -> v, sigma'

(** [eval_binop_on_floats op f1 f2 sigma] is the result of evaluating 
    [f1 op f2] in store [sigma], where [op] is some binary operator and both
    [f1] and [f2] are floats. If [op] is [Assign], [Dot], or [SolveSys], then
    [ComputationError.EvalError] is raised. *)
let eval_binop_on_floats op f1 f2 sigma =
  let value = match op with
    | Add -> f1 +. f2
    | Sub -> f1 -. f2
    | Mul -> f1 *. f2
    | Div -> f1 /. f2
    | Mod -> modulo f1 f2
    | Pow -> f1 ** f2
    | Eq -> Bool.to_float (f1 = f2)
    | LT -> Bool.to_float (f1 < f2)
    | GT -> Bool.to_float (f1 > f2)
    | LTE -> Bool.to_float (f1 <= f2)
    | GTE -> Bool.to_float (f1 >= f2)
    | SolveSys ->
      raise_exn ("Left arg to \\ must be a matrix, right arg must be a vector")
    | _ ->
      raise_exn ("Invalid operation between two floats: " ^ string_of_binop op)
  in
  (VFloat value, sigma)

(** [eval_binop_on_vectors op v1 v2 sigma] is the result of evaluating 
    [v1 op v2] in store [sigma], where [op] is some binary operator and both
    [v1] and [v2] are vectors. If [op] is [Assign], [Mod], or [SolveSys], then
    [ComputationError.EvalError] is raised. *)
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
    | _ ->
      raise_exn ("Invalid operation between two vectors: " ^ string_of_binop op)
  in
  (result, sigma)

(** [eval_binop_on_matrices op m1 m2 sigma] is the result of evaluating 
    [m1 op m2] in store [sigma], where [op] is some binary operator and both
    [m1] and [m2] are matrices. If [op] is not [Add], [Sub], [Mul], or [Eq],
    then [ComputationError.EvalError] is raised. *)
let eval_binop_on_matrices op m1 m2 sigma =
  let open Vector in
  let open Matrix in
  let result = match op with
    | Add -> VMatrix (map2 component_wise_add m1 m2)
    | Sub -> VMatrix (map2 component_wise_subtract m1 m2)
    | Mul -> VMatrix (matrix_multiply m1 m2)
    | Eq -> VFloat (Bool.to_float (m1 = m2))
    | SolveSys -> raise_exn "Right arg must be a vector"
    | _ ->
      raise_exn ("Invalid operation between two matrices: "
                 ^ string_of_binop op)
  in
  (result, sigma)

(** [eval_binop_on_scalar_and_array op k arr sigma] is the result of
    evaluating [k op arr] in store [sigma], where [op] is some binary operator,
    [k] is a scalar, and [arr] is either a matrix or a vector. If [op] is not
    [Mul] or [Pow], then [ComputationError.EvalError] is raised. *)
let eval_binop_on_scalar_and_array op k arr sigma =
  let open Matrix in
  let open Vector in
  let multiply = fun x -> k *. x in
  let exponentiate = fun x -> x ** k in
  let value = match arr, op with
    | VVector vec, Mul -> VVector (map multiply vec)
    | VVector vec, Pow -> VVector (map exponentiate vec)
    | VMatrix mat, Mul -> VMatrix (apply_to_all multiply mat)
    | VMatrix mat, Pow -> VMatrix (apply_to_all exponentiate mat)
    | _ -> raise_exn ("Invalid operation between scalar and array: "
                      ^ string_of_binop op)
  in
  (value, sigma)

(** [eval_binop_on_matrix_and_vector op arr1 arr2 sigma] is the result of
    evaluating [arr1 op arr2] in store [sigma], where [op] is some binary
    operator and exactly one of [arr1] and [arr2] is a vector and exactly one
    is a matrix. If [op] is not [Mul] or [SolveSys], then
    [ComputationError.EvalError] is raised. *)
let eval_binop_on_matrix_and_vector op arr1 arr2 sigma =
  let open Matrix in
  let open Linalg in
  let value = match arr1, arr2, op with
    | VVector vec, VMatrix mat, Mul -> matrix_vector_product mat vec true
    | VMatrix mat, VVector vec, Mul -> matrix_vector_product mat vec false
    | VMatrix mat, VVector (ColVector _ as cv), SolveSys -> solve_system mat cv
    | VMatrix mat, VVector (RowVector _), SolveSys ->
      raise_exn "Shape error: second argument to \\ should be a column vector"
    | _ -> raise_exn ("Invalid operation between matrix and vector: "
                      ^ string_of_binop op)
  in
  (VVector value, sigma)

(* ===========================================================================
   COMMAND EVALUATION UTILITY FUNCTIONS
   ===========================================================================*)

(** [eval_linalg_command cmd value] is the result of applying the linear
    algebra command [cmd] to [value]. If an error occurs during evaluation,
    [ComputationError.EvalError] is raised instead. *)
let eval_linalg_command cmd value =
  let open Linalg in
  match cmd, value with
  | "rref", VMatrix m -> VMatrix (rref m)
  | "rref", _ -> raise_exn "Cannot row reduce a non-matrix"
  | "transpose", VMatrix mat -> VMatrix (Matrix.transpose mat)
  | "transpose", VVector vec -> VVector (Vector.transpose vec)
  | "transpose", _ -> raise_exn "Cannot transpose a non-matrix / non-vector"
  | "pivots", VMatrix m -> VList (pivot_cols m)
  | "pivots", _ -> raise_exn "Cannot calculate pivots of a non-matrix"
  | "det", VMatrix mat -> VFloat (determinant mat)
  | "det", _ -> raise_exn "Cannot calculate determinant of a non-matrix"
  | "inv", VMatrix mat -> VMatrix (inverse mat)
  | "inv", _ -> raise_exn "Cannot calculate inverse of a non-matrix"
  | "plu",  VMatrix mat -> let (p, l, u, _) = plu_decomposition mat in
    VList [VMatrix p; VMatrix l; VMatrix u]
  | _ -> raise_exn ("No such command: " ^ cmd)

(** [eval_projection cmd lst] is the [i]th element (where one-indexing is
    employed) in [lst], where [i] is the integer represented by substring of
    [cmd] that excludes the first character. If an error occurs during
    evaluation, [ComputationError.EvalError] is raised instead.
    Requires: [cmd] is a string whose first character is '#'  *)
let eval_projection cmd lst =
  let substr = String.(sub cmd 1 (length cmd - 1)) in
  match int_of_string_opt substr with
  | None -> raise_exn "Cannot index list with non-integer"
  | Some idx -> begin
      match List.nth_opt lst (idx - 1) with
      | None -> raise_exn "Index out of bounds"
      | Some v -> v
    end

(** [eval_stat_command cmd value] is the result of applying the statistical
    command [cmd] to vector [vec]. If an error occurs during evaluation,
    [ComputationError.EvalError] is raised instead. *)
let eval_stat_command cmd vec =
  let open Stat in
  match cmd with
  | "mean"-> stats_noargs_float mean vec
  | "median" -> stats_noargs_float median vec
  | "sort_asc" -> stats_noargs_vec sort_asc vec
  | "sort_desc"-> stats_noargs_vec sort_desc vec
  | "unique"-> stats_noargs_vec unique vec
  | "min" -> stats_noargs_float min vec
  | "max" -> stats_noargs_float max vec
  | "variance" -> stats_noargs_float smpl_var vec
  | "std" -> stats_noargs_float smpl_std vec
  | "sum" -> stats_noargs_float cum_sum vec
  | "product" -> stats_noargs_float cum_prod vec
  | "mode" -> stats_noargs_float mode vec
  | "range" -> stats_noargs_float range vec
  | _ -> raise_exn ("No such command: " ^ cmd)

(** [dbl_int_command_nk f arg1 arg2] is the result of applying [f] to the
    pair [arg1] [arg2] if [arg1] and [arg2] if [arg2] <= [arg1].
    If an error occurs during evaluation, [ComputationError.EvalError] 
    is raised instead. *)
let dbl_int_cmd_nk f arg1 arg2 =
  if Float.is_integer arg1 && Float.is_integer arg2 then
    let arg1 = int_of_float arg1 in
    let arg2 = int_of_float arg2 in
    nk_check arg1 arg2;
    f arg1 arg2
  else raise_exn ("Both arguements must be integer")

let cmd_linreg x_vec y_vec =
  if List.length x_vec = List.length y_vec then 
    let points = List.combine x_vec y_vec in
    let mb = Stat.linear_regression points in
    VTuple (VFloat (fst mb), VFloat (snd mb))
  else raise_exn ("Must give the same number of x and y coords")

(** [eval_double_command cmd v1 v2] is the result of applying the statistical
    command [cmd] to the paris [v1] [v12]. If an error occurs during evaluation,
    [ComputationError.EvalError] is raised instead.*)
let eval_double_command cmd v1 v2 = 
  let open Stat in 
  let open Prob in
  let open Vector in
  let open Solve in
  match cmd, v1, v2 with
  | "choose", VFloat arg1, VFloat arg2 ->
    VFloat (dbl_int_cmd_nk choose arg1 arg2)
  | "comb", VFloat arg1, VFloat arg2 -> VFloat (dbl_int_cmd_nk choose arg1 arg2)
  | "perm", VFloat arg1, VFloat arg2 -> VFloat (dbl_int_cmd_nk perm arg1 arg2)
  | "count", VFloat arg, VVector vec -> VFloat (count arg (to_list vec))
  | "quantile", VFloat arg, VVector vec -> prob_check arg; 
    VFloat (quantile (to_list vec) arg)
  | "bestfit", VVector vec1, VVector vec2 ->
    cmd_linreg (to_list vec1) (to_list vec2)
  | "linreg", VVector vec1, VVector vec2 ->
    cmd_linreg (to_list vec1) (to_list vec2)
  | "gcd", VFloat arg1, VFloat arg2 -> 
    VFloat (float_of_int (gcd (int_of_float arg1) (int_of_float arg2)))
  | "lcm", VFloat arg1, VFloat arg2 -> 
    VFloat (float_of_int (lcm (int_of_float arg1) (int_of_float arg2)))
  | _ -> raise_exn ("No such command: " ^ cmd)

(* ===========================================================================
    EXPRESSION EVALUATION
   ===========================================================================*)

(** [eval_solve op e1 e2] is the result of solving the equation given by
    Binop (op, e1 ,e2) for some variable that is input by the user. If an error
    occurs during evaluation, [ComputationError.EvalError] is raised instead. *)
let rec eval_solve op e1 e2 sigma =
  let open Solve in
  print_endline "What variable would you like to solve for?";
  let input = read_line () in 
  let solve_output = solve input (Binop (op, e1, e2)) in
  match solve_output with
  | Binop (op', e1', e2') -> begin
      match e2' with
      | Binop (op'', e1'', e2'') -> if (Solve.has_var_any e2') = false 
          then fst (eval_expr e2' sigma)
          else VEquation (op', e1', e2')
      | _ -> VEquation (op', e1', e2')
    end
  | _ -> raise_exn "Error solving equation"

and eval_binop op e1 e2 sigma  =
  let (v1, sigma') = eval_expr e1 sigma in
  let (v2, sigma'') = eval_expr e2 sigma in
  match v1, v2 with
  | VFloat f1, VFloat f2 -> eval_binop_on_floats op f1 f2 sigma''
  | VMatrix m1, VMatrix m2 -> eval_binop_on_matrices op m1 m2 sigma 
  | VFloat f, (_ as arr)| (_ as arr), VFloat f ->
    eval_binop_on_scalar_and_array op f arr sigma
  | VMatrix _, VVector _ | VVector _, VMatrix _  -> 
    eval_binop_on_matrix_and_vector op v1 v2 sigma
  | VVector vec1, VVector vec2 -> eval_binop_on_vectors op vec1 vec2 sigma''
  | _ -> raise_exn "Invalid binary operation on given inputs"

(** [eval_command cmd e sigma] is the result of applying command [cmd] to
    the result of evaluating [e] in store [sigma]. If an error occurs during
    evaluation, [ComputationError.EvalError] is raised instead. *)
and eval_command cmd e sigma = 
  let stat_commands = ["mean"; "median"; "sort_asc"; "sort_desc"; "min"; "max";
                       "variance"; "std"; "sum"; "product";"mode";"range";
                       "unique"] in
  let linalg_commands = ["rref"; "transpose"; "pivots"; "det"; "inv"; "plu"] in
  let double_commands = ["choose";"perm";"comb";"count";"quantile";"bestfit";
                         "linreg";"lcm"; "gcd"] in
  let (value, sigma') =
    if cmd <> "solve" then eval_expr e sigma
    else
      match e with
      | Binop (op, e1, e2) -> (VEquation (op, e1, e2)), sigma
      | _ -> raise_exn "Invalid operation on a non-equation" in
  let result = match cmd, value with
    | "solve", VEquation (op, e1, e2) -> eval_solve op e1 e2 sigma
    | _, VList lst when String.(length cmd > 0 && get cmd 0 = '#') ->
      eval_projection cmd lst
    | stat_cmd, VVector vec when List.mem stat_cmd stat_commands -> 
      eval_stat_command stat_cmd vec
    | linalg_cmd, _ when List.mem linalg_cmd linalg_commands ->
      eval_linalg_command linalg_cmd value
    | dbl_cmd, VTuple (v1,v2) when List.mem dbl_cmd double_commands ->
      eval_double_command dbl_cmd v1 v2
    | "fac", VFloat i when Float.is_integer i -> 
      VFloat(i |> int_of_float |> Prob.factorial |> float_of_int )
    | _ -> raise_exn ("No such command: " ^ cmd)
  in
  (result, sigma')

(** [eval_tup e1 e2 sigma] is the tuple [(v1, v2)] that results from evaluating
    [e1] to a value [v1] and [e2] to a value [v2] in store [sigma]. *)
and eval_tup e1 e2 sigma =
  let val1 = eval_expr e1 sigma in
  let val2 = eval_expr e2 (snd val1)in
  VTuple (fst val1, fst val2), snd val2

and eval_expr e sigma =
  match e with
  | Var x -> eval_var x sigma
  | Int i -> VFloat (float_of_int i), sigma
  | Float f -> VFloat f, sigma
  | Prob dist -> eval_prob dist sigma
  | Matrix mat ->  VMatrix mat, sigma
  | Vector vec -> VVector vec, sigma
  | Command (cmd, e) -> 
    let cmd' = String.lowercase_ascii cmd in
    eval_command cmd' e sigma
  | Tuple (e1,e2) -> eval_tup e1 e2 sigma
  | Binop (Assign, Var x, e) -> 
    let (v, sigma') = eval_expr e sigma in 
    eval_assign x v sigma'
  | Binop (op, e1, e2) -> eval_binop op e1 e2 sigma

let rec eval_input e sigma = 
  let (value, sigma') = eval_expr e sigma in
  eval_assign "ans" value sigma'
