open Ast

exception InvalidInput

(** [modulo p q] is p modulo q.
    Requires: [p] and [q] are floats representing integers. *)
let modulo p q = 
  assert (Float.is_integer p && Float.is_integer q);
  let p' = int_of_float p in
  let q' = int_of_float q in
  p' mod q' |> float_of_int 

let rec var_present = function
  | Binop (op, e1, e2) -> var_present e1 || var_present e2
  | Var x -> true
  | _ -> false

let rec eval ast = 
  match ast with
  | Var x -> failwith "Unimplemented"
  | Int i -> float_of_int i
  | Float f -> f
  | Binop (op, e1, e2) -> begin
      match op with
      | Add -> (eval e1) +. (eval e2)
      | Sub -> (eval e1) -. (eval e2)
      | Mul -> (eval e1) *. (eval e2)
      | Div -> (eval e1) /. (eval e2)
      | Mod -> 
        let p = eval e1 in
        let q = eval e2 in
        if Float.is_integer p && Float.is_integer q then modulo p q
        else raise InvalidInput
      | Pow -> Float.pow (eval e1) (eval e2)
      | Eq -> if var_present ast = true then failwith "Unimplemented"
        else failwith "Unimplemented"
      | LT -> failwith "Unimplemented"
      | GT -> failwith "Unimplemented"
      | LTE -> failwith "Unimplemented"
      | GTE -> failwith "Unimplemented"
    end
  | Vector _ -> failwith "Unimplemented"
  | Matrix _ -> failwith "Unimplemented"
  | Binomial (PDF, n, p, k) -> Prob.binomial_pmf n p k
  | Binomial (CDF, n, p, k) -> Prob.binomial_cdf n p k
  | Bernoulli (PDF, p, k) -> Prob.bernoulli_pmf p k
  | Bernoulli (CDF, p, k) -> Prob.bernoulli_cdf p k
  | Uniform (PDF, a, b, x) -> Prob.uniform_pmf a b x
  | Uniform (CDF, a, b, x) -> Prob.uniform_cdf a b x
  | Poisson (PDF, l, x) -> Prob.poisson_pmf l x
  | Poisson (CDF, l, x) -> Prob.poisson_cdf l x
  | Geometric (PDF, p, k) -> Prob.geometric_pmf p k
  | Geometric (CDF, p, k) -> Prob.geometric_cdf p k
  | Exponential (PDF, l, x) -> Prob.exponential_pmf l x
  | Exponential (CDF, l, x) -> Prob.exponential_cdf l x
  | Normal (PDF, m, s, x) -> Prob.normal_pmf m s x
  | Normal (CDF, m, s, x) -> Prob.normal_cdf m s x