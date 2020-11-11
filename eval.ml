open Ast

let parse str =
  let lexbuf = Lexing.from_string str in
  Parser.prog Lexer.read lexbuf

let modulo p q = 
  assert (Float.is_integer p && Float.is_integer q);
  let p' = int_of_float p in
  let q' = int_of_float q in
  p' mod q' |> float_of_int 

let rec var_present = function
  | Binop (op, e1, e2) -> var_present e1 || var_present e2
  | Var x -> true
  | _ -> false

let rec eval_arith e =
  match e with
  | Var x -> failwith "Unimplemented"
  | Int i -> float_of_int i
  | Float f -> f
  | Binop (op, e1, e2) -> begin
      match op with
      | Add -> (eval_arith e1) +. (eval_arith e2)
      | Sub -> (eval_arith e1) -. (eval_arith e2)
      | Mul -> (eval_arith e1) *. (eval_arith e2)
      | Div -> (eval_arith e1) /. (eval_arith e2)
      | Mod -> 
        let p = eval_arith e1 in
        let q = eval_arith e2 in
        if Float.is_integer p && Float.is_integer q then modulo p q
        else failwith "Invalid input"
      | Pow -> Float.pow (eval_arith e1) (eval_arith e2)
      | Eq -> if var_present e then failwith "Unimplemented"
        else (eval_arith e1) = (eval_arith e2) |> Bool.to_float
      | LT -> if var_present e then failwith "Unimplemented"
        else (eval_arith e1) < (eval_arith e2) |> Bool.to_float
      | GT -> if var_present e then failwith "Unimplemented"
        else (eval_arith e1) > (eval_arith e2) |> Bool.to_float
      | LTE -> if var_present e then failwith "Unimplemented"
        else (eval_arith e1) <= (eval_arith e2) |> Bool.to_float
      | GTE -> if var_present e then failwith "Unimplemented"
        else (eval_arith e1) >= (eval_arith e2) |> Bool.to_float
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
  | _ -> failwith "No operation specified for this input"


let eval parsed_input = 
  match parsed_input with
  | Command (c, e) -> begin
      let cmd = String.lowercase_ascii c in
      match e with 
      | NumArray arr -> begin
          match cmd with
          | "transpose" -> NumArray (Linalg.transpose arr)
          | "rref" -> begin
              match arr with
              | Matrix mat -> NumArray (Linalg.rref mat)
              | _ -> failwith "Cannot row reduce a vector"
            end
          | _ -> failwith "Unimplemented" 
        end
      | _ ->
        if cmd = "evaluate" then Float (eval_arith e)
        else failwith "No operation specified for this input"
    end
  | Expression e -> Float (eval_arith e)

