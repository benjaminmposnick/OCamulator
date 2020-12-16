open Ast
open Prob
open Inverse

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

let rec eval_numeric e sigma =
  match e with
  | Var x -> begin
      match List.assoc x sigma with
      | Float f -> f
      | _ -> failwith "Cannot use non-float value in arithmetic operation"
    end
  | Int i -> float_of_int i
  | Float f -> f
  | Binop (op, e1, e2) -> begin
      match eval_numeric e1 sigma, eval_numeric e2 sigma with
      | e1', e2' ->
        match op with
        | Add -> e1' +. e2'
        | Sub -> e1' -. e2'
        | Mul -> e1' *. e2'
        | Div -> e1' /. e2'
        | Mod -> 
          if Float.(is_integer e1' && is_integer e2') then
            modulo e1' e2'
          else failwith "Invalid input"
        | Pow -> Float.pow e1' e2'
        | Eq ->
          if var_present e then failwith "Unimplemented"
          else Bool.to_float (e1' = e2')
        | LT ->
          if var_present e then failwith "Unimplemented"
          else Bool.to_float (e1' < e2')
        | GT ->
          if var_present e then failwith "Unimplemented"
          else Bool.to_float (e1' > e2')
        | LTE ->
          if var_present e then failwith "Unimplemented"
          else Bool.to_float (e1' <= e2')
        | GTE ->
          if var_present e then failwith "Unimplemented"
          else Bool.to_float (e1' >= e2')
        | Assign -> failwith "This case is handled elsewhere"
    end
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
  | _ -> failwith "No operation specified for this input"

let rec eval_array arr op sigma =
  let open Linalg in 
  match op with
  | "transpose" -> transpose arr
  | "rref" -> begin
      match arr with
      | Matrix mat -> Matrix (rref mat)
      | _ -> failwith "Cannot row reduce a vector"
    end
  | _ -> failwith "Operation is not available" 

let rec eval_input parsed_input sigma ans = 
  match parsed_input with
  | Command (c, e) -> begin
      let cmd = String.lowercase_ascii c in
      match e with 
      | NumArray arr -> (NumArray (eval_array arr cmd sigma), sigma)
      | Var x -> eval_input (Command (c, List.assoc x sigma)) sigma ans
      | Binop _ as eq -> (inverse c eq, sigma)
      | _ ->
        if cmd = "evaluate" then (Float (eval_numeric e sigma), sigma)
        else failwith "No operation specified for this input"
    end
  | Expression e ->
    match e with
    | Binop (Assign, e1, Ans) ->
      eval_input (Expression (Binop (Assign, e1, ans))) sigma ans
    | Binop (Assign, e1, e2) -> begin
        let x =
          match e1 with
          | Var x -> x
          | _ -> failwith "Cannot assign to an expression" in
        let e2' =
          match e2 with
          | NumArray arr ->  NumArray arr
          | _ -> Float (eval_numeric e2 sigma) in
        List.remove_assoc x sigma 
        |> (fun store -> (x, e2') :: store)
        |> (fun sigma' -> (e2', sigma'))
      end
    | Var x -> (List.assoc x sigma, sigma)
    | _ -> (Float (eval_numeric e sigma), sigma)

