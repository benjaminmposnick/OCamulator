open Ast

exception InvalidBinop

let rec has_var e1 var = 
  if e1 = var then true
  else
    match e1 with
    | Binop (op', e1', e2') -> has_var e1' var or has_var e2' var
    | _ -> false

(** [match_inner_ast ast] is the tuple of contents of the Ast expression if it
    is a Binop expression and contains a variable.
    Otherwise, [match_ast ast] is the input ast *)
let match_inner_ast ast var = match ast with
  | Binop (op, e1, e2) -> if has_var e1 var or has_var e2 var
    then (op, e1, e2) else raise InvalidBinop
  | _ -> raise InvalidBinop

(** [match_ast ast] is the tuple of contents of the Ast expression if it is a
    Binop expression. Otherwise, [match_ast ast] is the input ast *)
let match_ast ast var = match ast with
  | Binop (op, e1, e2) -> (op, e2, e1)
  | _ -> raise InvalidBinop

(** [inverse_add outer_ast x_ast var] isolates [var] in [outer_ast] when it
    is nested in an addition operation contained in [x_ast]
    Example: 
    [inverse_add (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))*)
let inverse_add outer_ast x_ast var =
  let (op, e1, e2) = match_ast outer_ast var in
  let (left_op, left_e1, left_e2) = match_ast x_ast var in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Sub, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, left_e2, Binop(Sub, e2, left_e1))
  else failwith "No add operator given" 


(** [inverse_sub outer_ast x_ast var] isolates [var] in [outer_ast] when
    it is nested in an subtraction operation contained in [x_ast]
    Example: 
    [inverse_sub (Binop(Eq, Binop(Sub, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))*)
let inverse_sub outer_ast x_ast var =
  let (op, e1, e2) = match_ast outer_ast var in
  let (left_op, left_e1, left_e2) = match_ast x_ast var in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Add, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Sub, left_e1, e2), left_e2)
  else failwith "No subtraction operator given" 

(** [inverse_div outer_ast x_ast var] isolates [var] in [outer_ast] when it
    is nested in an division operation contained in [x_ast]
    Example: 
    [inverse_div (Binop(Eq, Binop(Div, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))*)
let inverse_div outer_ast x_ast var =
  let (op, e1, e2) = match_ast outer_ast var in
  let (left_op, left_e1, left_e2) = match_ast x_ast var in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Mul, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Div, left_e1, e2), left_e2)
  else failwith "No division operator given" 

(** [inverse_mul outer_ast x_ast var] isolates [var] in [outer_ast] when it
    is nested in an division operation contained in [x_ast]
    Example: 
    [inverse_mul (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))*)
let inverse_mul outer_ast x_ast var =
  let (op, e1, e2) = match_ast outer_ast var in
  let (left_op, left_e1, left_e2) = match_ast x_ast var in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Div, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Div, e2, left_e1), left_e2)
  else failwith "No division operator given" 

let inverse_helper (op, e_var, e_other) var =
  (* let (op, e_var, e_other) = match_ast e var in *)
  match e_var with 
  | Binop(Add, e1, e2) -> 
    let e_var', e_other' = if has_var e1 var then e1, e2 else e2, e1 in
    Binop(op, e_var', Binop(Sub, e_other, e_other'))
  | Binop(Sub, e1, e2) -> 
    if has_var e1 var 
    then Binop(op, e1, Binop(Add, e_other, e2))
    else Binop(op, e2, Binop(Sub, e1, e_other))
  | Binop(Mul, e1, e2) -> 
    let e_var', e_other' = if has_var e1 var then e1, e2 else e2, e1 in
    Binop(op, e_var', Binop(Div, e_other, e_other'))
  | Binop(Div, e1, e2) -> 
    if has_var e1 var 
    then Binop(op, e1, Binop(Mul, e_other, e2))
    else Binop(op, e2, Binop(Div, e1, e_other))

  (* | Var _ -> e
     | Int _ -> e *)
  | _ -> failwith "not or subtraction"

(** [step_solve var e] implements the primitive operation
    [FILL IN].  Requires: [e] and [var] are both Expr, and var is 
    specifically a Var. *)
let step_solve var e = 
  match e with
  | Binop (op, e1, e2) -> begin
      let e_var, e_other = if has_var e1 var then e1, e2 else e2, e1 in
      inverse_helper (op, e_var, e_other) var
    end
  | _ -> failwith "not binop"

let rec solve var e = 
  let (op, e1, e2) = match_ast e (Var var) in
  if e1 = Var var or e2 = Var var then e
  else if (has_var e1 (Var var) or  has_var e2 (Var var)) = false
  then failwith "No variable given"
  else e |> step_solve (Var var) |> solve var

(** TODO: specification *)
(* let inverse_old ast var = 
   let (rel_op, e1, e2) = match_ast ast var in
   let (bop, _, _), e = 
    try match_inner_ast e1 var, e1 with
    | InvalidBinop -> match_inner_ast e2 var, e2 in
   match bop with
   | Add -> inverse_add ast e"var"  (* pass [rel_op] in *)
   | Sub -> inverse_sub ast e"var"  (* pass [rel_op] in *)
   | Div -> inverse_div ast e"var"  (* pass [rel_op] in, negate if div by negative *)
   | Mul -> inverse_mul ast e var  (* pass [rel_op] in, negate if div by negative *)
   | _ -> failwith "Unimplimented equation type" *)