open Ast

(** [match_ast ast] is the tuple of contents of the Ast expression if it is a
    Binop expression. Otherwise, [match_ast ast] is the input ast *)
let match_ast ast = match ast with
  | Binop (op, e1, e2) -> (op, e1, e2)
  | _ -> failwith "Unimplimented"

(** [inverse_add outer_ast left_ast var] isolates [var] in [outer_ast] when it
    is nested in an addition operation contained in [left_ast]
    Example: 
    [inverse_add (Binop(Eq, Binop(Add, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))*)
let inverse_add outer_ast left_ast var =
  let (op, e1, e2) = match_ast outer_ast in
  let (left_op, left_e1, left_e2) = match_ast left_ast in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Sub, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, left_e2, Binop(Sub, e2, left_e1))
  else failwith "No add operator given"

(** [inverse_sub outer_ast left_ast var] isolates [var] in [outer_ast] when it
    is nested in an subtraction operation contained in [left_ast]
    Example: 
    [inverse_sub (Binop(Eq, Binop(Sub, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Sub, Int 5, Int 4))*)
let inverse_sub outer_ast left_ast var =
  let (op, e1, e2) = match_ast outer_ast in
  let (left_op, left_e1, left_e2) = match_ast left_ast in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Add, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Sub, left_e1, e2), left_e2)
  else failwith "No subtraction operator given"

(** [inverse_div outer_ast left_ast var] isolates [var] in [outer_ast] when it
    is nested in an division operation contained in [left_ast]
    Example: 
    [inverse_div (Binop(Eq, Binop(Div, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Mul, Int 5, Int 4))*)
let inverse_div outer_ast left_ast var =
  let (op, e1, e2) = match_ast outer_ast in
  let (left_op, left_e1, left_e2) = match_ast left_ast in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Mul, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Div, left_e1, e2), left_e2)
  else failwith "No division operator given"

(** [inverse_mul outer_ast left_ast var] isolates [var] in [outer_ast] when it
    is nested in an division operation contained in [left_ast]
    Example: 
    [inverse_mul (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 )) ("x"))] 
    is Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))*)
let inverse_mul outer_ast left_ast var =
  let (op, e1, e2) = match_ast outer_ast in
  let (left_op, left_e1, left_e2) = match_ast left_ast in
  if left_e1 = Var var then
    Binop(Eq, left_e1, Binop(Div, e2, left_e2))
  else if left_e2 = Var var then
    Binop(Eq, Binop(Div, e2, left_e1), left_e2)
  else failwith "No division operator given"

(** TODO: specification *)
let inverse ast var = 
  let (op, e1, e2) = match_ast ast in
  let (left_op, left_e1, left_e2) = match_ast e1 in
  match left_op with
  | Add -> inverse_add ast e1 var  
  | Sub -> inverse_sub ast e1 var
  | Div -> inverse_div ast e1 var
  | Mul -> inverse_mul ast e1 var
  | _ -> failwith "Unimplimented equation type"

