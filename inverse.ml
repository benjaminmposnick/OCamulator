open Ast

exception NotABinop

(** [match_ast ast] is the tuple of contents of the Ast expression if it is a
    Binop expression. Otherwise, [match_ast ast] is the input ast *)
let match_ast ast = match ast with
  | Binop (op, e1, e2) -> (op, e1, e2)
  | _ -> raise NotABinop

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
  (* else if e1 = Var var or e2 = Var var then *)
    (* Binop(Eq, outer_ast, ) *)
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
  let (rel_op, e1, e2) = match_ast ast in
  let (bop, _, _), e = 
    try match_ast e1, e1 with
    | NotABinop -> match_ast e2, e2 in
  match bop with
  | Add -> inverse_add ast e var  (* pass [rel_op] in *)
  | Sub -> inverse_sub ast e var  (* pass [rel_op] in *)
  | Div -> inverse_div ast e var  (* pass [rel_op] in, negate if div by negative *)
  | Mul -> inverse_mul ast e var  (* pass [rel_op] in, negate if div by negative *)
  | _ -> failwith "Unimplimented equation type"
