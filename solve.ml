open Ast

exception InvalidBinop

let rec has_var e1 var = 
  if e1 = var then true
  else
    match e1 with
    | Binop (op', e1', e2') -> has_var e1' var || has_var e2' var
    | _ -> false

(** [match_ast ast] is the tuple of contents of the Ast expression if it is a
    Binop expression. Otherwise, [match_ast ast] is the input ast *)
let match_ast ast var = match ast with
  | Binop (op, e1, e2) -> (op, e2, e1)
  | _ -> raise InvalidBinop

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
  if e1 = Var var || e2 = Var var then e
  else if (has_var e1 (Var var) ||  has_var e2 (Var var)) = false
  then failwith "No variable given"
  else e |> step_solve (Var var) |> solve var

let rec gcd v1 v2 = 
  let i1 = max v1 v2 in
  let i2 = min v1 v2 in
  if i1 = 0 then i2
  else if i2 = 0 then i1 else
  let remainder = i1 mod i2 in
  gcd i2 remainder

let lcm v1 v2 = 
  if v1 = 0 or v2 = 0 then failwith "LCM of zero does not exist" else
  let prod = Int.abs (v1 * v2) in
  let divisor = gcd v1 v2 in
  prod / divisor