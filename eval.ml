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
      (** Benny P *)
      | Mod -> 
        let p = eval e1 in
        let q = eval e2 in
        if Float.is_integer p && Float.is_integer q then modulo p q
        else raise InvalidInput
      | Pow -> failwith "Unimplemented"
      (** Matty Light *)
      | Eq -> if var_present ast then failwith "Unimplemented"
        else (eval e1) = (eval e2) |> Bool.to_float
      | LT -> if var_present ast then failwith "Unimplemented"
        else (eval e1) < (eval e2) |> Bool.to_float
      | GT -> if var_present ast then failwith "Unimplemented"
        else (eval e1) > (eval e2) |> Bool.to_float
      | LTE -> if var_present ast then failwith "Unimplemented"
        else (eval e1) <= (eval e2) |> Bool.to_float
      | GTE -> if var_present ast then failwith "Unimplemented"
        else (eval e1) >= (eval e2) |> Bool.to_float
    end

