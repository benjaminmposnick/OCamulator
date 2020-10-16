open Ast

exception InvalidInput

(** [modulo p q] is p modulo q.
    Requires: [p] and [q] are*)
let modulo p q = 
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
      | Eq -> if var_present ast = true then failwith "Unimplemented"
        else failwith "Unimplemented"
      | LT -> failwith "Unimplemented"
      | GT -> failwith "Unimplemented"
      | LTE -> failwith "Unimplemented"
      | GTE -> failwith "Unimplemented"
    end

