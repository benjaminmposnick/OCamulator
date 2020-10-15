open Ast

let rec eval = function
  | Var x -> failwith "Unimplemented"
  | Int i -> float_of_int i
  | Float f -> f
  | Binop (op, e1, e2) -> begin
      match op with
      | Add -> (eval e1) +. (eval e2)
      | Sub -> (eval e1) -. (eval e2)
      | Mul -> (eval e1) *. (eval e2)
      | Div -> (eval e1) /. (eval e2)
      | Mod -> failwith "Unimplemented"
      | Pow -> failwith "Unimplemented"
      | Eq -> failwith "Unimplemented"
      | LT -> failwith "Unimplemented"
      | GT -> failwith "Unimplemented"
      | LTE -> failwith "Unimplemented"
      | GTE -> failwith "Unimplemented"
    end
