open Ast
open Eval

let isolate_addition_relation expr var =
  match expr with
  | Binop (op, lt, rt) -> begin
      match op with
      | (Eq | LT | GT | LTE | GTE) as relation_op -> begin
          (* For simplicity here just considering single instance of variable 
             on left side, e.g. solve for x in 4 + x = 5 *)
          match lt with 
          | Binop (Add, lt_e1, lt_e2) -> begin
              if lt_e1 = (Var var) then 
                Binop (relation_op, lt_e2, Binop(Add, rt, lt_e2))
              else if lt_e2 = (Var var) then
                Binop (relation_op, lt_e1, Binop(Add, rt, lt_e1))
              else failwith "Not relevant to this example"
            end
          | _ -> failwith "Not relevant to this example"
        end
      | _ -> failwith "Not a relation"
    end
  | _ -> failwith "Not a binary operation"
