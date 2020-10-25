open Ast

(** [transpose arr] is the tranpose of [arr].
    Requires: [arr] is a valid Array *)
let transpose = function
  | RowVector vec -> NumArray (ColumnVector vec)
  | ColumnVector vec -> NumArray (RowVector vec)
  | Matrix mat -> 
    let rec transpose_matrix acc lst = 
      match lst with
      | [] -> []
      | h::t ->
        if List.length h = 0 then List.rev acc
        else 
          let next_row = List.map List.hd lst in
          let submatrix = List.map List.tl lst in
          transpose_matrix (next_row::acc) submatrix in
    NumArray (Matrix (transpose_matrix [] mat))
