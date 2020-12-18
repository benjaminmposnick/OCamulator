open Matrix

(** [rref matrix] is the row-reduced echelon form of [matrix]. *)
val rref : Matrix.t -> Matrix.t 

(** [pivot_cols matrix] is a list of the pivot columns of [matrix], found
    via Gaussian elimination. *)
val pivot_cols : Matrix.t  -> int list

val plu_decomposition : Matrix.t  -> (Matrix.t * Matrix.t * Matrix.t)
