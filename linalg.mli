open Matrix
open Vector

(** [rref matrix] is the row-reduced echelon form of [matrix]. *)
val rref : Matrix.t -> Matrix.t 

(** [pivot_cols matrix] is a list of the pivot columns of [matrix], found
    via Gaussian elimination. *)
val pivot_cols : Matrix.t  -> Ast.value list

val plu_decomposition : ?no_round:bool -> Matrix.t  -> (Matrix.t * Matrix.t * Matrix.t * int)

val determinant : Matrix.t -> float

val inverse : Matrix.t -> Matrix.t

val solve_system : Matrix.t -> Vector.t -> Vector.t
