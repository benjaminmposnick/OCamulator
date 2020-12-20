open Matrix
open Vector

(** [rref matrix] is the row-reduced echelon form of [matrix]. *)
val rref : Matrix.t -> Matrix.t 

(** [pivot_cols matrix] is a list of the pivot columns of [matrix], found
    via Gaussian elimination. *)
val pivot_cols : Matrix.t  -> Ast.value list

(** [plu_decomposition no_round matrix] is the PLU decomposition of [matrix],
    i.e. the triple of matrices [P], [L], and [U] such that if [A] = [matrix],
    then [A = (P^T)LU] where [P] is the permutation matrix, [L] is lower
    triangular, and [U] is upper triangular. Similar to the LU decomposition,
    except the permutation matrix keeps track of row interchanges which are
    required for numerical stability. If [no_round] is true, then the values in
    [P], [L], and [U] are not rounded.
    Requires: [matrix] is square. *)
val plu_decomposition : ?no_round:bool -> Matrix.t  ->
                        (Matrix.t * Matrix.t * Matrix.t * int)

val determinant : Matrix.t -> float

val inverse : Matrix.t -> Matrix.t

val solve_system : Matrix.t -> Vector.t -> Vector.t
