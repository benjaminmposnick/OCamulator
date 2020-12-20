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

(** [determinant mat] is the determinant of [mat].
    Requires: [matrix] is square. *)
val determinant : Matrix.t -> float

(** [inverse mat] is the inverse of [mat], i.e if [A] =def= [mat], then 
    [inverse mat] = [A^(-1)].
    Requires: [matrix] is square. *)
val inverse : Matrix.t -> Matrix.t

(** [solve_system a b] is the vector [x] that results from solving the 
    system of equations given by [Ax = b]. If [a] is non-singular, then 
    [Failure] is raised because there either exists no solution (i.e. an
    inconsistent system) or there exist infinitely many solutions.
    Requires: [a] is square.
    Note: The requirement that [a] is square comes from the fact that this 
    implementation utilizes the PLU factorization, which also requires that [a]
    is square. Solving [Ax = b] for general [A] requires a different
    factorization. *)
val solve_system : Matrix.t -> Vector.t -> Vector.t
