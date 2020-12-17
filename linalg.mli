(** [transpose arr] is the transpose of [arr]. More specifically, if [arr] is...
    - a [RowVector], then [transpose arr] is a [ColVector]
    - a [ColVector], then [transpose arr] is a [RowVector]
    - a [Matrix], then [transpose arr] is a [Matrix] with the columns and rows
      of [arr] swapped.
    Requires: [arr] is a valid Array *)
val transpose : Ast.array -> Ast.array

(** [is_symmetric m] is true iff [m] is symmetric, i.e. A = A^T
    Requires: [m] is square *)    
val is_symmetric : float list list -> bool

(** [component_wise_add v1 v2] is the result of adding the elements of vectors
    [v1] and [v2] component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_add : float list -> float list -> float list

(** [component_wise_subtract v1 v2] is the result of subtracting [v2] from [v1]
    component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_subtract : float list -> float list -> float list

(** [component_wise_multiply v1 v2] is the result of multiplying the elements of
    vectors [v1] and [v2] component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_multiply : float list -> float list -> float list

(** [dot_product v1 v2] is the dot-product of [v1] and [v2], i.e. if [v1] = 
    [[v_11, v_12, ..., v_1n]] and [v2] = [[v_21, v_22, ..., v_2n]], then 
    [dot_product v1 v2] = [v_11 * v_21 + v_21 * v_22 + ... + v_1n * v_2n].
    Requires: [v1] and [v2] are the same length. *)
val dot_product : float list -> float list  -> float

(** [matrix_multiply m1 m2] is the result of performing matrix multiplication
    between matrices [m1] and [m2], i.e. if A = [m1], where A is m X n, and
    B = [m2], where B is n X p, then [matrix_multiply m1 m2] is AB, where AB is 
    is m X p.
    Requires: the number of columns of [m1] must be equal to the number of row
    of [m2]. *)
val matrix_multiply : float list list -> float list list -> float list list

(** [rref matrix] is the row-reduced echelon form of [matrix]. *)
val rref : float list list -> float list list

(** [pivot_cols matrix] is a list of the pivot columns of [matrix], found
    via Gaussian elimination. *)
val pivot_cols : float list list -> float list

val lu_decomposition : float list list -> (float list list * float list list)

val determinant : float list list -> float