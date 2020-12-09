(** [transpose arr] is the tranpose of [arr].
    Requires: [arr] is a valid Array *)
val transpose : Ast.array -> Ast.array

(** [is_symmetric m] is true iff [m] is symmetric, i.e. A = A^T
    Requires: [m] is square *)    
val is_symmetric : float list list -> bool

(** [rref matrix] is the row-reduced echelon form of [matrix]. *)
val rref : float list list -> float list list

val matrix_multiply : float list list -> float list list -> float list list

val dot_product : float list -> float list  -> float

val component_wise_add : float list -> float list -> float list

val component_wise_subtract : float list -> float list -> float list

val component_wise_multiply : float list -> float list -> float list

val pivot_cols : float list list -> float list