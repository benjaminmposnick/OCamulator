open Vector

(** [t] is the type of [Matrix]. *)
type t

(** [to_array mat] reutrns a fresh array containing the elements of [Matrix]
    [mat]. *)
val to_array : t -> float array array
    
(** [of_array arr] returns a [Matrix] containing the elements of [arr]. *)
val of_array : float array array -> t

(** [of_list lst] returns a [Matrix] containing the elements of [lst]. *)
val of_list : float list list -> t

(** [zeros (n, m)] is a [Matrix] with [n] rows and [m] columns such
    that each entry is initialized to the float zero. *)
val zeros : int * int -> t

(** [identity n] is a [Matrix] with [n] rows and [n] columns such that the
    entries along the main diagonal are ones and all other entries are zero.
    E.g. | 1  0  0 |
        | 0  1  0 |
        | 0  0  1 | *)
val identity : int -> t

(** [n_rows mat] is the number of rows in [mat]. *)
val n_rows : t -> int

(** [n_cols mat] is the number of columns in [mat]. *)
val n_cols : t -> int

(** [is_square mat] is true iff [mat] has the same number of rows and
    columns. *)
val is_square : t -> bool

(** [transpose mat] is the transpose of [mat], obtained by interchanging the
    rows and columns of [mat], i.e. if A =def= [mat], then
    [transpose mat] = A^T.*)
val transpose : t -> t

(** [is_symmetric mat] is true iff [mat] is symmetric, i.e. A = A^T, if
    A =def= [mat].
    Requires: [m] is square *) 
val is_symmetric : t -> bool

(** [multiply m1 m2] is the result of performing matrix multiplication
    between matrices [m1] and [m2], i.e. if [A] =def= [m1], where [A] is
    [m X n], and [B] =def= [m2], where [B] is [n X p], then
    [matrix_multiply m1 m2] is [AB], where [AB] is is [m X p].
    Requires: The number of columns of [m1] must be equal to the number of row
    of [m2]. *)
val multiply : t -> t -> t

(** [get_row mat i] is the [i]th row of [mat]. *)
val get_row : t -> int -> float list

(** [get_col mat j] is the [j]th column of [mat]. *)
val get_col : t -> int -> float list

(** [drop_row mat i] is [mat] without row [i]. *)
val drop_row : t -> int -> t

(** [drop_col mat j] is [mat] without column [j]. *)
val drop_col : t -> int -> t

(** [row_sums mat] is list of the sums of all of the rows of [mat], in the same
    order they occur in [mat]. *)
val row_sums : t -> float list

(** [apply_to_all f mat] is [mat] after applying [f] to all elements in the
    matrix. *)
val apply_to_all : (float -> float) -> t -> t

val string_of_matrix : t -> string
