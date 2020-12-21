(** [Matrix] is the module for creating and manipulating matrices. *)

open Vector

(** [t] is the type of [Matrix]. 
    Representation invariants:
    - A [Matrix] represented as a list of [RowVector]s
    - All [RowVectors] which comprise a [Matrix] are the same length. *)
type t

(** [to_list mat] returns a 2-dimensional list containing the elements of
    [Matrix] [mat]. *)
val to_list : t -> float list list

(** [to_array mat] returns a fresh 2-dimensional array containing the elements
    of [Matrix] [mat]. *)
val to_array : t -> float array array
    
(** [of_list lst] returns a [Matrix] containing the elements of the 
    2-dimensional list [lst]. *)
val of_list : float list list -> t

(** [of_array arr] returns a [Matrix] containing the elements of the 
    2-dimensional array [arr]. *)
val of_array : float array array -> t

(** [make n m init] returns a [Matrix] that has [n] rows and [m] columns, where
    all entries are initialized to [init]. *)
val make : int -> int -> float -> t

(** [zeros (n, m)] is a [Matrix] with [n] rows and [m] columns such
    that each entry is initialized to the float zero. If [n] is not provided,
    then the resulting matrix will be [m] X [m]. *)
val zeros : ?n:int -> int -> t

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

(** [string_of_matrix mat] is the string representation of matrix [mat]. *)
val string_of_matrix : t -> string

(** [transpose mat] is the transpose of [mat], obtained by interchanging the
    rows and columns of [mat], i.e. if A =def= [mat], then
    [transpose mat] = A^T. *)
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
val matrix_multiply : t -> t -> t

(** [get_row mat i] is the [i]th row of [mat] as a list. *)
val get_row : t -> int -> float list

(** [get_col mat j] is the [j]th column of [mat] as a list. *)
val get_col : t -> int -> float list

(** [drop_row mat i] is [mat] without row [i]. *)
val drop_row : t -> int -> t

(** [drop_col mat j] is [mat] without column [j]. *)
val drop_col : t -> int -> t

(** [apply_to_all f mat] is [mat] after applying [f] to all elements in the
    matrix. *)
val apply_to_all : (float -> float) -> t -> t

(** [of_vectors vec_lst] returns a [Matrix] containing the elements of the 
    vectors in [vec_lst].
    Requires: [vec_lst] is non-empty. *)
val of_vectors : Vector.t list -> t

(** [is_upper_triangular mat] is true iff [mat] is upper-triangular, i.e. all
    entries below the main diagonal are zero.
    Requires: [mat] is square. *)
val is_upper_triangular : t -> bool

(** [is_lower_triangular mat] is true iff [mat] is lower-triangular, i.e. all
    entries above the main diagonal are zero.
    Requires: [mat] is square. *)
val is_lower_triangular : t -> bool

(** [map fn mat] is [mat] after applying [fn] to every row vector in [mat]. *)
val map : (Vector.t -> Vector.t) -> t -> t

(** [map2 fn mat1 mat2] is the result from applying [fn] to all of the
    corresponding row vectors in [mat1] and [mat2]. *)
val map2 : (Vector.t -> Vector.t -> Vector.t) -> t -> t -> t

(** [row_sums mat] is list of the sums of all of the rows of [mat], in the same
    order they occur in [mat]. *)
val row_sums : t -> float list

(** [matrix_vector_product mat vec swap_order] is the one of the following: 
    Suppose that [A] =def= [mat] and [b] =def= [vec]. Then the result is...
    - [Ab] if [swap_order] is false and the number of columns of [A] equals the
      number of rows of [b].
    - [bA] if [swap_order] is true and the number of columns of [b] equals the
      number of rows of [A].
    If a shape error occurs, then [Failure] is raised.
    Requires: The number of columns of [m1] must be equal to the number of row
    of [m2]. *)
val matrix_vector_product : t -> Vector.t -> bool -> Vector.t