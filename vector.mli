(** [t] is the type of [Vector]. *)
type t =
  | RowVector of float list
  | ColVector of float list

(** [make_row_vec lst] is a [RowVector] containing the elements of [lst]. *)
val make_row_vec : float list -> t

(** [make_col_vec lst] is a [ColVector] containing the elements of [lst]. *)
val make_col_vec : float list -> t

(** [transpose vec] is the transpose of [vec]. More specifically, if [vec] is
    a [RowVector], then [transpose arr] is a [ColVector]. If instead [vec] is
    a [ColVector], then [transpose arr] is a [RowVector] *)
val transpose : t -> t

(** [to_list vec] returns the list which holds the elements of [vec]. *)
val to_list : t -> float list

(** [to_array vec] returns a fresh array containing the elements of [vec]. *)
val to_array : t -> float array

(** [component_wise v1 v2 op] is the list resulting from applying the function
    [op] component-wise to the elements of vectors [v1] and [v2].
    Requires: [v1] and [v2] are the same length. *)
val component_wise : t -> t -> (float -> float -> float) -> float list

(** [component_wise_add v1 v2] is the result of adding the elements of vectors
    [v1] and [v2] component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_add : t -> t -> t

(** [component_wise_subtract v1 v2] is the result of subtracting [v2] from
    [v1] component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_subtract : t -> t -> t

(** [component_wise_multiply v1 v2] is the result of multiplying the elements
    of vectors [v1] and [v2] component-wise.
    Requires: [v1] and [v2] are the same length. *)
val component_wise_multiply : t -> t -> t

(** [dot_product v1 v2] is the dot-product of [v1] and [v2], i.e. if [v1]
    =def= [[v_11, v_12, ..., v_1n]] and [v2] =def= [[v_21, v_22, ..., v_2n]],
    then [dot_product v1 v2] = [v_11 * v_21 + v_21 * v_22 + ... + v_1n * v_2n].
    Requires: [v1] and [v2] are the same length. *)
val dot_product : t -> t -> float

(** [map fn vec] is [vec] after applying [fn] to every entry in [vec]. *)
val map : (float -> float) -> t -> t

(** [sum vec] is the sum of all the entries in [vec]. *)
val sum : t -> float

(** [string_of_vector_contents sep vec] is the string respresentation of
    [vec]. *)
val string_of_vector : t -> string

(** [size vec] is the number of entries in [vec]. *)
val size : t -> int

(** [of_array vec] returns a [Vector] containing the elements of array [arr]. *)
val of_array : float array -> t

(** [zeros n] returns a [Vectors] of size [n] with all entries initialized to
    zero. *)
val zeros : int -> t

(** [head vec] returns the first element in [vec]. *)
val head : t -> float

(** [head vec] returns a [RowVector] containing all the elements of [vec]
    except for the first element. *)
val tail : t -> t