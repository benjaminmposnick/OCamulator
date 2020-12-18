module Vector = struct

  type t =
    | RowVector of float list
    | ColVector of float list

  (** [make_row_vec lst] is a [RowVector] containing the elements of [lst]. *)
  let make_row_vec lst = RowVector lst

  (** [make_col_vec lst] is a [ColVector] containing the elements of [lst]. *)
  let make_col_vec lst = ColVector lst

  (** [transpose vec] is the transpose of [vec]. More specifically, if [vec] is
      a [RowVector], then [transpose arr] is a [ColVector]. If instead [vec] is
      a [ColVector], then [transpose arr] is a [RowVector] *)
  let transpose = function
    | RowVector vec -> ColVector vec
    | ColVector vec -> RowVector vec

  (** [to_list vec] returns the list which holds the elements of [vec]. *)
  let to_list = function
    | RowVector vec -> vec
    | ColVector vec -> vec

  (** [to_list vec] returns a fresh array containing the elements of [vec]. *)
  let to_array = function
    | RowVector vec -> Array.of_list vec
    | ColVector vec -> Array.of_list vec

  (** [component_wise v1 v2 op] is the result of applying the function [op]
      component-wise to the elements of vectors [v1] and [v2].
      Requires: [v1] and [v2] are the same length. *)
  let component_wise v1 v2 op =
    try RowVector (List.map2 op (to_list v1) (to_list v2)) with
    | Invalid_argument _ -> failwith "Vectors must be the same length"

  (** [component_wise_add v1 v2] is the result of adding the elements of vectors
      [v1] and [v2] component-wise.
      Requires: [v1] and [v2] are the same length. *)
  let component_wise_add v1 v2 =
    component_wise v1 v2 ( +. )

  (** [component_wise_subtract v1 v2] is the result of subtracting [v2] from
      [v1] component-wise.
      Requires: [v1] and [v2] are the same length. *)
  let component_wise_subtract v1 v2 =
    component_wise v1 v2 ( -. )

  (** [component_wise_multiply v1 v2] is the result of multiplying the elements
      of vectors [v1] and [v2] component-wise.
      Requires: [v1] and [v2] are the same length. *)
  let component_wise_multiply v1 v2 =
    component_wise v1 v2 ( *. )

  (** [dot_product v1 v2] is the dot-product of [v1] and [v2], i.e. if [v1]
      =def= [[v_11, v_12, ..., v_1n]] and [v2] =def= [[v_21, v_22, ..., v_2n]],
      then [dot_product v1 v2] = [v_11 * v_21 + v_21 * v_22 + ... + v_1n * v_2n].
      Requires: [v1] and [v2] are the same length. *)
  let dot_product v1 v2 =
    component_wise_multiply v1 v2
    |> to_list
    |> List.fold_left ( +. ) 0.

  let map f = function
    | RowVector vec -> RowVector (List.map f vec)
    | ColVector vec -> ColVector (List.map f vec)

  (** [sum vec] is the sum of all the entries in [vec]. *)
  let sum vec = 
    to_list vec |> List.fold_left ( +. ) 0.

  (** [string_of_vector_contents sep vec] is the string respresentation of
      [vec]. *)
  let string_of_vector vec = 
    let string_of_vector_aux sep vec =
      List.map string_of_float vec 
      |> String.concat sep
      |> (fun str -> "[" ^ str ^ "]") in
    match vec with
    | RowVector vec -> string_of_vector_aux "," vec
    | ColVector vec -> string_of_vector_aux ";" vec

end