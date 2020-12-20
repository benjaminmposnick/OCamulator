type t =
  | RowVector of float list
  | ColVector of float list

let make_row_vec lst = 
  RowVector lst

let make_col_vec lst = 
  ColVector lst

let transpose = function
  | RowVector vec -> ColVector vec
  | ColVector vec -> RowVector vec

let to_list = function
  | RowVector vec -> vec
  | ColVector vec -> vec

let to_array = function
  | RowVector vec -> Array.of_list vec
  | ColVector vec -> Array.of_list vec

let component_wise v1 v2 op =
  try List.map2 op (to_list v1) (to_list v2) with
  | Invalid_argument _ -> failwith "Vectors must be the same length"

let component_wise_add v1 v2 =
  RowVector (component_wise v1 v2 ( +. ))

let component_wise_subtract v1 v2 =
  RowVector (component_wise v1 v2 ( -. ))

let component_wise_multiply v1 v2 =
  RowVector (component_wise v1 v2 ( *. ))

let dot_product v1 v2 =
  component_wise_multiply v1 v2
  |> to_list
  |> List.fold_left ( +. ) 0.

let map f = function
  | RowVector vec -> RowVector (List.map f vec)
  | ColVector vec -> ColVector (List.map f vec)

let sum vec = 
  to_list vec
  |> List.fold_left ( +. ) 0.

let string_of_vector vec = 
  let string_of_vector_aux sep vec =
    List.map string_of_float vec 
    |> String.concat sep
    |> (fun str -> "[" ^ str ^ "]") in
  match vec with
  | RowVector vec -> string_of_vector_aux ", " vec
  | ColVector vec -> string_of_vector_aux "; " vec

let size vec =
  to_list vec
  |> List.length

let of_array arr =
  ColVector (Array.to_list arr)

let zeros n =
  Array.make n 0.
  |> of_array

let head vec =
  to_list vec
  |> List.hd

let tail vec =
  to_list vec
  |> List.tl
  |> make_row_vec