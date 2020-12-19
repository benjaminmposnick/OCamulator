open Vector

type t = Vector.t list

let to_array mat =
  List.map Vector.to_array mat |> Array.of_list

let to_list mat =
  List.map Vector.to_list mat

let of_list lst = 
  List.map Vector.make_row_vec lst

let of_array arr = 
  Array.(map to_list arr)
  |> Array.to_list
  |> of_list

let make n m init : t =
  Array.make_matrix n m init |> of_array

let zeros ?(n=(~-1)) m =
  if n = ~-1 then make m m 0.
  else make n m 0.

let identity n =
  let arr = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    arr.(i).(i) <- 1.
  done;
  of_array arr

let n_rows mat = 
  List.length mat

let n_cols mat = 
  if List.length mat = 0 then 0
  else List.hd mat |> Vector.size 

let is_square mat =
  n_cols mat = n_rows mat

(** [string_of_matrix_row max_digits row] is the string representation of
    [row] such that each entry in [row] is given [max_digits] number of 
    digits. If a vector entry takes up [d] digits in a string, then there are
    [max_digits] - [d] whitespaces to ensure all columns are of equal width. *)
let string_of_matrix_row max_digits row =
  let string_of_entry e = 
    let float_str = string_of_float e in
    let n_spaces = max_digits - (String.length float_str) in
    float_str ^ (String.make n_spaces ' ')
  in
  List.map string_of_entry row |> String.concat " "

let string_of_matrix mat =
  let max_digits =
    let string_list = List.map (fun row -> List.map string_of_float (Vector.to_list row)) mat in
    let length_max s1 s2 =
      if String.(length s1 >= length s2) then s1 else s2 in
    let max_string_by_row =
      List.(map (fun lst -> fold_left length_max "" lst) string_list) in
    let max_string = List.fold_left length_max "" max_string_by_row in
    String.length max_string
  in
  to_list mat
  |> List.map (fun vec -> "| " ^ string_of_matrix_row max_digits vec)
  |> String.concat " |\n"
  |> fun str -> str ^ " |" 

let transpose mat : t =
  let rec transpose_aux (lst : t) acc =
    match lst with
    | [] -> []
    | h :: t ->
      if Vector.size h = 0 then List.rev acc |> of_list
      else 
        let next_row = List.map Vector.head lst in
        let submatrix = List.map Vector.tail lst in
        transpose_aux submatrix (next_row :: acc)
  in
  transpose_aux mat []

let is_symmetric mat =
  assert (is_square mat);
  mat = (transpose mat)

let matrix_multiply (m1 : t) (m2 : t) = 
  assert (n_cols m1 = n_rows m2);
  let open List in
  let m2_t = transpose m2 in
  let row_fn row = List.map (Vector.dot_product row) m2_t in
  List.map row_fn m1
  |> of_list

let get_row (mat : t) i =
  List.nth mat i |> Vector.to_list

let get_col mat j = 
  transpose mat
  |> (fun mat_t -> get_row mat_t j)

let drop_row mat i = 
  let rec drop_row_aux acc idx = function
    | [] -> List.rev acc
    | h :: t ->
      if i = idx then (List.rev acc) @ t
      else drop_row_aux (h :: acc) (idx + 1) t
  in
  drop_row_aux [] 0 mat

let drop_col mat j =
  transpose mat
  |> (fun mat_t -> drop_row mat_t j)
  |> transpose

let row_sums = List.map Vector.sum 

let apply_to_all f matrix = 
  List.map (fun row -> Vector.(map f row)) matrix

let is_upper_triangular matrix =
  assert (is_square matrix);
  let n = n_rows matrix in
  let a = to_array matrix in
  let elems = ref [] in
  for j = 0 to n - 1 do
    for i = j + 1 to n - 1 do
      elems := a.(i).(j) :: !elems
    done;
  done;
  List.fold_left (fun acc x -> if x = 0. then acc else false) true !elems

let is_lower_triangular matrix =
  transpose matrix |> is_upper_triangular

let of_vectors vec_lst =
  let rec vectors_to_lists lst acc =
    match lst with
    | [] -> List.rev acc
    | h :: t -> vectors_to_lists t (Vector.to_list h :: acc)
  in
  let flt_lst = vectors_to_lists vec_lst [] in
  match vec_lst with
  | [] -> failwith "Cannot create empty matrix"
  | RowVector rvec :: _ -> of_list flt_lst
  | ColVector cvec :: _ -> 
    of_list flt_lst |> transpose

let map = List.map

let map2 (fn : Vector.t -> Vector.t -> Vector.t) (m1 : t) (m2 : t) : t =
  List.map2 fn m1 m2

let matrix_vector_product mat vec swap_order =
  match vec, swap_order with
  | Vector.RowVector _, true -> 
    List.hd (matrix_multiply [vec] mat) (* Outputs a row vector *)
  | ColVector _, false ->
    matrix_multiply mat [vec] (* Outputs a column vector *)
    |> List.map Vector.to_list
    |> List.flatten
    |> Vector.make_col_vec
  | ColVector _, true ->
    failwith "Shape error: first argument should be a row vector"
  | RowVector _, false ->
    failwith "Shape error: second argument should be a column vector"