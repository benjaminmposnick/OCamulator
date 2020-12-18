open Vector

type t = float list list

let to_array mat =
  List.map Array.of_list mat |> Array.of_list

let of_array arr = 
  Array.(map to_list arr) |> Array.to_list

let of_list lst : t = lst

let zeros (n, m) =
  Array.make_matrix n m 0. |> of_array

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
  else List.hd mat |> List.length 

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
    let string_list = List.(map (map string_of_float) mat) in
    let length_max s1 s2 =
      if String.(length s1 >= length s2) then s1 else s2 in
    let max_string_by_row =
      List.(map (fun lst -> fold_left length_max "" lst) string_list) in
    let max_string = List.fold_left length_max "" max_string_by_row in
    String.length max_string
  in
  List.map (fun vec -> string_of_matrix_row max_digits vec) mat
  |> String.concat " |\n"
  |> ( ^ ) "| "
  |> fun str -> str ^ " |" 

let transpose mat =
  let open List in
  let rec transpose_aux lst acc =
    match lst with
    | [] -> []
    | h :: t ->
      if length h = 0 then rev acc
      else 
        let next_row = map hd lst in
        let submatrix = map tl lst in
        transpose_aux submatrix (next_row :: acc)
  in
  transpose_aux mat []

let is_symmetric mat =
  assert (is_square mat);
  mat = (transpose mat)

let multiply m1 m2 = 
  assert (n_cols m1 = n_rows m2);
  let open List in
  let m2_t = transpose m2 |> map (fun row -> Vector.make_row_vec row) in
  let row_fn row = 
    let row_vec = Vector.make_row_vec row in
    List.map (Vector.dot_product row_vec) m2_t in
  List.map row_fn m1

let get_row = List.nth

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

let row_sums matrix =
  List.map (fun rvec -> Vector.RowVector rvec) matrix
  |> List.map Vector.sum 

let apply_to_all f matrix = 
  List.(map (map f) matrix)

let is_lower_triangular matrix =
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

let is_upper_triangular matrix =
  transpose matrix |> is_lower_triangular

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

