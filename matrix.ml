open Vector

type t = Vector.t list

let n_rows mat =
  List.length mat

let n_cols mat =
  if List.length mat = 0 then 0
  else
    List.hd mat
    |> Vector.size 

(** [rep_ok mat] is [mat] if [mat] satisfies its representation invariants and
    raises [Failure] otherwise. *)
let rep_ok mat =
  let open Vector in
  let rec check_for_col_vecs = function
    | [] -> ()
    | RowVector _ :: t -> check_for_col_vecs t
    | ColVector _ :: t-> failwith "Matrix cannot contain column vectors" in
  let all_rows_same_len =
    List.map Vector.size mat
    |> List.fold_left (fun acc n ->
        if not (List.mem n acc) then n :: acc else acc) []
    |> (fun lst -> if List.length lst <= 1 then true else false) in
  ignore(check_for_col_vecs mat);
  if not (all_rows_same_len) then
    failwith "Matrix must have all rows the same length"
  else mat
[@@coverage off]

let to_list mat =
  ignore(rep_ok mat);
  List.map Vector.to_list mat

let to_array mat =
  ignore(rep_ok mat);
  List.map Vector.to_array mat
  |> Array.of_list

let of_list lst =
  List.map Vector.make_row_vec lst
  |> rep_ok

let of_array arr =
  Array.(map to_list arr)
  |> Array.to_list
  |> of_list
  |> rep_ok

let make n m init =
  Array.make_matrix n m init
  |> of_array
  |> rep_ok

let zeros ?(n=(~-1)) m =
  let matrix =
    if n = ~-1 then make m m 0.
    else make n m 0. in
  rep_ok matrix

let identity n =
  let arr = Array.make_matrix n n 0. in
  for i = 0 to n - 1 do
    arr.(i).(i) <- 1.
  done;
  of_array arr
  |> rep_ok

let is_square mat =
  ignore(rep_ok mat);
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
  List.map string_of_entry row
  |> String.concat " "

let string_of_matrix mat =
  ignore(rep_ok mat);
  let max_digits =
    let string_list =
      let vec_to_float_strings vec =
        List.map string_of_float (Vector.to_list vec) in
      List.map vec_to_float_strings mat in
    let length_max s1 s2 =
      if String.(length s1 >= length s2) then s1 else s2 in
    let max_string_by_row =
      let get_max_str lst = List.fold_left length_max "" lst in
      List.map get_max_str string_list in
    let max_string = List.fold_left length_max "" max_string_by_row in
    String.length max_string
  in
  to_list mat
  |> List.map (fun vec -> "| " ^ string_of_matrix_row max_digits vec)
  |> String.concat " |\n"
  |> fun str -> str ^ " |"

let transpose mat =
  let rec transpose_aux lst acc =
    match lst with
    | [] -> []
    | h :: t ->
      if Vector.size h = 0 then
        List.rev acc
        |> of_list
      else
        let next_row = List.map Vector.head lst in
        let submatrix = List.map Vector.tail lst in
        transpose_aux submatrix (next_row :: acc)
  in
  transpose_aux mat []
  |> rep_ok

let is_symmetric mat =
  assert (is_square mat);
  ignore(rep_ok mat);
  mat = (transpose mat)

let matrix_multiply m1 m2 =
  assert (n_cols m1 = n_rows m2);
  ignore(rep_ok m1);
  ignore(rep_ok m2);
  let m2_t = transpose m2 in
  let row_fn row = List.map (Vector.dot_product row) m2_t in
  List.map row_fn m1
  |> of_list
  |> rep_ok

let get_row mat i =
  ignore(rep_ok mat);
  List.nth mat i
  |> Vector.to_list

let get_col mat j =
  ignore(rep_ok mat);
  transpose mat
  |> (fun mat_t -> get_row mat_t j)

let drop_row mat i =
  let rec drop_row_aux acc idx = function
    | [] -> List.rev acc
    | h :: t ->
      if i = idx then (List.rev acc) @ t
      else drop_row_aux (h :: acc) (idx + 1) t
  in
  ignore(rep_ok mat);
  drop_row_aux [] 0 mat
  |> rep_ok

let drop_col mat j =
  ignore(rep_ok mat);
  transpose mat
  |> (fun mat_t -> drop_row mat_t j)
  |> transpose
  |> rep_ok

let apply_to_all f mat =
  ignore(rep_ok mat);
  List.map (fun row -> Vector.map f row) mat
  |> rep_ok

let rec of_vectors vec_lst =
  match vec_lst with
  | [] -> failwith "Cannot create empty matrix"
  | Vector.RowVector rvec :: _ -> vec_lst
  | Vector.ColVector cvec :: _ ->
    List.map Vector.transpose vec_lst
    |> transpose
    |> rep_ok

let is_upper_triangular mat =
  assert (is_square mat);
  ignore(rep_ok mat);
  let n = n_rows mat in
  let a = to_array mat in
  let elems = ref [] in
  for j = 0 to n - 1 do
    for i = j + 1 to n - 1 do
      elems := a.(i).(j) :: !elems
    done;
  done;
  List.fold_left (fun acc x -> if x = 0. then acc else false) true !elems

let is_lower_triangular mat =
  ignore(rep_ok mat);
  transpose mat
  |> is_upper_triangular

let map fn mat =
  ignore(rep_ok mat);
  List.map fn mat
  |> rep_ok

let map2 fn m1 m2 =
  ignore(rep_ok m1);
  ignore(rep_ok m2);
  List.map2 fn m1 m2
  |> rep_ok

let row_sums mat =
  List.map Vector.sum mat

let matrix_vector_product mat vec swap_order =
  ignore(rep_ok mat);
  let open Vector in
  match vec, swap_order with
  | Vector.RowVector _, true ->
    List.hd (matrix_multiply [vec] mat) (* Outputs a row vector *)
  | ColVector _, false ->
    let cvec = List.map (fun elem -> make_row_vec [elem]) (to_list vec) in
    matrix_multiply mat cvec (* Outputs a column vector *)
    |> List.map Vector.to_list
    |> List.flatten
    |> Vector.make_col_vec
  | ColVector _, true ->
    failwith "Shape error: first argument should be a row vector"
  | RowVector _, false ->
    failwith "Shape error: second argument should be a column vector"
