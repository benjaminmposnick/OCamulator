open Ast

let transpose = function
  | RowVector vec -> ColumnVector vec
  | ColumnVector vec -> RowVector vec
  | Matrix mat -> 
    let rec transpose_matrix acc lst = 
      let open List in
      match lst with
      | [] -> []
      | h::t ->
        if length h = 0 then rev acc
        else 
          let next_row = map hd lst in
          let submatrix = map tl lst in
          transpose_matrix (next_row::acc) submatrix in
    transpose_matrix [] mat
    |> (fun m -> Matrix m)

let is_symmetric m =
  let open List in
  let n_rows = length m in
  let n_cols = length (hd m) in
  if n_cols <> n_rows then
    failwith "Matrix must be square"
  else
    let matrix = Matrix m in
    matrix = (transpose matrix)

(** [component_wise_application v1 v2 op] is the result of applying the function
    [op] component-wise to the elements of vectors [v1] and [v2].
    Requires: [v1] and [v2] are the same length. *)
let component_wise_application v1 v2 op =
  try List.map2 op v1 v2 with
  | Invalid_argument _ -> 
    failwith "Vectors must be the same length"

let component_wise_add v1 v2 =
  component_wise_application v1 v2 ( +. )

let component_wise_subtract v1 v2 =
  component_wise_application v1 v2 ( -. )

let component_wise_multiply v1 v2 =
  component_wise_application v1 v2 ( *. )

let dot_product v1 v2 =
  component_wise_multiply v1 v2
  |> List.fold_left ( +. ) 0.

let matrix_multiply m1 m2 = 
  let open List in
  let m2' = 
    match transpose (Matrix m2) with
    | Matrix m -> m
    | _ -> failwith "Impossible"
  in
  map (fun row -> map (dot_product row) m2') m1

(** [zero_out_entries_below_pivot matrix next_pivot_idx tolerance] is [matrix]
    with all entries below the pivot zeroed out, i.e. any row with index greater
    than [next_pivot_idx]. Any entry less than [tolerance] is zeroed out to ensure
    that round-off errors do not lead to more pivots being declared than are
    truly present. *)
let zero_out_entries_below_pivot matrix next_pivot_idx tolerance = 
  let open List in
  let pivot_row = nth matrix next_pivot_idx in
  let rec zero_out_aux acc idx matrix = 
    match matrix with
    | [] -> rev acc
    | next_row::remaining_rows -> 
      let acc' = 
        if next_pivot_idx < idx then
          let pivot = hd pivot_row in
          let entry = hd next_row in
          let c = ~-.(entry /. pivot) in (* Scale factor for pivot *)
          let scaled_pivot_row = map (fun x -> c *. x) pivot_row in
          let round n = if Float.abs n <= tolerance then 0. else n in
          let scaled_next_row =
            map2 (fun x y -> round (x +. y)) scaled_pivot_row next_row in
          (scaled_next_row::acc)
        else (next_row::acc) in
      zero_out_aux acc' (idx + 1) remaining_rows
  in
  zero_out_aux [] 0 matrix

(** [swap_partial_pivot matrix next_pivot_idx partial_pivot_idx] is [matrix]
    except for the row with number [next_pivot_idx] is swapped with the row with 
    number [partial_pivot_idx].
    Requires: [partial_pivot_idx] is the index of a valid pivot *)
let swap_partial_pivot matrix next_pivot_idx partial_pivot_idx  = 
  let open List in
  let dest_row = nth matrix next_pivot_idx in
  let partial_pivot_row = nth matrix partial_pivot_idx in
  let rec construct_matrix_after_swap acc idx = function
    | [] -> rev acc
    | next_row::remaining_rows ->
      let acc' =
        if idx = next_pivot_idx then partial_pivot_row::acc
        else if idx = partial_pivot_idx then dest_row::acc
        else next_row::acc in
      construct_matrix_after_swap acc' (idx + 1) remaining_rows
  in
  construct_matrix_after_swap [] 0 matrix

(** [partial_pivot_idx col next_pivot_min] is the index of the largest entry in
    absolute value in [col] in a row greater than or equal to [next_pivot_idx]
    Requires: [col] is a pivot column. *)
let partial_pivot_idx col next_pivot_min =
  let (|<|) x y = Float.(abs x < abs y) in (* absolute value less than *)
  let rec partial_pivot_idx_aux (abs_max, abs_max_idx) curr_idx col =
    match col with
    | [] -> abs_max_idx
    | x::xs ->
      let new_max = 
        if abs_max |<| x && next_pivot_min <= curr_idx then (x, curr_idx)
        else (abs_max, abs_max_idx) in
      partial_pivot_idx_aux new_max (curr_idx + 1) xs
  in
  partial_pivot_idx_aux (0., ~-1) 0 col

(** [is_pivot_col col next_pivot_idx] is true iff [col] is a pivot column,
    i.e. there exists at least one nonzero entry in [col] in a row greater
    than or equal to [next_pivot_idx] 
    Note: Ignoring nonzero entries less than [next_pivot_col] is equivalent
    to ignoring/covering up previous pivots, as described in the Gaussian
    elimination algorithm. *)
let is_pivot_col col next_pivot_idx =
  let rec is_pivot_col_aux idx col =
    match col with
    | [] -> false
    | x::xs -> 
      if x <> 0. && next_pivot_idx <= idx then true
      else is_pivot_col_aux (idx + 1) xs
  in
  is_pivot_col_aux 0 col

(** [determine_tolerance matrix] is the tolerance for [matrix] that is used
    to determine negligible column elements which can be zeroed-out to reduce
    roundoff errors. More specifically, if A = [matrix] and A is m X n, then the
    tolerance is defined as:
      max(m, n) * eps * l_inf_norm(A) 
    where eps is machine epsilon and l_inf_norm is the L-infinity norm. *)
let determine_tolerance matrix =
  let open List in
  let n_rows = length matrix in
  let n_cols = hd matrix |> length in
  let max_dim = max n_rows n_cols |> Float.of_int in
  let machine_epsilon = Float.epsilon in
  let col_sums = map (fold_left ( +. ) 0.) matrix in
  let abs_col_sums = map (Float.abs) col_sums in
  let l_inf_norm = fold_left max ~-.1. abs_col_sums in
  max_dim *. machine_epsilon *. l_inf_norm

(** [matrix_of_column_list col_list] is a matrix in row-major form created from
    the columns of [col_list]. *)
let matrix_of_column_list col_list =
  let transposed_matrix =
    List.rev col_list (* Reverse the list of columns *)
    |> (fun m -> Matrix m)
    |> transpose (* Convert from column-major to row-major *)
  in
  match transposed_matrix with
  | Matrix m' -> m'
  | _ -> failwith "Impossible"

(** [row_echelon_form matrix] is [matrix] in echelon form, i.e. the result of
    the forward phase of Gaussian elimination. The result of this algorithm is
    deterministic but not unique -- that is, there are infinitely many valid
    echelon forms of [matrix] but this algorithm will yield the same echelon
    form if given the same input repeatedly. This implementation uses partial
    pivoting to reduce round-off errors as well as zeros out any entries that are
    smaller than [tolerance].
    Requires: [matrix] is no smaller than a 2x2 matrix and [tolerance] is
    computed by the function [determine_tolerance] applied to [matrix].  *)
let row_echelon_form matrix tolerance = 
  let open List in
  let rec row_echelon_form_aux matrix next_pivot_idx acc pivot_col_idxs =
    let row_size = hd matrix |> length in 
    if row_size = 0 then (matrix_of_column_list acc, pivot_col_idxs)
    else
      let next_col = map hd matrix in
      if not (is_pivot_col next_col next_pivot_idx) then (* Not a pivot column *)
        let remaining_cols = map tl matrix in 
        let acc' = next_col::acc in
        row_echelon_form_aux remaining_cols next_pivot_idx acc' pivot_col_idxs
      else (* Is a pivot column *)
        let partial_pivot_idx = partial_pivot_idx next_col next_pivot_idx in
        let matrix' = swap_partial_pivot matrix next_pivot_idx partial_pivot_idx in
        let matrix'' =
          zero_out_entries_below_pivot matrix' next_pivot_idx tolerance in
        let pivot_col = map hd matrix'' in
        let remaining_cols = map tl matrix'' in
        let acc' = pivot_col::acc in
        let col_idx = length (acc') - 1 in
        let pivot_col_idxs' = col_idx::pivot_col_idxs in
        row_echelon_form_aux remaining_cols (next_pivot_idx + 1) acc' pivot_col_idxs'
  in
  row_echelon_form_aux matrix 0 [] []

(** [zero_out_entries_above_pivot matrix i j n_cols] uses the pivot located in
    row [i] and column [j] of [matrix] to zero-out all entries above the 
    pivot, i.e. any row with index less than [i], as part of the backward phase
    of Gaussian elimination.
    Requires: If A = [matrix], then A[[i][j]] is one of the pivots of matrix A.
*)
let zero_out_entries_above_pivot matrix i j n_cols = 
  let pivot = matrix.(i).(j) in
  let rec loop k =
    if k = i then ()
    else
      let entry = matrix.(k).(j) in
      let c = ~-.(entry /. pivot) in 
      for idx = 0 to n_cols - 1 do
        matrix.(k).(idx) <- (c *. matrix.(i).(idx)) +. matrix.(k).(idx)
      done;
      loop (k + 1) in
  loop 0;
  for idx = 0 to n_cols - 1 do
    matrix.(i).(idx) <- ((1. /. pivot) *. matrix.(i).(idx))
  done; ()

(** [reduced_row_echelon_form matrix pivot_col_idxs] is [matrix] in reduced 
    row echelon form, i.e. the result of the backward phase of Gaussian
    elimination.
    Requires: [matrix] is no smaller than a 2x2 matrix and [matrix] is already
    in row echelon form; [pivot_col_idxs] are the indexs of the columns of
    [matrix] that are pivot columns *)
let reduced_row_echelon_form matrix pivot_col_idxs = 
  let n_cols = List.(length (hd matrix)) in
  let n_pivots = List.length pivot_col_idxs in
  let rec rref_aux matrix next_pivot_row_idx pivot_col_idxs =
    match pivot_col_idxs with
    | [] -> matrix 
    | j::remaining_col_idxs -> 
      zero_out_entries_above_pivot matrix next_pivot_row_idx j n_cols;
      rref_aux matrix (next_pivot_row_idx - 1) remaining_col_idxs
  in
  let matrix_array = List.map Array.of_list matrix |> Array.of_list in
  let rref_matrix = rref_aux matrix_array (n_pivots - 1) pivot_col_idxs in
  Array.(map to_list rref_matrix) |> Array.to_list

(** [purify tolerance matrix] is [matrix] except all negative zero entries are
    converted to zero (i.e. without the negative) sign, any values in the
    matrix less than or equal to [tolerance] are zeroed out, and all values are
    expressed with up to four decimal places of precision. *)
let purify tolerance matrix = 
  let open List in
  let purify_aux x = 
    if Float.abs x <= tolerance then 0.
    else 
      Printf.sprintf "%.4f" x
      |> float_of_string
      |> (fun x -> if x = ~-.0. then 0. else x)
  in
  map (map purify_aux) matrix

let pivot_cols matrix =
  let tolerance = determine_tolerance matrix in
  snd (row_echelon_form matrix tolerance)
  |> List.sort compare
  |> List.map (fun i -> float_of_int i)

let rref matrix =
  let tolerance = determine_tolerance matrix in
  let (echelon_form_matrix, pivot_col_idxs) = 
    row_echelon_form matrix tolerance in
  reduced_row_echelon_form echelon_form_matrix pivot_col_idxs
  |> purify tolerance

(** [lu_decomposition matrix] is the LU decomposition of [matrix], i.e. the
    pair of matrices L and U such that if A = [matrix], then A = LU. Computed
    using the Doolittle method. 
    Requires: [matrix] is square. *)
let lu_decomposition matrix =
  let n_rows = List.length matrix in
  let n_cols = List.(length (hd matrix)) in
  if n_cols <> n_rows then
    failwith "Matrix must be square"
  else
    let open Array in
    let n = n_rows in
    let get_ith_col_of_u u i =
      let u_i = make n 0. in
      for j = 0 to n - 1 do
        u_i.(j) <- u.(j).(i);
      done; u_i in
    let a = List.map of_list matrix |> of_list in
    let l = make_matrix n n 0. in
    let u = make_matrix n n 0. in
    for k = 0 to n - 1 do
      l.(k).(k) <- 1.;
      for j = k to n - 1 do
        let u_j = get_ith_col_of_u u j in
        u.(k).(j) <- a.(k).(j) -. (dot_product (to_list l.(k)) (to_list u_j))
      done;
      for i = k + 1 to n - 1 do
        let u_k = get_ith_col_of_u u k in
        l.(i).(k) <- (1. /. u.(k).(k)) *. (a.(i).(k) -. (dot_product (to_list l.(i)) (to_list u_k)))
      done;
    done;
    let list_of_array arr = Array.(map to_list arr) |> Array.to_list in
    (list_of_array l, list_of_array u)

(** [diagonal_product matrix] is the product of the entries on the main
    diagonal of [matrix].
    Requires: [matrix] is square. *)
let diagonal_product matrix =
  let n_rows = List.length matrix in
  let n_cols = List.(length (hd matrix)) in
  if n_cols <> n_rows then
    failwith "Matrix must be square"
  else
    let open Array in
    let a = List.map of_list matrix |> of_list in
    let diag = make n_rows 0. in
    for i = 0 to n_rows - 1 do
      diag.(i) <- a.(i).(i)
    done;
    Array.fold_left ( *. ) 1. diag

let determinant matrix =
  let (l, u) = lu_decomposition matrix in
  (diagonal_product l) *. (diagonal_product u)