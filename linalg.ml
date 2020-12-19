open Ast
open Matrix
open Vector

(* ===========================================================================
    UTILITY FUNCTIONS
   ===========================================================================*)

(** [round n] rounds [n] to 4 decimal places.  *)
let round n =
  Printf.sprintf ("%.4f") n
  |> float_of_string
  |> (fun x -> if x = ~-.0. then 0. else x)

(** [purify tolerance matrix] is [matrix] except all negative zero entries are
    converted to zero (i.e. without the negative sign), any values in the
    matrix less than or equal to [tolerance] are zeroed out, and all values are
    expressed with up to four decimal places of precision. *)
let purify ?no_round:(no_round=false) tolerance matrix = 
  let open List in
  let purify_aux x = 
    if Float.abs x <= tolerance then 0.
    else if no_round then x
    else round x
  in
  Matrix.apply_to_all purify_aux matrix

(** [determine_tolerance matrix] is the tolerance for [matrix] that is used
    to determine negligible column elements which can be zeroed-out to reduce
    roundoff errors. More specifically, if [A] = [matrix] and [A] is [m X n],
    then the tolerance is defined as:
    [max(m, n) * eps * l_inf_norm(A)]
    where [eps] is machine epsilon and [l_inf_norm] is the L-infinity norm. *)
let determine_tolerance matrix =
  let open List in
  let n_rows = Matrix.n_rows matrix in
  let n_cols = Matrix.n_cols matrix in
  let max_dim = max n_rows n_cols |> Float.of_int in
  let machine_epsilon = Float.epsilon in
  let row_sums = Matrix.row_sums matrix in
  let abs_col_sums = map (Float.abs) row_sums in
  let l_inf_norm = fold_left max ~-.1. abs_col_sums in
  max_dim *. machine_epsilon *. l_inf_norm

(* ===========================================================================
    ROW REDUCTION ALGORITHMS
   ===========================================================================*)

(** [elementary_row_addition pivot_row next_row tolerance] is the result of 
    using the pivot in [pivot_row] to zero out the first entry of [next_row] via
    elementary row addition. 
    Requires: The head element of the [pivot_row] is a pivot. *)
let elementary_row_addition pivot_row next_row tolerance = 
  let open List in
  let pivot = hd pivot_row in
  let entry = hd next_row in
  let c = ~-.(entry /. pivot) in (* Scale factor for pivot *)
  let scaled_pivot_row = map (fun x -> c *. x) pivot_row in
  let round = fun n -> if Float.abs n <= tolerance then 0. else n
  in
  map2 (fun x y -> round (x +. y)) scaled_pivot_row next_row

(** [zero_out_entries_below_pivot matrix next_pivot_idx tolerance] is [matrix]
    with all entries below the pivot zeroed out, i.e. any row with index greater
    than [next_pivot_idx]. Any entry less than [tolerance] is zeroed out to
    ensure that round-off errors do not lead to more pivots being declared than
    are truly present. *)
let zero_out_entries_below_pivot matrix next_pivot_idx tolerance = 
  let open List in
  let pivot_row = Matrix.get_row matrix next_pivot_idx in
  let rec zero_out_aux acc idx matrix = 
    if Matrix.n_rows matrix = 0 then List.rev acc
    else
      let next_row = Matrix.get_row matrix 0 in
      let remaining_rows = Matrix.drop_row matrix 0 in
      let acc' = 
        if next_pivot_idx < idx then (* [idx] is below the pivot *)
          let scaled_next_row =
            elementary_row_addition pivot_row next_row tolerance in
          (scaled_next_row :: acc)
        else (next_row :: acc) in
      zero_out_aux acc' (idx + 1) remaining_rows
  in
  zero_out_aux [] 0 matrix |> Matrix.of_list

(** [swap_partial_pivot matrix next_pivot_idx partial_pivot_idx] is [matrix]
    except for the row with index [next_pivot_idx] is swapped with the row with 
    index [partial_pivot_idx].
    Requires: [partial_pivot_idx] is the index of a valid pivot *)
let swap_partial_pivot matrix next_pivot_idx partial_pivot_idx  = 
  let open List in
  let dest_row = Matrix.get_row matrix next_pivot_idx in
  let partial_pivot_row = Matrix.get_row matrix partial_pivot_idx in
  let n_rows = Matrix.n_rows matrix in
  let rec construct_matrix_after_swap acc idx matrix =
    if idx = n_rows then List.rev acc
    else
      let next_row = Matrix.get_row matrix 0 in
      let remaining_rows = Matrix.drop_row matrix 0 in
      let acc' =
        if idx = next_pivot_idx then partial_pivot_row :: acc
        else if idx = partial_pivot_idx then dest_row :: acc
        else next_row :: acc in
      construct_matrix_after_swap acc' (idx + 1) remaining_rows
  in
  construct_matrix_after_swap [] 0 matrix |> Matrix.of_list

(** [partial_pivot_idx col next_pivot_min] is the index of the largest entry in
    absolute value in [col] in a row greater than or equal to [next_pivot_min]
    Requires: [col] is a pivot column. *)
let partial_pivot_idx col next_pivot_min =
  let (|<|) x y = Float.(abs x < abs y) in (* absolute value less than *)
  let rec partial_pivot_idx_aux (abs_max, abs_max_idx) curr_idx col =
    match col with
    | [] -> abs_max_idx
    | x :: xs ->
      let new_max = 
        if abs_max |<| x && next_pivot_min <= curr_idx then (x, curr_idx)
        else (abs_max, abs_max_idx) in
      partial_pivot_idx_aux new_max (curr_idx + 1) xs
  in
  partial_pivot_idx_aux (0., ~-1) 0 col

(** [is_pivot_col col next_pivot_idx] is true iff [col] is a pivot column,
    i.e. there exists at least one nonzero entry in [col] in a row greater
    than or equal to [next_pivot_idx].
    Note: Ignoring nonzero entries less than [next_pivot_col] is equivalent
    to ignoring/covering up previous pivots, as described in the Gaussian
    elimination algorithm. *)
let is_pivot_col col next_pivot_idx =
  let rec is_pivot_col_aux idx col =
    match col with
    | [] -> false
    | x :: xs -> 
      if x <> 0. && next_pivot_idx <= idx then true
      else is_pivot_col_aux (idx + 1) xs
  in
  is_pivot_col_aux 0 col

(** [row_echelon_form matrix] is [matrix] in echelon form, i.e. the result of
    the forward phase of Gaussian elimination. The result of this algorithm is
    deterministic but not unique -- that is, there are infinitely many valid
    echelon forms of [matrix] but this algorithm will yield the same echelon
    form if given the same input repeatedly. This implementation uses partial
    pivoting to reduce round-off errors as well as zeros out any entries that
    are smaller than [tolerance].
    Requires: [matrix] is no smaller than a 2x2 matrix and [tolerance] is
    computed by the function [determine_tolerance] applied to [matrix].  *)
let row_echelon_form matrix tolerance = 
  let open List in
  let rec row_echelon_form_aux matrix next_pivot_idx acc pivot_col_idxs =
    let n_cols = Matrix.n_cols matrix in 
    if n_cols = 0 then (Matrix.(transpose (of_list (List.rev acc))), pivot_col_idxs)
    else
      let next_col = Matrix.get_col matrix 0 in
      if not (is_pivot_col next_col next_pivot_idx) then
        let remaining_cols = Matrix.drop_col matrix 0 in 
        let acc' = next_col :: acc in
        row_echelon_form_aux remaining_cols next_pivot_idx acc' pivot_col_idxs
      else (* Is a pivot column *)
        let partial_pivot_idx = partial_pivot_idx next_col next_pivot_idx in
        let matrix' =
          swap_partial_pivot matrix next_pivot_idx partial_pivot_idx
          |> fun m -> zero_out_entries_below_pivot m next_pivot_idx tolerance in
        let pivot_col = Matrix.get_col matrix' 0 in
        let remaining_cols = Matrix.drop_col matrix' 0 in
        let acc' = pivot_col :: acc in
        let col_idx = length (acc') - 1 in
        let pivot_col_idxs' = col_idx :: pivot_col_idxs in
        row_echelon_form_aux remaining_cols (next_pivot_idx + 1) acc' pivot_col_idxs'
  in
  row_echelon_form_aux matrix 0 [] []

(** [zero_out_entries_above_pivot matrix i j n_cols] uses the pivot located in
    row [i] and column [j] of [matrix] to zero-out all entries above the 
    pivot, i.e. any row with index less than [i], as part of the backward phase
    of Gaussian elimination.
    Requires: If [A] =def= [matrix], then [A][[i][j]] is one of the pivots of
    matrix [A]. *)
let zero_out_entries_above_pivot matrix i j n_cols = 
  let a = Matrix.to_array matrix in
  let pivot = a.(i).(j) in
  let rec loop k =
    if k = i then ()
    else
      let entry = a.(k).(j) in
      let c = ~-.(entry /. pivot) in 
      for idx = 0 to n_cols - 1 do
        a.(k).(idx) <- (c *. a.(i).(idx)) +. a.(k).(idx)
      done;
      loop (k + 1) in
  loop 0;
  for idx = 0 to n_cols - 1 do
    a.(i).(idx) <- ((1. /. pivot) *. a.(i).(idx))
  done;
  Matrix.of_array a

(** [reduced_row_echelon_form matrix pivot_col_idxs] is [matrix] in reduced 
    row echelon form, i.e. the result of the backward phase of Gaussian
    elimination.
    Requires: [matrix] is no smaller than a 2x2 matrix and [matrix] is already
    in row echelon form; [pivot_col_idxs] are the indexs of the columns of
    [matrix] that are pivot columns. *)
let reduced_row_echelon_form matrix pivot_col_idxs = 
  let n_cols = Matrix.n_cols matrix in
  let n_pivots = List.length pivot_col_idxs in
  let rec rref_aux matrix next_pivot_row_idx pivot_col_idxs =
    match pivot_col_idxs with
    | [] -> matrix 
    | j :: remaining_col_idxs -> 
      zero_out_entries_above_pivot matrix next_pivot_row_idx j n_cols
      |> fun m -> rref_aux m (next_pivot_row_idx - 1) remaining_col_idxs
  in
  rref_aux matrix (n_pivots - 1) pivot_col_idxs

let pivot_cols matrix =
  let tolerance = determine_tolerance matrix in
  snd (row_echelon_form matrix tolerance)
  |> List.sort compare
  |> List.map (fun i -> VInt i)

let rref matrix =
  let tolerance = determine_tolerance matrix in
  let (echelon_form, pivot_col_idxs) = row_echelon_form matrix tolerance in
  reduced_row_echelon_form echelon_form pivot_col_idxs
  |> purify tolerance

(* ===========================================================================
   MATRIX FACTORIZATIONS
   ===========================================================================*)

(** [plu_decomposition matrix] is the PLU decomposition of [matrix], i.e. the
    triple of matrices P, L, and U such that if A = [matrix], then A = PLU, 
    where P is the permutation matrix, L is lower triangular, and U is upper
    triangular. Similar to the LU decomposition, except the permutation matrix
    keeps track of row interchanges which are required for numerical stability.
    Requires: [matrix] is square. *)
let plu_decomposition ?no_round:(no_round=false) matrix =
  assert (Matrix.is_square matrix);
  let tolerance = determine_tolerance matrix in
  let n = n_rows matrix in
  let a = Matrix.to_array matrix in (* original matrix *)
  let u = ref (Array.copy a) in (* upper triangular *)
  let l = ref (Matrix.(to_array (identity n))) in (* lower triangular *)
  let p = ref (Matrix.(to_array (identity n))) in (* permutation matrix *)
  let n_swaps = ref 0 in
  for i = 0 to n - 1 do
    let rec swap k =
      if !u.(i).(i) = 0. then
        if k = n - 1 then false
        else
          let tmp_u = !u.(i) in
          !u.(i) <- !u.(k + 1); !u.(k + 1) <- tmp_u;
          let tmp_p = !p.(i) in
          !p.(i) <- !p.(k + 1); !p.(k + 1) <- tmp_p;
          n_swaps := !n_swaps + 1;
          swap (k + 1)
      else true in
    if swap i then
      for j = i + 1 to n - 1 do
        !l.(j).(i) <- !u.(j).(i) /. !u.(i).(i) ;
        !u.(j) <- Array.(map2 ( -. ) !u.(j) (map (fun x -> x *. !l.(j).(i)) !u.(i)));
      done
    else ()
  done;
  let format arr =
    Matrix.of_array arr
    |> (fun m -> purify ~no_round:no_round tolerance m)in
  (format !p, format !l, format !u, !n_swaps)

(** [diagonal_product matrix] is the product of the entries on the main
    diagonal of [matrix].
    Requires: [matrix] is square. *)
let diagonal_product matrix =
  assert (Matrix.is_square matrix);
  let n = Matrix.n_rows matrix in
  let a = Matrix.to_array matrix in
  let diag = ref [] in
  for i = 0 to n - 1 do
    diag := a.(i).(i) :: !diag
  done;
  List.fold_left ( *. ) 1. !diag |> round

let determinant mat = 
  let (p, l, u, n_swaps) = plu_decomposition mat in
  let det_p = if n_swaps mod 2 = 0 then 1. else -1. in
  let det_l = diagonal_product l in
  let det_u = diagonal_product u in
  det_p *. det_l *. det_u |> round

let range lo hi =
  let rec range_aux idx acc =
    if idx = lo - 1 then acc 
    else range_aux (idx - 1) (idx :: acc) in
  range_aux hi []

let forward_substitution l b =
  let n = Vector.size b in
  let y = ref (Vector.(to_array (zeros n))) in
  let b = ref (Vector.(to_array b)) in
  let l = ref (Matrix.(to_array l)) in
  !y.(0) <- !b.(0) /. !l.(0).(0);
  for i = 1 to n - 1 do
    let sum = ref 0. in
    List.iter (fun j ->
        sum := !sum +. (!l.(i).(j) *. !y.(j))) (range 0 (i - 1));
    !y.(i) <- (1. /. !l.(i).(i)) *. (!b.(i) -. !sum)
  done; 
  Vector.of_array !y
  |> Vector.make_col_vec

let back_substitution u y =
  let n = Vector.size y in
  let x = ref (Vector.(to_array (zeros n))) in
  let u = ref (Matrix.(to_array u)) in
  let y = ref (Vector.(to_array y)) in
  !x.(n - 1) <- !y.(n - 1) /. !u.(n - 1).(n - 1);
  for i' = 0 to n - 2 do
    let i = (n - 2) - i' in
    let sum = ref 0. in
    List.iter (fun j ->
        sum := !sum +. (!u.(i).(j) *. !x.(j))) (range (i + 1) (n - 1));
    !x.(i) <- (1. /. !u.(i).(i)) *. (!y.(i) -. !sum)
  done; !x

let inverse mat =
  let tolerance = determine_tolerance mat in
  let (p, l, u, _) = plu_decomposition ~no_round:true mat in
  let n = n_rows p in
  let b = Matrix.identity n in
  let a_inv = ref (Matrix.(to_array (zeros (n, n)))) in
  for i = 0 to n - 1 do
    let bi = Vector.make_col_vec (Matrix.get_row b i) in
    let p_dot_bi_mat = Matrix.(multiply p (of_vectors [bi])) in
    let p_dot_bi_vec = 
      Matrix.to_list p_dot_bi_mat
      |> List.flatten
      |> Vector.make_col_vec in
    let y = forward_substitution l p_dot_bi_vec in
    !a_inv.(i) <- back_substitution u y
  done;
  !a_inv
  |> Matrix.of_array
  |> Matrix.transpose
  |> purify tolerance