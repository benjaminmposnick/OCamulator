let sort_asc (data : float list) =
  List.sort compare data

let sort_desc (data : float list) =
  sort_asc data |> List.rev

let cum_sum (data : float list) =
  List.fold_left (fun acc x -> acc +. x) 0. data

let cum_prod (data : float list) =
  List.fold_left (fun acc x -> acc *. x) 1. data

let mean (data : float list) =
  let n = List.length data in
  if n > 0 then
    cum_sum data /. (float_of_int n)
  else
    0.

let err (data : float list) =
  let mu = mean data in 
  List.map (fun x -> x -. mu) data

let squared_sum (data : float list) =
  List.fold_left (fun acc x -> acc +. x ** 2.) 0. data

let mean_squared (data : float list) =
  let n = List.length data in
  if n > 0 then
    (squared_sum data /. (float_of_int n)) 
  else
    0.

let rms (data : float list) =
  mean_squared data ** 0.5

let median_val (data : float list) (n : float) =
  let data = sort_asc data in
  let ind = n /. 2. in
  if int_of_float n mod 2 = 1 then List.nth data (ind -. 0.5 |> int_of_float)
  else mean (List.nth data (ind -. 1. |> int_of_float) 
             :: [List.nth data (ind |> int_of_float)])

let median (data : float list) =
  let n = List.length data in
  let data = sort_asc data in
  if n = 0 then 0.
  else if n = 1 then List.nth data 0
  else median_val data (float_of_int n)

let count (v : float) (data : float list) =
  let rec count_helper v acc = function
    | [] -> acc
    | h::t ->
      begin 
        if h = v
        then count_helper v (acc +. 1.) t
        else count_helper v acc t
      end
  in count_helper v 0. data

let rec mode_assoc acc lst =
  match lst with 
  | [] -> acc
  | h::t -> 
    begin 
      if List.mem_assoc h acc
      then mode_assoc acc t 
      else mode_assoc ((h,count h lst)::acc) t
    end

(** from rosetta code*)
let mode (data : float list) =
  let counts = mode_assoc [] data in
  let rec max_value acc counts = 
    match counts with 
    | [] -> acc
    | (k,v)::t -> 
      begin 
        if List.assoc k counts > acc
        then max_value k t
        else max_value acc t
      end
  in 
  match data with
  |[] -> 0.
  |h::t -> max_value h counts

let get_slope (x : float list) (y : float list) (mu_x : float) (mu_y : float) =
  mean (List.map2 (fun a b -> (a -. mu_x) *. (b -. mu_y)) x y) 
  /. (err x |> squared_sum)

let compute_lin_reg (x : float list) (y : float list) = 
  let mu_x = mean x in
  let mu_y = mean y in
  let m = get_slope x y mu_x mu_y in
  (m, mu_y -. m *. mu_x)

let linear_regression (data : (float * float) list) =
  let x = List.map (fun a -> fst a) data in
  let y = List.map (fun a -> snd a) data in
  compute_lin_reg x y

let quantile_helper (q : float) (n : int) = 
  floor (q *. (n + 1 |> float_of_int))

let quantile (data : float list) (q : float) =
  match data with
  | [] -> 0.
  | x::[] -> x
  | h::t -> 
    List.length data 
    |> quantile_helper q
    |> int_of_float
    |> List.nth (sort_asc data)

let rec max_helper = function
  | [] -> 0.
  | h::[] -> h
  | h::t -> max_helper t

let rec min_helper = function
  | [] -> 0.
  | h::t -> h

let max (data : float list) =
  let data = sort_asc data in
  max_helper data

let min (data : float list) =
  let data = sort_asc data in
  min_helper data

let range (data : float list) = 
  max data -. min data

let smpl_var (data : float list) =
  let n = List.length data |> float_of_int in
  (mean_squared (err data)) *. n /. (n -. 1.)

let smpl_std (data : float list) =
  smpl_var data ** (0.5)

let unique (data : float list) =
  let rec unique_helper acc  = function
    | [] -> acc
    | h::t -> 
      begin
        if List.mem h acc 
        then unique_helper acc t
        else unique_helper (h::acc) t
      end
  in List.rev (unique_helper [] data)

