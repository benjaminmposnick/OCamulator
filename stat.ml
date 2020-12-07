let get_U () = Random.float 1.

(** [bernoulli_sam k p] is a random variable drawn from Bernoulli p
    Requires: [k] is a valid bernoulli rv, 0 or 1
    [p] is a valid bernolli p, [0,1] *)
let bernoulli_sam  (p : float) =
  if get_U () < p then 1.
  else 0.

let exponential_sam (l : float) =
  -1. *. log (1. -. (get_U ())) /. l

let geo_helper x = log (-1. *. (x -. 1.))

let geometric_sam (p : float) =
  ceil (geo_helper (get_U ()) /. geo_helper p)

let uniform_sam (a : float) (b : float) =
  (b -. a) *. (get_U ()) +. a

let sort_asc (data : float list) =
  List.sort compare data

let sort_desc (data : float list) =
  sort_asc data |> List.rev

let cum_sum (data : float list) =
  List.fold_left (fun acc x -> acc +. x) 0. data

let cum_prod (data : float list) =
  List.fold_left (fun acc x -> acc *. x) 0. data

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

(** from rosetta code*)
let mode (data : float list) =
  let seen = Hashtbl.create 42 in
  List.iter (fun x ->
      let old = if Hashtbl.mem seen x then
          Hashtbl.find seen x
        else 0 in
      Hashtbl.replace seen x (old + 1))
    data;
  let best = Hashtbl.fold (fun _ -> max) seen 0 in
  Hashtbl.fold (fun k v acc ->
      if v = best then k :: acc
      else acc)
    seen [] 

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

let smpl_var (data : float list) =
  mean_squared (err data)

let smpl_std (data : float list) =
  smpl_var data ** (0.5)




