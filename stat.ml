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