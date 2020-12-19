
let reduce_tr inc test op base f x =
  let rec apply_tr acc x =
    if test x then acc
    else apply_tr (op (f x) acc) (inc x)
  in apply_tr base x

let sigma_tr f a b =
  reduce_tr (fun x -> x + 1) 
    (fun x -> if x = b then true else false) ( +. ) (f b) f a

(** from lecture code*)
let factorial : int -> int = fun num ->
  let rec helper : int -> int -> int = fun n acc ->
    if n > 0
    then helper (n-1) (acc * n)
    else acc
  in
  helper num 1

let choose (n : int) (k : int) : float = 
  float_of_int(factorial(n)) /. float_of_int((factorial(k) * factorial(n - k)))

let perm (n : int) (r : int) : float =
  float_of_int(factorial(n)) /. float_of_int((factorial(n - r)))

let uniform_pmf (a : float) (b : float) (x : float) : float =
  if x >= a && x <= b then 1. /. (b -. a) else 0.

let uniform_cdf (a : float) (b : float) (x : float) : float =
  if x < a then 0. 
  else if x > b then 1.
  else (x -. a) /. (b -. a)

let bernoulli_pmf  (p : float) (k : int) : float =
  if k = 1 then p else 1. -. p

let bernoulli_cdf  (p : float) (k : int ) : float =
  if k < 0 then 0.
  else 1.

let geometric_pmf (p : float) (k : int) : float=
  p *. (1. -. p) ** (float_of_int (k - 1))

let geometric_cdf  (p : float) (k : int) : float =
  1. -. (1. -. p) ** (float_of_int k)

let exponential_pmf  (l : float) (x : float) : float =
  l *. exp (x *. -1. *. l)

let exponential_cdf (l : float) (x : float) : float =
  1. -. exp (-1. *. l *. x)

let binomial_pmf (n : int) (p : float) (k : int) : float =
  (choose n k) *. p ** (float_of_int k) *. (1. -. p) ** (float_of_int (n - k))

let binomial_cdf (n : int) (p : float) (k : int) : float =
  sigma_tr (fun x -> binomial_pmf n p x) 0 k

let poisson_pmf (l : float) (k : int) : float =
  l ** (float_of_int k) *. exp (-1. *. l) /. (float_of_int (factorial k))

let poisson_cdf  (l : float) (k : int) : float =
  sigma_tr (fun k -> poisson_pmf l k) 0 k

(** [normal_pmf mu sigma x] is the normal([mu],[sigma]) probability mass of [x]
    Requires: [sigma] is a valid std, >= 0 *)
let normal_pmf (mu : float) (sigma : float) (x : float) : float =
  failwith "Unimplemented"
(**exp (-0.5 *. ((x -. mu) /. sigma) ** (2.)) /. *)

(**
*)
let normal_cdf (mu : float) (sigma : float) (x : float) : float =
  failwith "Unimplemented"
