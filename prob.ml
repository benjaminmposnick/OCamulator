let reduce_tr inc test op base f x =
  let rec apply_tr acc x =
    if test x then acc
    else apply_tr (op (f x) acc) (inc x)
  in apply_tr base x

let sigma_tr f a b =
  reduce_tr (fun x -> x + 1) 
    (fun x -> if x = b then true else false) ( +. ) (f b) f a

let factorial : int -> int = fun num ->
  let rec helper : int -> int -> int = fun n acc ->
    if n > 0
    then helper (n-1) (acc * n)
    else acc
  in
  helper num 1

let choose n k = 
  float_of_int(factorial(n)) /. float_of_int((factorial(k) * factorial(n - k)))

(** [bernoulli_pmf k p] is the bernoulli([p]) probability mass of k
    Requires: [k] is a valid bernoulli rv, 0 or 1
    [p] is a valid bernolli p, [0,1]
*)
let bernoulli_pmf (k : int) (p : float) =
  if k = 1 then p else 1. -. p

(** [bernoulli_cdf k p] is the bernoulli([p]) cumulative density of k
    Requires: [p] is a valid bernolli p, [0.1]
*)
let bernoulli_cdf (k : float) (p : float) =
  if k < 0. then 0.
  else if k < 1. then 1. -. p
  else 1.

(** [geometric_pmf k p] is the geometric([p]) probability mass of k
    Requires: [k] is a valid geometric rv, rational number
    [p] is a valid geometric p, [0.1]
*)
let geometric_pmf (k : int) (p : float) =
  p *. (1. -. p) ** (float_of_int (k - 1))

(** [geometric_cdf k p] is the geometric([p]) cumulative density of k
    Requires: [k] is a valid geometric rv, rational number
    [p] is a valid geometric p, [0,1]
*)
let geometric_cdf (k : int) (p : float) =
  1. -. (1. -. p) ** (float_of_int k)

(** [exponential_pmf x l] is the exponential([l]) probability mass of x
    Requires: [x] is a valid exponential rv, > 0
    [p] is a valid exponential l, > 0
*)
let exponential_pmf (x : float) (l : float) =
  l *. exp (x *. l)

(** [exponential_cdf x l] is the exponential([l]) cumulative density of x
    Requires: [x] is a valid exponential rv, > 0
    [p] is a valid exponential l, > 0
*)
let exponential_cdf (x : float) (l : float) =
  1. -. exp (-1. *. l *. x)

(** [binomial_pmf k n p] is the binomial([n],[p]) probability mass of k
    successes of [n] trials with prob [p]
    Requires: k <= n,
    [p] is a valid binomial p, [0.1]
*)
let binomial_pmf (k : int) (n : int) (p : float) =
  (choose n k) *. p ** (float_of_int k) *. (1. -. p) ** (float_of_int (n - k))

(** [binomial_cdf k n p] is the binomial([n],[p]) vumulative denisty of k
    successed of [n] trials with prob [p]
    Requires: k >= 0
    [p] is a valid binomial p, [0.1]
*)
let binomial_cdf (k : int) (n : int) (p : float) =
  sigma_tr (fun x -> binomial_pmf x n p) 0 k

(** [poisson_pmf k l] is the poisson([l]) probability mass of [k]
    Requires: [k] is a valid poisson rv, rational number
    [l] is a valid exponential l, > 0
*)
let poisson_pmf (k : int) (l : float) =
  l ** (float_of_int k) *. exp (-1. *. l) /. (float_of_int (factorial k))

(** [poisson_cdf k l] is the poisson([l]) probability mass of [k]
    Requires: [k] is a valid poisson rv, rational number
    [l] is a valid exponential l, > 0
*)
let poisson_cdf (k : int) (l : float) =
  sigma_tr (fun k -> poisson_pmf k l) 0 k

(** [normal_pmf mu sigma x] is the normal([mu],[sigma]) probability mass of [x]
    Requires: [sigma] is a valid std, >= 0
*)
let normal_pmf (x : float) (mu : float) (sigma : float) =
  exp (-0.5 *. ((x -. mu) /. sigma) ** (2.))


