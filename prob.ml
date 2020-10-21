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


let factorial : int -> int = fun num ->
  let rec helper : int -> int -> int = fun n acc ->
    if n > 0
    then helper (n-1) (acc * n)
    else acc
  in
  helper num 1

let choose n k = 
  float_of_int(factorial(n)) /. float_of_int((factorial(k) * factorial(n - k)))

(** [binomial_pmf k n p] is the binomial([n],[p]) probability mass of k
    successed of [n] trials with prob [p]
    Requires: k <= n,
    [p] is a valid binomial p, [0.1]
*)
let binomial_pmf (k : int) (n : int) (p : float) =
  (choose n k) *. p ** (float_of_int k) *. (1. -. p) ** (float_of_int (n - k))


