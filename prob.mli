(** [factorial n] is the factorial of [n]*)
val factorial : int -> int 

(** [choose n k] is [n] choose [k]*)
val choose : int -> int -> float

(** [perm n r] is the nubmer of permuations [n] [r]*)
val perm : int -> int -> float

(** [uniform_pmf a b x] is the probability mass of the uniform
    distribution for interval [a] to [b] at [x]
    Requires: [b] >= [a]*)
val uniform_pmf : float -> float -> float -> float

(** [uniform_cdf a m] is the probability mass of the uniform
    distribution from interval [a] to [b]  *)
val uniform_cdf : float -> float -> float -> float

(** [bernoulli_pmf k p] is the bernoulli([p]) probability mass of k
    Requires: [k] is a valid bernoulli rv, 0 or 1
    [p] is a valid bernolli p, [0,1] *)
val bernoulli_pmf : float -> int -> float 

(** [bernoulli_cdf k p] is the bernoulli([p]) cumulative density of k
    Requires: [p] is a valid bernolli p, [0.1] *)
(**else if k < 1 then 1. -. p *)
val bernoulli_cdf : float -> int -> float

(** [geometric_pmf k p] is the geometric([p]) probability mass of k
    Requires: [k] is a valid geometric rv, rational number
    [p] is a valid geometric p, [0.1] *)
val geometric_pmf : float -> int -> float

(** [geometric_cdf k p] is the geometric([p]) cumulative density of k
    Requires: [k] is a valid geometric rv, rational number
    [p] is a valid geometric p, [0,1] *)
val geometric_cdf : float -> int -> float

(** [exponential_pmf x l] is the exponential([l]) probability mass of x
    Requires: [x] is a valid exponential rv, > 0
    [p] is a valid exponential l, > 0 *)
val exponential_pmf : float -> float -> float

(** [exponential_cdf x l] is the exponential([l]) cumulative density of x
    Requires: [x] is a valid exponential rv, > 0
    [p] is a valid exponential l, > 0 *)
val exponential_cdf : float -> float -> float

(** [binomial_pmf k n p] is the binomial([n],[p]) probability mass of k
    successes of [n] trials with prob [p]
    Requires: k <= n,
    [p] is a valid binomial p, [0.1] *)
val binomial_pmf : int -> float -> int -> float

(** [binomial_cdf k n p] is the binomial([n],[p]) cumulative denisty of k
    successed of [n] trials with prob [p]
    Requires: k >= 0
    [p] is a valid binomial p, [0.1] *)
val binomial_cdf : int -> float -> int -> float

(** [poisson_pmf k l] is the poisson([l]) probability mass of [k]
    Requires: [k] is a valid poisson rv, rational number
    [l] is a valid exponential l, > 0 *)
val poisson_pmf : float -> int -> float

(** [poisson_cdf k l] is the poisson([l]) probability mass of [k]
    Requires: [k] is a valid poisson rv, rational number
    [l] is a valid exponential l, > 0 *)
val poisson_cdf : float -> int -> float 

(** [normal_pmf mu sigma x] is the normal([mu],[sigma]) probability mass of [x]
    Requires: [sigma] is a valid std, >= 0 *)
val normal_pmf : float -> float -> float -> float

(** [normal_cdf mu sigma x] is the normal([mu],[sigma]) cumulative density
     of [x]
    Requires: [sigma] is a valid std, >= 0 *)
val normal_cdf : float -> float -> float -> float

