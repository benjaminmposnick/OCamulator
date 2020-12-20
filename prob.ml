
let pi = acos (-1.)

(** http://alaska-kamtchatka.blogspot.com/2011/12/
    better-gauss-error-function.html*)

let r0 = [|
  3.20937_75891_38469_47256_2e+03, 2.84423_68334_39170_62227_3e+03;
  3.77485_23768_53020_20813_7e+02, 1.28261_65260_77372_27564_5e+03;
  1.13864_15415_10501_55649_5e+02, 2.44024_63793_44441_73305_6e+02;
  3.16112_37438_70565_59694_7e+00, 2.36012_90952_34412_09349_9e+01;
  1.85777_70618_46031_52673_0e-01, 1.00000_00000_00000_00000_0e+00;
|]

let r1 = [|
  1.23033_93547_97997_25272e+03, 1.23033_93548_03749_42043e+03;
  2.05107_83778_26071_46532e+03, 3.43936_76741_43721_63696e+03;
  1.71204_76126_34070_58314e+03, 4.36261_90901_43247_15820e+03;
  8.81952_22124_17690_90411e+02, 3.29079_92357_33459_62678e+03;
  2.98635_13819_74001_31132e+02, 1.62138_95745_66690_18874e+03;
  6.61191_90637_14162_94775e+01, 5.37181_10186_20098_57509e+02;
  8.88314_97943_88375_94118e+00, 1.17693_95089_13124_99305e+02;
  5.64188_49698_86700_89180e-01, 1.57449_26110_70983_47253e+01;
  2.15311_53547_44038_46343e-08, 1.00000_00000_00000_00000e+00;
|]

let r2 = [|
  -6.58749_16152_98378_03157e-04, 2.33520_49762_68691_85443e-03;
  -1.60837_85148_74227_66278e-02, 6.05183_41312_44131_91178e-02;
  -1.25781_72611_12292_46204e-01, 5.27905_10295_14284_12248e-01;
  -3.60344_89994_98044_39429e-01, 1.87295_28499_23460_47209e+00;
  -3.05326_63496_12323_44035e-01, 2.56852_01922_89822_42072e+00;
  -1.63153_87137_30209_78498e-02, 1.00000_00000_00000_00000e+00;
|]

let horner2 r x =
  let n = Array.length r in
  let s = ref 0.
  and t = ref 0. in
  for i = n - 1 downto 0 do
    let p, q = Array.unsafe_get r i in
    s := !s *. x +. p;
    t := !t *. x +. q
  done;
  !s /. !t

let iqpi = 5.64189_58354_77562_86948_1e-01

let erfc x =
  let z  = abs_float x in
  let z2 = z *. z in
  let y =
    if z < 0.46875 then   1. -.         z   *. horner2 r0 z2 else
    if z < 4.      then         exp (-. z2) *. horner2 r1 z  else
      let z'  = 1. /. z  in
      let z'2 = z' *. z' in       exp (-. z2) *. z' *. (iqpi +. z'2 *. horner2 r2 z'2)
  in if x < 0. then 2. -. y else y

let erf x = 1. -. erfc x

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

let normal_pmf (mu : float) (sigma : float) (x : float) : float =
  exp (-0.5 *. ((x -. mu) /. sigma) ** (2.)) /. (sigma *. ((pi *. 2.) ** (0.5)))

let normal_cdf (mu : float) (sigma : float) (x : float) : float =
  let var = (x -. mu) /. (sigma *. 2. ** 0.5) in
  0.5 *. (1. +. erf var)

let get_U () = Random.float 1.

let bernoulli_sam  (p : float) : float  =
  if get_U () < p then 1.
  else 0.

let exponential_sam (l : float) : float =
  -1. *. log (1. -. (get_U ())) /. l

let geo_helper x = log (-1. *. (x -. 1.))

let geometric_sam (p : float) : float =
  ceil (geo_helper (get_U ()) /. geo_helper p)

let uniform_sam (a : float) (b : float) : float =
  (b -. a) *. (get_U ()) +. a

let binomial_sam (n : int) (p : float) : float =
  let rec binomial_helper n p curr = 
    if n = 0 then curr
    else
      let b = bernoulli_sam p in 
      if b = 1. then binomial_helper (n - 1) p (curr +. 1.)
      else binomial_helper (n - 1) p curr
  in binomial_helper n p 0.

let poisson_sam (l : float) (t : float) : float =
  let rec poisson_helper l t curr =
    let e = exponential_sam l in
    if  e > t then curr
    else poisson_helper l (t -. e) (curr +. 1.)
  in poisson_helper l t 0.

let normal_sam (mu : float) (sigma : float) : float =
  let theta = 2. *. pi *. get_U () in 
  let r = (log (get_U ()) *. -2.) ** 0.5 in
  (r *. sin theta) *. sigma +. mu
