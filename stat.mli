(** [Stat] is the module for statistical of vectors, including measures of
    central tendency and linear regression. *)

(** [sort_asc d] is [d] sorted in ascending order*)
val sort_asc : float list -> float list

(** [sort_desc d] is [d] sorted in descending order*)
val sort_desc : float list -> float list

(** [cum_sum d] is the cumulative sum of all values in [d]*)
val cum_sum : float list -> float

(** [cum_prod d] is the cumulative prod of all values in [d]*)
val cum_prod : float list -> float

(** [mean d] is mean of the values in [d]*)
val mean : float list -> float

(** [median d] is median of the values in [d]*)
val median : float list -> float

(** [mode d] is mode of the values in [d]*)
val mode : float list -> float

(** [range d] is the range of the values in [d]. max [d] - min [d]*)
val range : float list -> float

(** [rms d] is the root mean squared of [d]*)
val rms : float list -> float

(** [linear_regression d] is the line of best fit for the points (x,y)
    in [d] using a cartesian coordinate system*)
val linear_regression : (float * float) list -> float * float 

(** [quantile d q] is the [q]th quantile of [d]
    Requires: q is a valid quantile, [0,1]*)
val quantile : float list -> float -> float

(** [max d] is the max of the values in [d]*)
val max : float list -> float

(** [min d] is the min of the values in [d]*)
val min : float list -> float

(** [smpl_var d] is the sample variance of the values in [d]*)
val smpl_var : float list -> float

(** [smpl_std d] is the sample standard deviation of the values in [d]*)
val smpl_std : float list -> float

(** [count v d] is the number of occurences of [v] in [d]*)
val count : float -> float list -> float

(** [unique d] is the set of [d]. All duplicated removed*)
val unique : float list -> float list

