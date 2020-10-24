open Ast

val parse : string -> expr

val modulo : float -> float -> float

(** [var_present ast] returns [true] if ast contains a non-numeric character to
    be treated as a variable in an equation, [false] otherwise.
    Requires: input is a valid ast *)
val var_present : expr -> bool

val eval : expr -> float