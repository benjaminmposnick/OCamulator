(** [modulo p q] is [p] mod [q] if [p] and [q] are floats representing integers
    (e.g. 1.0 for 1) and raises [TypeError] otherwise. *)
val modulo : float -> float -> float

(** [eval_numeric e sigma] is the result of evaluating expression [e] in the
    store represented by [sigma]. *)
val eval_expr : Ast.expr -> (string * Ast.value) list ->
    (Ast.value * (string * Ast.value) list)

(** [var_present ast] returns [true] if [ast] contains a non-numeric character
    to be treated as a variable in an equation, [false] otherwise.
  Requires: input is a valid ast *)
  val var_present : Ast.expr -> bool

module EvalError : sig
    exception UnboundVariable of string
    exception TypeError of string
end

val eval_input : Ast.expr -> (string * Ast.value) list ->
    (Ast.value * (string * Ast.value) list)