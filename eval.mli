(** [ComputationError] is the module for interpreter errors. *)
module ComputationError : sig
    (** [EvalError msg] is the exception raised when the interpreter encounters
        a runtime error. *)
    exception EvalError of string
end

(** [var_present ast] is [true] if [ast] contains a non-numeric character to be
    treated as a variable in an equation and is [false] otherwise.
    Requires: [ast] is a valid ast *)
val var_present : Ast.expr -> bool


(** [eval_numeric e sigma] is the result of evaluating expression [e] in the
    store represented by [sigma]. *)
val eval_expr : Ast.expr -> (string * Ast.value) list ->
    (Ast.value * (string * Ast.value) list)





val eval_input : Ast.expr -> (string * Ast.value) list ->
    (Ast.value * (string * Ast.value) list)