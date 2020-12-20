open Ast

(** [store] is the type of the variable store, which maps variable identifiers
    to their most recently bound value. *)
type store = (string * value) list

(** [result] is the type of the result from evaluation, which contains both
    the value resulting from evaluation under the big-step relation as well
    as the store. *)
type result = value * store

(** [ComputationError] is the module for interpreter errors. *)
module ComputationError : sig
    (** [EvalError msg] is the exception raised when the interpreter encounters
        a runtime error. *)
    exception EvalError of string
end

(** [var_present ast] is [true] if [ast] contains a non-numeric character to be
    treated as a variable in an equation and is [false] otherwise. *)
val var_present : expr -> bool

(** [eval_prob dist sigma] is the result of evaluating the probability
distribution [dist] in store [sigma]. *)
val eval_prob : distribution -> store -> result

(** [eval_expr e sigma] is the result of evaluating expression [e] in
    store [sigma]. If an error occurs during evaluation,
    [ComputationError.EvalError] is raised instead.*)
val eval_expr : expr -> store -> result

(** [eval_input e sigma] is the result of evaluating expression [e] in
    store [sigma]. The store that results from the evaluation of [e] is also 
    updated such that the value to which [e] evaluates is bound to variable
    [ans]. If an error occurs during evaluation, [ComputationError.EvalError]
    is raised instead.*)
val eval_input : expr -> store -> result
