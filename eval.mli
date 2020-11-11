open Ast

(** [parse str] is a [parsed_input] derived from lexing and parsing the user
    input to the calculator. *)
val parse : string -> parsed_input

(** [modulo p q] is p modulo q.
    Requires: [p] and [q] are floats representing integers. *)
val modulo : float -> float -> float

(** [var_present ast] returns [true] if ast contains a non-numeric character to
    be treated as a variable in an equation, [false] otherwise.
    Requires: input is a valid ast *)
val var_present : expr -> bool

(** [eval parsed_input] is the result of evaluating [parsed_input]. *)
val eval : parsed_input -> Ast.expr