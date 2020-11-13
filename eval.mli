(** [parse str] is the abstract syntax tree that results from lexing and 
    parsing [str], which is the user input to the calculator. *)
val parse : string -> Ast.parsed_input

(** [modulo p q] is [p] mod [q].
    Requires: [p] and [q] are floats representing integers. *)
val modulo : float -> float -> float

(** [var_present ast] returns [true] if [ast] contains a non-numeric character
    to be treated as a variable in an equation, [false] otherwise.
  Requires: input is a valid ast *)
val var_present : Ast.expr -> bool

(** [eval_numeric e sigma] is the result of evaluating expression [e] in the
    store represented by [sigma]. *)
val eval_numeric : Ast.expr -> (string * Ast.expr) list -> float

(** [eval_array arr op sigma] is the result of operation [op] on array [arr] in
    the store represented by [sigma]. *)
val eval_array : Ast.array -> string -> (string * Ast.expr) list -> Ast.array

(** [eval parsed_input sigma] is the result of evaluating [parsed_input] in the
store represented by [sigma]. *)
val eval_input : Ast.parsed_input -> (string * Ast.expr) list -> Ast.expr -> (Ast.expr * (string * Ast.expr) list)