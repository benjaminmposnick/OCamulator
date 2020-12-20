(** [solve var e] is the equation [e] solved for Var [var].
    Example: [solve "x" (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 ))] is
              (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
    Requires: [var] is only on one side of the equation [e] *)
val solve : string -> Ast.expr -> Ast.expr

(** [has_var e1 e2 var] is true if [e1] or [e2] contain the Variable
    [var], false otherwise. *)
val has_var : Ast.expr -> Ast.expr -> bool

(** [gcd e1 e2] is the greatest common divisor between the fractions
    represented by expressions e1 and e2.
    Requires: e1 and e2 are Binops with Div operators *)
val gcd : int -> int -> int

(** [lcd e1 e2] is the least common between the fractions
    represented by expressions e1 and e2.
    Requires: e1 and e2 are Binops with Div operators *)
(* val lcd : Ast.expr -> Ast.expr -> int *)