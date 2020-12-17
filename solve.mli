(** [solve var e] is the equation [e] solved for Var [var].
    Example: [solve "x" (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 ))] is
              (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
    Requires: [var] is only on one side of the equation [e] *)
val solve : string -> Ast.expr -> Ast.expr

(** [has_var e1, e2 var] is true if [e1] or [e2] contain the Variable
    [var], false otherwise. *)
val has_var : Ast.expr -> Ast.expr -> bool