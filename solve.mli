(** [solve var e] is the equation [e] solved for Var [var].
    Example: [solve "x" (Binop(Eq, Binop(Mul, Var "x", Int 4), Int 5 ))] is
              (Binop(Eq, Var "x", Binop(Div, Int 5, Int 4))) 
    Requires: [var] is only on one side of the equation [e] *)
val solve : string -> Ast.expr -> Ast.expr

(** [has_var e1 e2 var] is true if [e1] or [e2] contain the Variable
    [var], false otherwise. *)
val has_var : Ast.expr -> Ast.expr -> bool

(** [gcd v1 v2] is the greatest common divisor between the 
    ints [v1] and [v2]. *)
val gcd : int -> int -> int

(** [lcm e1 e2] is the least common multiple between the 
    ints [v1] and [v2]. *)
val lcm : int -> int -> int