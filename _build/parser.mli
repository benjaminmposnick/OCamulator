
(* The type of tokens. *)

type token = 
  | TOTHEPOWER
  | TIMES
  | RPAREN
  | PLUS
  | OVER
  | MOD
  | MINUS
  | LTE
  | LT
  | LPAREN
  | INT of (int)
  | ID of (string)
  | GTE
  | GT
  | FLOAT of (float)
  | EQUALS
  | EOF

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
