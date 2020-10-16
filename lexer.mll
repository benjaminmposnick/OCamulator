{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = digit? '.' digit*
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let vector_contents = (int | float) (',' (int | float))*
let matrix_contents = vector_contents ';' vector_contents (';' vector_contents)*

rule read = 
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "[" { BEGINARRAY }
  | "]" { ENDARRAY }
  | vector_contents { VECTOR_CONTENTS (Lexing.lexeme lexbuf)}
  | matrix_contents { MATRIX_CONTENTS (Lexing.lexeme lexbuf)}
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { OVER }
  | "%" { MOD }
  | "^" { TOTHEPOWER }
  | "=" { EQUALS }
  | ">" { GT }
  | "<" { LT }
  | ">=" { GTE }
  | "<=" { LTE }
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF }