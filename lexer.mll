{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float = digit* '.' digit+
let int = digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let comma_sep = white* ',' white*
let semicolon_sep = white* ';' white*
let entry = int | float
let vector = '[' entry (comma_sep entry)* ']'
let at_least_2d_vector = entry (comma_sep entry)+
let matrix = '[' at_least_2d_vector (semicolon_sep at_least_2d_vector)+ ']'

rule read = 
  parse
  | white { read lexbuf }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "(" { LPAREN }
  | ")" { RPAREN }
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
  | "pi" { CONST_PI }
  | vector { VECTOR (Lexing.lexeme lexbuf)}
  | matrix { MATRIX (Lexing.lexeme lexbuf)}
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF; }