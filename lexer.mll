{
open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let float = '-'? digit* '.' digit+
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let comma_sep = white* ',' white*
let semicolon_sep = white* ';' white*
let entry = int | float
let row_vector = '[' entry (comma_sep entry)* ']'
let col_vector = '[' entry (semicolon_sep entry)* ']'
let at_least_2d_row_vector = entry (comma_sep entry)+
let matrix = '[' at_least_2d_row_vector (semicolon_sep at_least_2d_row_vector)+ ']'

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
  | ":=" { ASSIGN }
  | "dot" { DOT }
  | "binom" { BINOM }
  | "binomial" { BINOM }
  | "bern" { BERN }
  | "bernoulli" { BERN }
  | "unif" { UNIF }
  | "uniform" { UNIF }
  | "pois" { POIS }
  | "poisson" { POIS }
  | "exp" { EXP }
  | "exponential"  { EXP }
  | "geo" { GEO }
  | "geometric" { GEO }
  | "norm" { NORM }
  | "normal" { NORM } 
  | "pdf" { PDF }
  | "cdf" { CDF }
  | "pi" { CONST_PI }
  | "$" { BEGIN_CMD }
  | row_vector { ROW_VECTOR (Lexing.lexeme lexbuf)}
  | col_vector { COL_VECTOR (Lexing.lexeme lexbuf)}
  | matrix { MATRIX (Lexing.lexeme lexbuf)}
  | id { ID (Lexing.lexeme lexbuf) }
  | eof { EOF; }
