%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token PLUS
%token TIMES  
%token OVER  
%token MOD
%token TOTHEPOWER
%token EQUALS
%token GT
%token LT
%token GTE
%token LTE
%token LPAREN
%token RPAREN
%token BEGINARRAY
%token ENDARRAY
%token <string> VECTOR_CONTENTS
%token <string> MATRIX_CONTENTS
%token MINUS  
%token EOF

%token BINOM
%token BERN
%token UNIF
%token POIS
%token NORM
%token GEO
%token EXP
%token PDF
%token CDF

%nonassoc EQUALS
%nonassoc GT
%nonassoc LT
%nonassoc GTE
%nonassoc LTE
%left PLUS
%left MINUS
%left TIMES
%left OVER  
%left MOD  
%left TOTHEPOWER  

%start <Ast.expr> prog

%%

prog:
	| kw = ID; e = expr; EOF { print_string (kw ^ " "); e }
	| e = expr; EOF { e }
	;
	
expr:
	| x = ID { Var x }
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| MINUS; i = INT { Int (~-i) }
	| MINUS; f = FLOAT { Float (~-.f) }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; TIMES; e2 = expr { Binop (Mul, e1, e2) } 
	| i = INT; x = ID { Binop (Mul, Int i, Var x) } 
	| f = FLOAT; x = ID { Binop (Mul, Float f, Var x) } 
	| e1 = expr; OVER; e2 = expr { Binop (Div, e1, e2) } 
	| e1 = expr; MOD; e2 = expr { Binop (Mod, e1, e2) } 
	| e1 = expr; TOTHEPOWER; e2 = expr { Binop (Pow, e1, e2) } 
	| e1 = expr; EQUALS; e2 = expr { Binop (Eq, e1, e2) } 
	| e1 = expr; GT; e2 = expr { Binop (GT, e1, e2) } 
	| e1 = expr; LT; e2 = expr { Binop (LT, e1, e2) } 
	| e1 = expr; GTE; e2 = expr { Binop (GTE, e1, e2) } 
	| e1 = expr; LTE; e2 = expr { Binop (LTE, e1, e2) } 
	| LPAREN; e=expr; RPAREN { e } 
	| BEGINARRAY; c=VECTOR_CONTENTS; ENDARRAY {
		let num_list = String.split_on_char ',' c in
		Vector (List.map Float.of_string num_list)
	}
	| BEGINARRAY; c=MATRIX_CONTENTS; ENDARRAY {
		let vec_list = String.split_on_char ';' c in
		let num_list = List.map (fun lst -> String.split_on_char ',' lst |> List.map Float.of_string) vec_list in
		Matrix (num_list)
	}
	| BINOM; PDF; n = INT; p = FLOAT; k = INT { Binomial (PDF, n, p, k) } 
	| BINOM; CDF; n = INT; p = FLOAT; k = INT { Binomial (CDF, n, p, k) } 
	| BERN; PDF; p = FLOAT; k = INT { Bernoulli (PDF, p, k) } 
	| BERN; CDF; p = FLOAT; k = INT { Bernoulli (CDF, p, k) } 
	| UNIF; PDF; a = FLOAT; b = FLOAT; x = FLOAT { Uniform (PDF, a, b, x) }
	| UNIF; CDF; a = FLOAT; b = FLOAT; x = FLOAT { Uniform (PDF, a, b, x) }
	| POIS; PDF; l = FLOAT; k = INT { Poisson (PDF, l, k) }
	| POIS; CDF; l = FLOAT; k = INT { Poisson (CDF, l, k) }
	| GEO; PDF; p = FLOAT k = INT { Geometric (PDF, p, k) }
	| GEO; CDF; p = FLOAT;  k = INT { Geometric (CDF, p, k) }
	| EXP; PDF; l = FLOAT; x = FLOAT { Exponential (PDF, l, x) }
	| EXP; CDF; l = FLOAT; x = FLOAT { Exponential (CDF, l, x) }
	| NORM; PDF; m = FLOAT; s = FLOAT; x = FLOAT { Normal (PDF, m, s, x) }
	| NORM; CDF; m = FLOAT; s = FLOAT; x = FLOAT { Normal (CDF, m, s, x) }
	;
	