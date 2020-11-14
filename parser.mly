%{
open Ast
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID
%token CONST_PI
%token END_KW
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
%token ASSIGN
%token LPAREN
%token RPAREN
%token <string> ROW_VECTOR
%token <string> COL_VECTOR
%token <string> MATRIX
%token MINUS  
%token ANS
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

%start <Ast.parsed_input> prog

%%

prog:
	| e = expr; EOF { Expression e }
	| kw = ID; END_KW; e = expr; EOF { Command (kw, e) }
	;
	
expr:
	| x = ID { Var x }
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| CONST_PI { Float (Float.pi) }
	| MINUS; i = INT { Int (~-i) }
	| MINUS; f = FLOAT { Float (~-.f) }
	| e1 = expr; PLUS; e2 = expr { Binop (Add, e1, e2) }
	| e1 = expr; MINUS; e2 = expr { Binop (Sub, e1, e2) } 
	| e1 = expr; TIMES; e2 = expr { Binop (Mul, e1, e2) } 
	| i = INT; x = ID { Binop (Mul, Int i, Var x) } 
	| f = FLOAT; x = ID { Binop (Mul, Float f, Var x) } 
	| MINUS; i = INT; x = ID { Binop (Mul, Int ~-i, Var x) } 
	| MINUS; f = FLOAT; x = ID { Binop (Mul, Float ~-.f, Var x) } 
	| e1 = expr; OVER; e2 = expr { Binop (Div, e1, e2) } 
	| e1 = expr; MOD; e2 = expr { Binop (Mod, e1, e2) } 
	| e1 = expr; TOTHEPOWER; e2 = expr { Binop (Pow, e1, e2) } 
	| e1 = expr; EQUALS; e2 = expr { Binop (Eq, e1, e2) } 
	| e1 = expr; GT; e2 = expr { Binop (GT, e1, e2) } 
	| e1 = expr; LT; e2 = expr { Binop (LT, e1, e2) } 
	| e1 = expr; GTE; e2 = expr { Binop (GTE, e1, e2) } 
	| e1 = expr; LTE; e2 = expr { Binop (LTE, e1, e2) } 
	| e1 = expr; ASSIGN; e2 = expr { Binop (Assign, e1, e2) } 
	| e1 = expr; ASSIGN; ANS { Binop (Assign, e1, Ans) } 
	| LPAREN; e=expr; RPAREN { e } 
	| v=ROW_VECTOR {
		let v' = String.sub v 1 (String.length v - 2) in
		let num_list = String.split_on_char ',' v' in
		NumArray (RowVector (List.map Float.of_string num_list))
	}
	| v=COL_VECTOR {
		let v' = String.sub v 1 (String.length v - 2) in
		let num_list = String.split_on_char ';' v' in
		NumArray (ColumnVector (List.map Float.of_string num_list))
	}
	| m=MATRIX {
		let m' = String.sub m 1 (String.length m - 2) in
		let vec_list = String.split_on_char ';' m' in
		let num_list = List.map (fun lst -> String.split_on_char ',' lst |> List.map Float.of_string) vec_list in
		NumArray (Matrix (num_list))
	}
	| BINOM; PDF; n = INT; p = FLOAT; k = INT { Binomial (PDF, n, p, k) } 
	| BINOM; CDF; n = INT; p = FLOAT; k = INT { Binomial (CDF, n, p, k) } 
	| BERN; PDF; p = FLOAT; k = INT { Bernoulli (PDF, p, k) } 
	| BERN; CDF; p = FLOAT; k = INT { Bernoulli (CDF, p, k) } 
	| UNIF; PDF; a = FLOAT; b = FLOAT; x = FLOAT { Uniform (PDF, a, b, x) }
	| UNIF; CDF; a = FLOAT; b = FLOAT; x = FLOAT { Uniform (CDF, a, b, x) }
	| POIS; PDF; l = FLOAT; k = INT { Poisson (PDF, l, k) }
	| POIS; CDF; l = FLOAT; k = INT { Poisson (CDF, l, k) }
	| GEO; PDF; p = FLOAT k = INT { Geometric (PDF, p, k) }
	| GEO; CDF; p = FLOAT;  k = INT { Geometric (CDF, p, k) }
	| EXP; PDF; l = FLOAT; x = FLOAT { Exponential (PDF, l, x) }
	| EXP; CDF; l = FLOAT; x = FLOAT { Exponential (CDF, l, x) }
	| NORM; PDF; m = FLOAT; s = FLOAT; x = FLOAT { Normal (PDF, m, s, x) }
	| NORM; CDF; m = FLOAT; s = FLOAT; x = FLOAT { Normal (CDF, m, s, x) }
;
	