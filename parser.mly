%{
	open Ast
	open Vector
	open Matrix

	(** [vector_as_string_list str sep] is the list of strings generated from 
			splitting [str] at [sep]. *)
	let vector_as_string_list str sep =
		(* Splice out left and right brackets *)
		let contents = String.sub str 1 (String.length str - 2) in
		String.split_on_char sep contents

	(** [vector_as_float_list str sep] is the list of floats generated from 
			splitting [str] at [sep] and converting all strings to floats. *)
	let vector_as_float_list str sep =
		let string_list = vector_as_string_list str sep in	
		List.map Float.of_string string_list
%}

// Primitve values
%token <int> INT
%token <float> FLOAT
%token <string> ID
%token <string> ROW_VECTOR
%token <string> COL_VECTOR
%token <string> MATRIX

// Binary operators
%token PLUS MINUS TIMES OVER MOD TOTHEPOWER EQUALS GT LT GTE LTE ASSIGN DOT SOLVE

// Probability
%token BINOM BERN UNIF POIS NORM GEO EXP PDF CDF SAM

// Miscellaenous
%token LPAREN RPAREN CONST_PI EOF BEGIN_CMD PROJ TILDE

%left ID
%right EQUALS GT LT GTE LTE ASSIGN DOT MOD
%left PLUS MINUS
%left TIMES OVER  
%left TOTHEPOWER

%start <Ast.expr> prog

%%

prog:
	| e = expr; EOF { e }
	;
	
expr :
	| LPAREN; e=expr; RPAREN { e } 
	| ID { Var $1 }
	| arith_expr { $1 }
	| prob_expr { $1 }
	| array_expr { $1 }
	| LPAREN; e1 = expr; TILDE; e2 = expr; RPAREN { Tuple (e1, e2)}
	| BEGIN_CMD; cmd = ID; e = expr { Command (cmd, e) }
	| PROJ; i = INT; e = expr { Command ("#" ^ string_of_int i, e)}
	;

num :
	| i = INT { Int i }
	| f = FLOAT { Float f }
	| CONST_PI { Float (Float.pi) }
	| MINUS; i = INT { Int (~-i) }
	| MINUS; f = FLOAT { Float (~-.f) }
	;

%inline binop :
	| PLUS { Add }
	| MINUS { Sub }
	| TIMES { Mul }
	| OVER { Div }
	| MOD { Mod }
	| TOTHEPOWER { Pow }
	| EQUALS { Eq }
	| GT { GT }
	| LT { LT }
	| GTE { GTE }
	| LTE { LTE }
	| ASSIGN { Assign }
	| DOT { Dot }
	| SOLVE { SolveSys }
	;

arith_expr :
	| e1 = expr; bop = binop; e2 = expr { Binop (bop, e1, e2) }
	| n = num; x = ID { Binop (Mul, n, Var x) } 
	| num { $1 }
	;
	
array_expr :
	| v=ROW_VECTOR {
			let vals = vector_as_float_list v ',' in
			Vector (Vector.make_row_vec vals)
		}
	| v=COL_VECTOR {
			let vals = vector_as_float_list v ';' in
			Vector (Vector.make_col_vec vals)
		}
	| m=MATRIX {
			let vals = vector_as_string_list m ';' in
			let num_list = 
				(fun lst -> String.split_on_char ',' lst |> List.map Float.of_string)
				|> fun fn -> List.map fn vals
			in
			Matrix (Matrix.of_list num_list)
		}

prob_input :
	| i = INT { float_of_int i }
	| f = FLOAT { f }
	| CONST_PI { Float.pi }
	;

prob_expr :
	| BINOM; SAM; n = prob_input; p = prob_input;
		{ Prob (Binomial (SAM, n, p, 0.)) } 
	| BINOM; fn = prob_func; n = prob_input; p = prob_input; k = prob_input
		{ Prob (Binomial (fn, n, p, k)) } 

	| BERN; SAM; p = prob_input;
		{ Prob (Bernoulli (SAM, p, 0.)) }
	| BERN; fn = prob_func; p = prob_input; k = prob_input
		{ Prob (Bernoulli (fn, p, k)) }

	| UNIF; SAM; a = prob_input; b = prob_input;
		{ Prob (Uniform (SAM, a, b, 0.)) }
	| UNIF; fn = prob_func; a = prob_input; b = prob_input; x = prob_input
		{ Prob (Uniform (fn, a, b, x)) }

	| POIS; SAM; l = prob_input;
	 	{ Prob (Poisson (SAM, l, 0.)) }
	| POIS; fn = prob_func; l = prob_input; k = prob_input 
	 	{ Prob (Poisson (fn, l, k)) }

	| GEO; SAM; p = prob_input;
		{ Prob (Geometric (SAM, p, 0.)) }
	| GEO; fn = prob_func; p = prob_input; k = prob_input
		{ Prob (Geometric (fn, p, k)) }


	| EXP; SAM; l = prob_input;
		{ Prob (Exponential (SAM, l, 0.)) }
	| EXP; fn = prob_func; l = prob_input; x = prob_input
		{ Prob (Exponential (fn, l, x)) }

	| NORM; SAM; m = prob_input; s = prob_input;
		{ Prob (Normal (SAM, m, s, 0.)) }
	| NORM; fn = prob_func; m = prob_input; s = prob_input; x = prob_input
		{ Prob (Normal (fn, m, s, x)) }
	;

%inline prob_func :
	| PDF { PDF }
	| CDF { CDF }
	| SAM { SAM }
