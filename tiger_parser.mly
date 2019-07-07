/*Tiger defined by :
http://www.cs.columbia.edu/~sedwards/classes/2002/w4115/tiger.pdf */

%{
open Ast
%}

 /* function calls*/
%token <string> ID
/* Built in types */
%token <int> INT
%token <string> STRING
/* reserved words */
%token LET
%token IN
%token NILL
/* Binary operators */
%token TRUE
%token FALSE
/* Punctuation */
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token LPAR
%token RPAR
%token COLON
%token COMMA
%token PERIOD
%token SEMI_COLON
/* assignment expression lvalue := expr
evaluates the expression then binds its
value to the contents of the lvalue*/
%token EQUALS
/* Binary operators: */
%token ADD
%token MINUS
%token MULT
%token DIV
%token EQ
%token INEQ
%token LESS
%token GREATER
%token LESS_OR
%token GREATER_OR
%token AND
%token OR
/*end of file*/
%token EOF
/* Left applied opperators */
%left ADD
%left MINUS
%left MULT
%left DIV

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }

expr:
  /* | lvalue */
  |	i = INT {Int i}
  | x = ID { Lvalue x }
  |	str = STRING { String str}
  | e1 = expr; ADD; e2 = expr { Add(e1,e2) }
  | e1 = expr; MINUS; e2 = expr { Sub(e1,e2) }
  | e1 = expr; MULT; e2 = expr { Mult(e1,e2) }
	| e1 = expr; DIV; e2 = expr { Div(e1,e2) }
  | LET; x = ID; EQUALS; e1 = expr; IN; e2 = expr { Let( x, e1, e2 ) }
	| LPAR; e = expr; RPAR {e}
	;
  /* |	sequence = ( exp *; )
  |	negation = - exp
  |	funcall = id ( exp *, )
  |	assign = lvalue := exp
  |	ifthenelse = if exp then exp [ else exp ]
  |	ifthen = if exp then exp
  |	while_ = while exp do exp
  |	for_ = for id := exp to exp do exp
  |	break_
  |	let_ = let dec+ in exp *; end */

/*
lvalue	:
	 	id
    |	subscript
    |	fieldexp
subscript	:
	 	lvalue [ exp ]
fieldexp	:
	 	lvalue . id

dec	:
	  | tydec
    |	vardec
    |	fundec
tydec	:
	  type typid = ty
ty	:
	  | typid
    |	arrty
    |	recty
arrty	:
	  	array of typid
recty	:
	  	{ fielddec *, }
fieldec	:
	  	id : typid
vardec	:
	  	var id [ : typid ] := exp
fundec	:
	  	function id ( fielddec *, ) [: typid ] = exp */
