%{
open Ast
%}

%token <string> ID
%token <string> WHITESPACE
%token <string> COMMENT
%token <int> INTLIT
%token <string> STRINGLIT
%token TRUE
%token FALSE
%token NILL
%token LEFT_BRACE
%token RIGHT_BRACE
%token LEFT_BRACK
%token RIGHT_BRACK
%token LEFT_PAR
%token RIGHT_PAR
%token COLON
%token COMMA
%token PERIOD
%token SEMI_COLON
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
%token EOF

%left ADD
%left MINUS
%left MULT
%left DIV

%start <Ast.expr> prog
%%

prog:
  | e = expr; EOF { e }

exp:
  | lvalue
  |	i = intlit  {Int}
  |	x = stringlit {String}
  |	sequence = ( exp *; )
  |	negation = - exp
  |	funcall = id ( exp *, )
  |	assign = lvalue := exp
  |	ifthenelse = if exp then exp [ else exp ]
  |	ifthen = if exp then exp
  |	while_ = while exp do exp
  |	for_ = for id := exp to exp do exp
  |	break_
  |	let_ = let dec+ in exp *; end


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
	  	function id ( fielddec *, ) [: typid ] = exp
