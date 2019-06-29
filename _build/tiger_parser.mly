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
%token PLUS
%token MINUS
%token MULT
%token DIV
%token EQUAL
%token INEQUAL
%token LESS
%token GREATER
%token LESS_OR
%token GREATER_OR
%token AND
%token OR
%token EOF

prog:
  | exp

exp:
  | lvalue
  |	intlit
  |	stringlit
  |	sequence
  |	negation
  |	funcall
  |	infix
  |	arrCreate
  |	recCreate
  |	assign
  |	ifthenelse
  |	ifthen
  |	while
  |	for
  |	break
  |	let

sequence	:
  ( exp *; )
negation	:
	 - exp
funcall	:
	 	id ( exp *, )
infix	:
	 	exp infixop exp
arrCreate	:
	 	typid [ exp ] of exp
recCreate	:
	 	typid { field *, }
field	:
	 	id = exp
assign	:
	 	lvalue := exp
ifthenelse  :
	 	if exp then exp [ else exp ]
while	:
	 	while exp do exp
for	:
	 	for id := exp to exp do exp
let	:
	 	let dec+ in exp *; end
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
