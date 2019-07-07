
(* The type of tokens. *)

type token = 
  | TRUE
  | STRING of (string)
  | SEMI_COLON
  | RPAR
  | RIGHT_BRACK
  | RIGHT_BRACE
  | PERIOD
  | OR
  | NILL
  | MULT
  | MINUS
  | LPAR
  | LET
  | LESS_OR
  | LESS
  | LEFT_BRACK
  | LEFT_BRACE
  | INT of (int)
  | INEQ
  | IN
  | ID of (string)
  | GREATER_OR
  | GREATER
  | FALSE
  | EQUALS
  | EQ
  | EOF
  | DIV
  | COMMA
  | COLON
  | AND
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
