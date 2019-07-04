type bop =
  | ADD
  | MINUS
  | MULT
  | DIV
  | EQ
  | INEQ
  | LESS
  | GREATER
  | LESS_OR
  | GREATER_OR
  | AND
  | OR

type expr =
  | String of string
  | Int of int
  | Bool of bool
  | Binop of bop * expr * expr
  | Let of string * expr * expr
  | If of expr * expr * expr
  | ID of string
