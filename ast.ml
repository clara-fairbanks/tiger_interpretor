(* The type of the abstract syntax tree (AST). *)
type bop =
  | Add
  | Sub
  | Mult
  | Div

type expr =
  | Lvalue of string
  | Int of int
  | Binop of bop * expr * expr
  | Bool of bool
  | String of string
  | ID of string
  | Let of string*expr*expr
