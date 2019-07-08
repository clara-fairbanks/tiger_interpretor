(* The type of the abstract syntax tree (AST). *)
type expr =
  | Lvalue of string
  | Int of int
  | Bool of bool
  | Add of expr*expr
  | Sub of expr*expr
  | Mult of expr*expr
  | Div of expr*expr
  | String of string
  | ID of string
  | Let of string*expr*expr
