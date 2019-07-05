open Ast

let is_value : expr -> bool = function
  | Int _ -> true
  | Add _ | Sub _ | Mult _ | Div _ | Var _ | Let _ -> false

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Tiger_parser.prog Lexer.read lexbuf in
  ast

let get_value = function
  | Int i -> i
  | _ -> failwith "No value"
