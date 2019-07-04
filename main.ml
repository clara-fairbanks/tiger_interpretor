open Ast

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Tiger_parser.prog Lexer.read lexbuf in
  ast
