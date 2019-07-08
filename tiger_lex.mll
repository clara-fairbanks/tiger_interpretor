{
open Tiger_parser
}
(*  Regular expressions  *)

let int = '-'? ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let letter = ['a'-'z' 'A'-'Z']
let id = letter+

(* Lexing Rules *)
rule read =
  parse
  | white    { read lexbuf }
  | id       { ID (Lexing.lexeme lexbuf) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }
  | "let"    { LET }
  | "in"     { IN }
  | "="      { EQUALS }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "null"   { NILL }
  | "{"      { LEFT_BRACE }
  | "}"      { RIGHT_BRACE }
  | "["      { LEFT_BRACK }
  | "]"      { RIGHT_BRACK }
  | ":"      { COLON }
  | ","      { COMMA }
  | ";"      { SEMI_COLON }
  | "."      { PERIOD }
  | "-"      { MINUS }
  | "+"      { ADD }
  | "*"      { MULT }
  | "/"      { DIV }
  | "=="     { EQ }
  | "<>"     { INEQ }
  | ">"      { GREATER }
  | "<"      { LESS }
  | "<="     { LESS_OR }
  | ">="     { GREATER_OR }
  | "&&"     { AND }
  | "|"      { OR }
  | eof      { EOF }
