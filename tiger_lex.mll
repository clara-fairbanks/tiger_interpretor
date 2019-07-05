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
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* Lexing Rules *)

rule read =
  parse
  | white    { read lexbuf }
  | id       { ID (Lexing.lexeme lexbuf) }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof      { EOF }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | "null"   { NILL }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | "{"      { LEFT_BRACE }
  | "}"      { RIGHT_BRACE }
  | "["      { LEFT_BRACK }
  | "]"      { RIGHT_BRACK }
  | ":"      { COLON }
  | ","      { COMMA }
  | ";"      { SEMI_COLON }
  | "."      { PERIOD }
  | "-"      { MINUS }
  | "+"      { PLUS }
  | "*"      { MULT }
  | "/"      { DIV }
  | "="      { EQUAL }
  | "<>"     { INEQUAL }
  | ">"      { GREATER }
  | "<"      { LESS }
  | "<="     { LESS_OR }
  | ">="     { GREATER_OR }
  | "&&"     { AND }
  | "|"      { OR }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }
