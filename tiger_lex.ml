# 3 "tiger_lex.mll"
 
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

# 16 "tiger_lex.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base =
   "\000\000\227\255\228\255\229\255\001\000\002\000\003\000\236\255\
    \237\255\238\255\239\255\241\255\242\255\243\255\244\255\245\255\
    \246\255\247\255\248\255\249\255\000\000\000\000\000\000\018\000\
    \028\000\254\255\001\000\003\000\001\000\000\000\252\255\000\000\
    \000\000\002\000\251\255\001\000\003\000\250\255\232\255\235\255\
    \231\255\230\255\002\000\245\255\003\000\039\000\255\255\248\255\
    \249\255\250\255\251\255\252\255\253\255\254\255";
  Lexing.lex_backtrk =
   "\255\255\255\255\255\255\255\255\027\000\021\000\022\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\027\000\027\000\027\000\002\000\
    \015\000\255\255\001\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\008\000\009\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255";
  Lexing.lex_default =
   "\002\000\000\000\000\000\000\000\255\255\255\255\255\255\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \255\255\000\000\255\255\255\255\255\255\255\255\000\000\255\255\
    \255\255\255\255\000\000\255\255\255\255\000\000\000\000\000\000\
    \000\000\000\000\044\000\000\000\044\000\255\255\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000";
  Lexing.lex_trans =
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\027\000\025\000\025\000\027\000\026\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \027\000\000\000\019\000\027\000\046\000\255\255\004\000\041\000\
    \000\000\000\000\009\000\010\000\013\000\024\000\011\000\008\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\014\000\012\000\006\000\007\000\005\000\040\000\
    \038\000\039\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\023\000\023\000\053\000\000\000\
    \000\000\000\000\000\000\016\000\000\000\015\000\045\000\255\255\
    \000\000\031\000\000\000\000\000\000\000\030\000\021\000\034\000\
    \000\000\000\000\000\000\000\000\032\000\036\000\020\000\037\000\
    \000\000\000\000\028\000\033\000\022\000\035\000\029\000\000\000\
    \000\000\000\000\000\000\018\000\003\000\017\000\000\000\000\000\
    \000\000\000\000\000\000\052\000\000\000\000\000\000\000\000\000\
    \000\000\051\000\000\000\000\000\000\000\050\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\049\000\000\000\000\000\
    \000\000\048\000\000\000\047\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \001\000\000\000\043\000\255\255\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    ";
  Lexing.lex_check =
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\026\000\027\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\000\000\027\000\042\000\044\000\000\000\004\000\
    \255\255\255\255\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\005\000\
    \006\000\006\000\023\000\023\000\023\000\023\000\023\000\023\000\
    \023\000\023\000\023\000\023\000\024\000\024\000\024\000\024\000\
    \024\000\024\000\024\000\024\000\024\000\024\000\045\000\255\255\
    \255\255\255\255\255\255\000\000\255\255\000\000\042\000\044\000\
    \255\255\021\000\255\255\255\255\255\255\029\000\000\000\033\000\
    \255\255\255\255\255\255\255\255\031\000\035\000\000\000\036\000\
    \255\255\255\255\022\000\032\000\000\000\020\000\028\000\255\255\
    \255\255\255\255\255\255\000\000\000\000\000\000\255\255\255\255\
    \255\255\255\255\255\255\045\000\255\255\255\255\255\255\255\255\
    \255\255\045\000\255\255\255\255\255\255\045\000\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\045\000\255\255\255\255\
    \255\255\045\000\255\255\045\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\042\000\044\000\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    ";
  Lexing.lex_base_code =
   "";
  Lexing.lex_backtrk_code =
   "";
  Lexing.lex_default_code =
   "";
  Lexing.lex_trans_code =
   "";
  Lexing.lex_check_code =
   "";
  Lexing.lex_code =
   "";
}

let rec read lexbuf =
   __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 31 "tiger_lex.mll"
             ( read lexbuf )
# 141 "tiger_lex.ml"

  | 1 ->
# 32 "tiger_lex.mll"
             ( next_line lexbuf; read lexbuf )
# 146 "tiger_lex.ml"

  | 2 ->
# 33 "tiger_lex.mll"
             ( INTLIT (int_of_string (Lexing.lexeme lexbuf)) )
# 151 "tiger_lex.ml"

  | 3 ->
# 34 "tiger_lex.mll"
             ( TRUE )
# 156 "tiger_lex.ml"

  | 4 ->
# 35 "tiger_lex.mll"
             ( FALSE )
# 161 "tiger_lex.ml"

  | 5 ->
# 36 "tiger_lex.mll"
             ( NILL )
# 166 "tiger_lex.ml"

  | 6 ->
# 37 "tiger_lex.mll"
             ( read_string (Buffer.create 17) lexbuf )
# 171 "tiger_lex.ml"

  | 7 ->
# 38 "tiger_lex.mll"
             ( LEFT_BRACE )
# 176 "tiger_lex.ml"

  | 8 ->
# 39 "tiger_lex.mll"
             ( RIGHT_BRACE )
# 181 "tiger_lex.ml"

  | 9 ->
# 40 "tiger_lex.mll"
             ( LEFT_BRACK )
# 186 "tiger_lex.ml"

  | 10 ->
# 41 "tiger_lex.mll"
             ( RIGHT_BRACK )
# 191 "tiger_lex.ml"

  | 11 ->
# 42 "tiger_lex.mll"
             ( COLON )
# 196 "tiger_lex.ml"

  | 12 ->
# 43 "tiger_lex.mll"
             ( COMMA )
# 201 "tiger_lex.ml"

  | 13 ->
# 44 "tiger_lex.mll"
             ( SEMI_COLON )
# 206 "tiger_lex.ml"

  | 14 ->
# 45 "tiger_lex.mll"
             ( PERIOD )
# 211 "tiger_lex.ml"

  | 15 ->
# 46 "tiger_lex.mll"
             ( MINUS )
# 216 "tiger_lex.ml"

  | 16 ->
# 47 "tiger_lex.mll"
             ( PLUS )
# 221 "tiger_lex.ml"

  | 17 ->
# 48 "tiger_lex.mll"
             ( MULT )
# 226 "tiger_lex.ml"

  | 18 ->
# 49 "tiger_lex.mll"
             ( DIV )
# 231 "tiger_lex.ml"

  | 19 ->
# 50 "tiger_lex.mll"
             ( EQUAL )
# 236 "tiger_lex.ml"

  | 20 ->
# 51 "tiger_lex.mll"
             ( INEQUAL )
# 241 "tiger_lex.ml"

  | 21 ->
# 52 "tiger_lex.mll"
             ( GREATER )
# 246 "tiger_lex.ml"

  | 22 ->
# 53 "tiger_lex.mll"
             ( LESS )
# 251 "tiger_lex.ml"

  | 23 ->
# 54 "tiger_lex.mll"
             ( LESS_OR )
# 256 "tiger_lex.ml"

  | 24 ->
# 55 "tiger_lex.mll"
             ( GREATER_OR )
# 261 "tiger_lex.ml"

  | 25 ->
# 56 "tiger_lex.mll"
             ( AND )
# 266 "tiger_lex.ml"

  | 26 ->
# 57 "tiger_lex.mll"
             ( OR )
# 271 "tiger_lex.ml"

  | 27 ->
# 58 "tiger_lex.mll"
      ( raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) )
# 276 "tiger_lex.ml"

  | 28 ->
# 59 "tiger_lex.mll"
             ( EOF )
# 281 "tiger_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

and read_string buf lexbuf =
   __ocaml_lex_read_string_rec buf lexbuf 42
and __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 63 "tiger_lex.mll"
                ( STRING (Buffer.contents buf) )
# 293 "tiger_lex.ml"

  | 1 ->
# 64 "tiger_lex.mll"
                ( Buffer.add_char buf '/'; read_string buf lexbuf )
# 298 "tiger_lex.ml"

  | 2 ->
# 65 "tiger_lex.mll"
                ( Buffer.add_char buf '\\'; read_string buf lexbuf )
# 303 "tiger_lex.ml"

  | 3 ->
# 66 "tiger_lex.mll"
                ( Buffer.add_char buf '\b'; read_string buf lexbuf )
# 308 "tiger_lex.ml"

  | 4 ->
# 67 "tiger_lex.mll"
                ( Buffer.add_char buf '\012'; read_string buf lexbuf )
# 313 "tiger_lex.ml"

  | 5 ->
# 68 "tiger_lex.mll"
                ( Buffer.add_char buf '\n'; read_string buf lexbuf )
# 318 "tiger_lex.ml"

  | 6 ->
# 69 "tiger_lex.mll"
                ( Buffer.add_char buf '\r'; read_string buf lexbuf )
# 323 "tiger_lex.ml"

  | 7 ->
# 70 "tiger_lex.mll"
                ( Buffer.add_char buf '\t'; read_string buf lexbuf )
# 328 "tiger_lex.ml"

  | 8 ->
# 72 "tiger_lex.mll"
      ( Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      )
# 335 "tiger_lex.ml"

  | 9 ->
# 75 "tiger_lex.mll"
        ( raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) )
# 340 "tiger_lex.ml"

  | 10 ->
# 76 "tiger_lex.mll"
          ( raise (SyntaxError ("String is not terminated")) )
# 345 "tiger_lex.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf;
      __ocaml_lex_read_string_rec buf lexbuf __ocaml_lex_state

;;
