open Lexing


let debug_pos b e tok =
  assert (b.pos_fname = e.pos_fname);
  Printf.printf "%s:[%d:%d-%d:%d] %s\n"
    b.pos_fname
    b.pos_lnum b.pos_bol
    e.pos_lnum e.pos_bol
    tok


let token lex lexbuf =
  let tok = lex lexbuf in
  let l_begin = lexeme_start_p lexbuf in
  let l_end = lexeme_end_p lexbuf in
  debug_pos l_begin l_end (Token.name_of_token tok);
  tok
