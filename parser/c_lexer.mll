{
  open C_tokens
  open Lexing

  exception Lexing_error of position * string
  exception Eof

  type scan_state =
    | LexNormal
    | LexAttribute
    | LexEOF

  type state = {
    mutable state : scan_state;
    mutable nesting : int;
  }


  let keyword_table = function
    | LexNormal -> Token.token_table
    | LexAttribute -> Token.attribute_table
    | LexEOF -> raise Eof

  let normalise id = function
    | LexNormal -> id
    | LexAttribute ->
        let rec rstrip id =
          if id.[String.length id - 1] = '_' then
            rstrip (String.sub id 0 (String.length id - 1))
          else
            id
        in
        let rec lstrip id =
          if id.[0] = '_' then
            lstrip (String.sub id 1 (String.length id - 1))
          else
            rstrip id
        in
        lstrip id
    | LexEOF -> raise Eof

  let find_id state id =
    let id = normalise id state in
    try
      match state with
      | LexNormal ->
          Hashtbl.find Token.token_table id
      | LexAttribute ->
          begin try
            Hashtbl.find Token.attribute_table id
          with Not_found ->
            Hashtbl.find Token.token_table id
          end
      | LexEOF -> raise Eof
    with Not_found ->
      if Lexer_hack.is_typedef id then
        TK_TYPEDEF_NAME id
      else
        TK_IDENTIFIER id

  let nest state = function
    | '(' ->
        if state.state = LexAttribute then begin
          state.nesting <- state.nesting + 1
        end
    | ')' ->
        if state.state = LexAttribute then begin
          state.nesting <- state.nesting - 1;
          if state.nesting = 0 then
            state.state <- LexNormal
        end
    | _ -> ()


  let update_pos lexbuf line file =
    let file =
      match file with
      | None -> lexbuf.lex_curr_p.pos_fname
      | Some file -> snd (Cstring.parse [file])
    in
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with
      pos_fname = file;
      pos_lnum = (int_of_string line) - 1;
    }
}

let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper)
let alnum = (alpha | digit)

let identifier = (alpha | '_')(alnum | '_')*

let bstring = '`'  ('\\' _ | [^ '\\' '`' ])* '`'
let dstring = '"'  ('\\' _ | [^ '\\' '"' ])* '"'
let sstring = '\'' ('\\' _ | [^ '\\' '\''])* '\''


let d = digit
let o = ['0'-'7']
let h = ['a'-'f' 'A'-'F' '0'-'9']
let xh = ('0'['x''X'])
let b = ['0' '1']
let xb = ('0'['b''B'])
let e = (['E''e']['+''-']?d+)
let p = (['P''p']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws = [' ' '\t' '\r']

let u = ['\x80'-'\xbf']


rule lex state = parse
  | "__attribute__"
  | "__attribute"			{ state.state <- LexAttribute; KW_ATTRIBUTE }
  | "__declspec"			{ state.state <- LexAttribute; KW_DECLSPEC }

  | "__int"	(d+ as size)		{ KW_INTN     (int_of_string size) }
  | "__float"	(d+ as size)		{ KW_FLOATN   (int_of_string size) }
  | "_Decimal"	(d+ as size)		{ KW_DECIMALN (int_of_string size) }

  | identifier			as id	{ find_id state.state id }

  | (xh h+ as i) (is as is)?		{ TK_HEX_CONSTANT (i, is) }
  | (xb b+ as i) (is as is)?		{ TK_BIN_CONSTANT (i, is) }
  | ('0'o+ as i) (is as is)?		{ TK_OCTAL_CONSTANT (i, is) }
  | (d+ as i) (is as is)?		{ TK_INTEGER_CONSTANT (i, is) }

  | (d+ as f) (fs as fs)?
  | (d+e as f) (fs as fs)?
  | (d*'.'d+e? as f) (fs as fs)?
  | (d+'.'d*e? as f) (fs as fs)?	{ TK_FLOATING_CONSTANT (f, fs) }
  | (xh h*p h* as f) (fs as fs)?
  | (xh h*'.'h*p h* as f) (fs as fs)?	{ TK_HEX_FLOATING_CONSTANT (f, fs) }

  | sstring			as s	{ TK_CHAR_CONSTANT s }
  | dstring			as s	{ TK_STRING_LITERAL s }
  | 'L'sstring			as s	{ TK_WCHAR_CONSTANT s }
  | 'L'dstring			as s	{ TK_WSTRING_LITERAL s }

  | ","					{ TK_COMMA }
  | "..."				{ TK_ELLIPSIS }
  | ">>"				{ TK_GTGT }
  | ">>="				{ TK_GTGT_EQ }
  | "<<"				{ TK_LTLT }
  | "<<="				{ TK_LTLT_EQ }
  | "+"					{ TK_PLUS }
  | "+="				{ TK_PLUS_EQ }
  | "-"					{ TK_MINUS }
  | "-="				{ TK_MINUS_EQ }
  | "*"					{ TK_STAR }
  | "*="				{ TK_STAR_EQ }
  | "/"					{ TK_SLASH }
  | "/="				{ TK_SLASH_EQ }
  | "%"					{ TK_PERCENT }
  | "%="				{ TK_PERCENT_EQ }
  | "&"					{ TK_AND }
  | "&="				{ TK_AND_EQ }
  | "^"					{ TK_CARET }
  | "^="				{ TK_CARET_EQ }
  | "|"					{ TK_PIPE }
  | "|="				{ TK_PIPE_EQ }
  | "<"					{ TK_LESS }
  | "<="				{ TK_LESS_EQ }
  | ">"					{ TK_GREATER }
  | ">="				{ TK_GREATER_EQ }
  | "="					{ TK_EQUALS }
  | "=="				{ TK_EQEQ }
  | "!="				{ TK_NE }

  | "&&"				{ TK_ANDAND }
  | "||"				{ TK_PIPEPIPE }
  | "++"				{ TK_INC }
  | "--"				{ TK_DEC }

  | "->"				{ TK_ARROW }
  | "."					{ TK_PERIOD }

  | "!"					{ TK_EXMARK }
  | "~"					{ TK_TILDE }
  | ";"					{ TK_SEMICOLON }
  | ":"					{ TK_COLON }
  | "?"					{ TK_QMARK }

  | "("					{ nest state '('; TK_LBRACK }
  | ")"					{ nest state ')'; TK_RBRACK }
  | "{"					{ nest state '{'; TK_LBRACE }
  | "}"					{ nest state '}'; TK_RBRACE }
  | "["					{ nest state '['; TK_LSQBRACK }
  | "]"					{ nest state ']'; TK_RSQBRACK }

  | "%include"([^'\n']+ as inc)		{ TK_INCLUDE inc }
  | '#' ws*				{ pp_directive state lexbuf }

  | "%d" as wc				{ WC_DECL wc }
  | "%e" as wc				{ WC_EXPR wc }
  | "%t" as wc				{ WC_TYPE wc }

  | '\n'				{ new_line lexbuf; lex state lexbuf }
  | ws+					{ lex state lexbuf }

  | eof					{ state.state <- LexEOF; EOF }

  | ['\xc0'-'\xdf'] u
  | ['\xe0'-'\xef'] u u
  | ['\xf0'-'\xf7'] u u u
  | ['\xf8'-'\xfb'] u u u u
  | ['\xfc'-'\xfd'] u u u u u as utf8	{ raise (Lexing_error (lexeme_start_p lexbuf, utf8)) }

  | _ as c				{ raise (Lexing_error (lexeme_start_p lexbuf, Char.escaped c)) }


and pp_directive state = parse
  | "ident" ws+ dstring			{ lex state lexbuf }
  | "sccs" ws+ dstring			{ lex state lexbuf }
  | "pragma" ws+ [^ '\n']*		{ lex state lexbuf }

  | (d+ as line)
    (ws+ (dstring as file) (ws+ d+)*)?	{ update_pos lexbuf line file; lex state lexbuf }

  | _ as c				{ raise (Lexing_error (lexeme_start_p lexbuf, Char.escaped c)) }


{
  let state () = {
    state = LexNormal;
    nesting = 0;
  }

  let token state =
    match state.state with
    | LexNormal
    | LexAttribute -> lex state
    | LexEOF -> raise Eof
}
