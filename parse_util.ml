let parse_string parsefn code =
  let lexbuf = Lexing.from_string code in
  parsefn (C_lexer.token (C_lexer.state ())) lexbuf

let expr_of_string code = parse_string C_parser.parse_expr code
let decl_of_string code = parse_string C_parser.parse_decl code
let stmt_of_string code = parse_string C_parser.parse_stmt code
let type_of_string code = parse_string C_parser.parse_type code
