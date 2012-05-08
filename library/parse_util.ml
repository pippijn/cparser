let parse_string parsefn code =
  let lexbuf = Lexing.from_string code in
  parsefn (Clexer.token (Clexer.state ())) lexbuf

let expr_of_string code = parse_string Cparser.parse_expr code
let decl_of_string code = parse_string Cparser.parse_decl code
let stmt_of_string code = parse_string Cparser.parse_stmt code
let type_of_string code = parse_string Cparser.parse_type code
