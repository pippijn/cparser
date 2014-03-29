open Attributes


let print_decl decl = print_endline (Ast.Show_decl.show decl)
let print_expr expr = print_endline (Ast.Show_expr.show expr)
let print_stmt stmt = print_endline (Ast.Show_stmt.show stmt)
let print_type ctyp = print_endline (Ast.Show_ctyp.show ctyp)


let string_of_storage_class  sc = Token.string_of_token (Token.token_of_storage_class  sc)
let string_of_type_qualifier tq = Token.string_of_token (Token.token_of_type_qualifier tq)
let string_of_sue_kind       sk = Token.string_of_token (Token.token_of_sue_kind       sk)
let string_of_basic_type     bt = Token.string_of_token (Token.token_of_basic_type     bt)


let string_of_ternop op =
  let left, right = Token.token_of_ternop op in
  Token.string_of_token left,
  Token.string_of_token right


let string_of_binop op = Token.string_of_token (Token.token_of_binop op)
let string_of_unop  op = Token.string_of_token (Token.token_of_unop  op)
