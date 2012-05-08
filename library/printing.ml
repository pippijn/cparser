open Attributes
open Sexplib.Sexp


let print_decl tu =
  output_hum stdout (Ast.sexp_of_declaration tu);
  print_newline ()

let print_expr expr =
  output_hum stdout (Ast.sexp_of_expression expr);
  print_newline ()

let print_stmt stmt =
  output_hum stdout (Ast.sexp_of_statement stmt);
  print_newline ()

let print_type stmt =
  output_hum stdout (Ast.sexp_of_ctype stmt);
  print_newline ()


let string_of_storage_class sc = Token.string_of_token (Token.token_of_storage_class sc)
let string_of_type_qualifier tq = Token.string_of_token (Token.token_of_type_qualifier tq)
let string_of_sue_kind kind = Token.string_of_token (Token.token_of_sue_kind kind)
let string_of_basic_type bt = Token.string_of_token (Token.token_of_basic_type bt)


let string_of_ternop op =
  let left, right = Token.token_of_ternop op in
  Token.string_of_token left,
  Token.string_of_token right


let string_of_binop op = Token.string_of_token (Token.token_of_binop op)
let string_of_unop op = Token.string_of_token (Token.token_of_unop op)
