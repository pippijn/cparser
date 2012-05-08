type iter_struct = {
  iter_type : Ast.ctype -> unit;
  iter_expr : Ast.expression -> unit;
  iter_stmt : Ast.statement -> unit;
  iter_decl : Ast.declaration -> unit;
}

val iter_decl : iter_struct -> Ast.declaration -> unit
val iter_expr : iter_struct -> Ast.expression -> unit
val iter_stmt : iter_struct -> Ast.statement -> unit
val iter_type : iter_struct -> Ast.ctype -> unit


type map_struct = {
  map_type : Ast.ctype -> Ast.ctype;
  map_expr : Ast.expression -> Ast.expression;
  map_stmt : Ast.statement -> Ast.statement;
  map_decl : Ast.declaration -> Ast.declaration;
}

val map_decl : map_struct -> Ast.declaration -> Ast.declaration
val map_expr : map_struct -> Ast.expression -> Ast.expression
val map_stmt : map_struct -> Ast.statement -> Ast.statement
val map_type : map_struct -> Ast.ctype -> Ast.ctype


type 'a fold_struct = {
  fold_type : 'a -> Ast.ctype -> 'a;
  fold_expr : 'a -> Ast.expression -> 'a;
  fold_stmt : 'a -> Ast.statement -> 'a;
  fold_decl : 'a -> Ast.declaration -> 'a;
}

val fold_decl : 'a fold_struct -> 'a -> Ast.declaration -> 'a
val fold_expr : 'a fold_struct -> 'a -> Ast.expression -> 'a
val fold_stmt : 'a fold_struct -> 'a -> Ast.statement -> 'a
val fold_type : 'a fold_struct -> 'a -> Ast.ctype -> 'a
