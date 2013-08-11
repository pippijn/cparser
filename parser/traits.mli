val empty_position : Ast.position

val copy_pos : Ast.declaration -> [> `Position of Location.t] list
val set_pos_decl : Location.t -> Ast.declaration -> Ast.declaration
val add_attrs : Ast.attribute list -> Ast.declaration -> Ast.declaration

val pos_of_expr : Ast.expression -> Location.t
val pos_of_stmt : Ast.statement -> Location.t
val pos_of_type : Ast.ctype -> Location.t
val pos_of_decl : Ast.declaration -> Location.t

val clear_deep_expr : Ast.expression -> Ast.expression
val clear_deep_stmt : Ast.statement -> Ast.statement
val clear_deep_type : Ast.ctype -> Ast.ctype
val clear_deep_decl : Ast.declaration -> Ast.declaration
