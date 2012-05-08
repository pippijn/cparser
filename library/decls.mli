val add_tqual : Ast.declaration -> Tqual.type_qualifier list -> Ast.declaration

val add_basic_type : Ast.declaration -> Ast.partial_basic_type -> Ast.declaration
val add_dqual : Ast.declaration -> Sclass.storage_class option * Tqual.type_qualifier option -> Ast.declaration
val add_pointer_type : Ast.declaration -> Ast.declaration
val add_parameter_types : Ast.declaration -> Ast.declaration list -> Ast.declaration
val set_tspec : Ast.declaration -> Ast.ctype -> Ast.declaration

val set_base_type : Ast.declaration -> Ast.ctype -> Ast.declaration

val merge_decls : Ast.declaration -> Ast.declaration -> Ast.declaration
val decl_name : Ast.declaration -> string
val decl_type : Ast.declaration -> Ast.ctype
val decl_decl : Ast.declaration -> Ast.declaration
val decl_base_type : Ast.declaration -> Ast.declaration
val finish_decl : Ast.declaration -> Ast.declaration -> Ast.expression option -> Ast.declaration

val return_type : Ast.declaration -> Ast.ctype
