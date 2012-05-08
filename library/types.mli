val add_tqual : Ast.ctype -> Tqual.type_qualifier list -> Ast.ctype
val add_basic_type : Ast.ctype -> Ast.partial_basic_type -> Ast.ctype

val pointer_to : Ast.ctype -> Ast.ctype

val set_base_type : Ast.ctype -> Ast.ctype -> Ast.ctype

val make_array_type : Ast.ctype list -> Ast.ctype

val base_type : Ast.ctype -> Ast.ctype
val unbase_type : Ast.ctype -> Ast.ctype
