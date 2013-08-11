val enter_scope : string -> unit
val leave_scope : unit -> unit

val insert_decl : string -> Symtab.symtype -> Ast.declaration -> unit
val insert_type : string -> Symtab.symtype -> Ast.ctype -> unit

val replace_decl : string -> Symtab.symtype -> Ast.declaration -> unit
val replace_type : string -> Symtab.symtype -> Ast.ctype -> unit
val lookup_decl : string -> Symtab.symtype -> Ast.declaration
val lookup_type : string -> Symtab.symtype -> Ast.ctype

val print : unit -> unit
