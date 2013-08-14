type entry_point =
  | EP_Decl
  | EP_Expr
  | EP_Stmt
  | EP_Type
  | EP_Unit

val ast : bool
val ast_pp : bool
val cflags : string
val codegen : bool
val compile : bool
val entry : entry_point
val files : string list
val globals : bool
val merr : bool
val no_pp : bool
val output : string
val stacktrace : bool
val string : bool
val syntax_only : bool
val tokenise : bool
val tokens : bool
val validate : bool
val verbose : bool
val w : bool
