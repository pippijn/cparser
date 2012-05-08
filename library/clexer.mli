exception Lexing_error of Lexing.position * string
exception Eof

type state

val state : unit -> state
val token : state -> Lexing.lexbuf -> Ctokens.token
