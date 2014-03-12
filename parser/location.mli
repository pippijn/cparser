type t = Lexing.position * Lexing.position

module Show_t : Deriving_Show.Show
  with type a = t

val dummy : t
