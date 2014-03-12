type t = private string

module Show_t : Deriving_Show.Show
  with type a = t

val uchar_of_int : int -> string
val adopt : string -> t
val length : t -> int
