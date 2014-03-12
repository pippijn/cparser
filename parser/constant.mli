type t =
  | NonConst
  | IntValue of Mach_int.mach_int
  | FloatValue of float
  | StringValue of string
  | WStringValue of ExUTF8.t

module Show_t : Deriving_Show.Show
with type a = t

val of_int : int -> t
val of_mach_int : Mach_int.mach_int -> t
val of_float : float -> t
val of_string : string -> t
val of_wstring : string -> t

val to_int : t -> int
val to_mach_int : t -> Mach_int.mach_int
val to_float : t -> float
val to_string : t -> string
val to_wstring : t -> string
