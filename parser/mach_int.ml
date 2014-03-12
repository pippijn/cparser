open Sexplib.Conv

module type IntType = sig
  type mach_int

  val is_big : bool

  val unit_mach_int : mach_int
  val zero_mach_int : mach_int

  val mach_int_of_string : string -> mach_int
  val string_of_mach_int : mach_int -> string

  val mach_int_of_int : int -> mach_int
  val int_of_mach_int : mach_int -> int

  val float_of_mach_int : mach_int -> float

  val add_mach_int : mach_int -> mach_int -> mach_int
  val and_mach_int : mach_int -> mach_int -> mach_int
  val div_mach_int : mach_int -> mach_int -> mach_int
  val eq_mach_int : mach_int -> mach_int -> bool
  val ge_mach_int : mach_int -> mach_int -> bool
  val gt_mach_int : mach_int -> mach_int -> bool
  val le_mach_int : mach_int -> mach_int -> bool
  val lt_mach_int : mach_int -> mach_int -> bool
  val mod_mach_int : mach_int -> mach_int -> mach_int
  val mult_mach_int : mach_int -> mach_int -> mach_int
  val or_mach_int : mach_int -> mach_int -> mach_int
  val shift_left_mach_int : mach_int -> int -> mach_int
  val shift_right_mach_int : mach_int -> int -> mach_int
  val sub_mach_int : mach_int -> mach_int -> mach_int
  val succ_mach_int : mach_int -> mach_int
  val xor_mach_int : mach_int -> mach_int -> mach_int

  module Show_mach_int : Deriving_Show.Show
    with type a = mach_int

end


module MachInt64 : IntType = struct
  type mach_int = int64
    deriving (Show)

  let is_big = false

  let unit_mach_int = 1L
  let zero_mach_int = 0L

  let mach_int_of_string = Int64.of_string
  let string_of_mach_int = Int64.to_string

  let mach_int_of_int = Int64.of_int
  let int_of_mach_int = Int64.to_int

  let float_of_mach_int = Int64.to_float

  let add_mach_int = Int64.add
  let and_mach_int = Int64.logand
  let div_mach_int = Int64.div
  let eq_mach_int : mach_int -> mach_int -> bool = (=)
  let ge_mach_int : mach_int -> mach_int -> bool = (>=)
  let gt_mach_int : mach_int -> mach_int -> bool = (>)
  let le_mach_int : mach_int -> mach_int -> bool = (<=)
  let lt_mach_int : mach_int -> mach_int -> bool = (<)
  let mod_mach_int = Int64.rem
  let mult_mach_int = Int64.mul
  let or_mach_int = Int64.logor
  let shift_left_mach_int = Int64.shift_left
  let shift_right_mach_int = Int64.shift_right
  let sub_mach_int = Int64.sub
  let succ_mach_int = Int64.succ
  let xor_mach_int = Int64.logxor

end


module MachBig_int : IntType = struct
  open Big_int

  type mach_int = big_int

  let is_big = true

  let unit_mach_int = unit_big_int
  let zero_mach_int = zero_big_int

  let mach_int_of_string = big_int_of_string
  let string_of_mach_int = string_of_big_int

  let mach_int_of_int = big_int_of_int
  let int_of_mach_int = int_of_big_int

  let float_of_mach_int = float_of_big_int

  let add_mach_int = add_big_int
  let and_mach_int = and_big_int
  let div_mach_int = div_big_int
  let eq_mach_int = eq_big_int
  let ge_mach_int = ge_big_int
  let gt_mach_int = gt_big_int
  let le_mach_int = le_big_int
  let lt_mach_int = lt_big_int
  let mod_mach_int = mod_big_int
  let mult_mach_int = mult_big_int
  let or_mach_int = or_big_int
  let shift_left_mach_int = shift_left_big_int
  let shift_right_mach_int = shift_right_big_int
  let sub_mach_int = sub_big_int
  let succ_mach_int = succ_big_int
  let xor_mach_int = xor_big_int


  module Show_mach_int = Deriving_Show.Defaults(
    struct

      type a = mach_int

      let format fmt i =
        Deriving_Show.Show_string.format fmt (string_of_mach_int i)

    end)

end


(*include MachBig_int*)
include MachInt64
