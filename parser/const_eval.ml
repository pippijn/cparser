open Ast
open Mach_int
open Constant


let parse_int kind str =
  IntValue (mach_int_of_string str)


let parse_float kind str =
  FloatValue (float_of_string str)


let parse_string_literal strlist =
  Cstring.parse strlist


let make_string kind chars =
  match kind with
  | LIT_String -> StringValue (chars)
  | LIT_WString -> WStringValue (ExUTF8.adopt chars)


let eval_arithmetic op lhs rhs =
  match lhs, rhs with
  (* Arithmetic with a non-constant operand results in a non-constant expression. *)
  | NonConst, _
  | _, NonConst -> NonConst
  | lhs, rhs ->
      let bool_of_mach_int a = not (eq_mach_int a zero_mach_int) in
      let mach_int_of_bool a = if a then unit_mach_int else zero_mach_int in

      let int_fun =
        match op with
        | OP_Add -> add_mach_int
        | OP_Subtract -> sub_mach_int
        | OP_Multiply -> mult_mach_int
        | OP_Divide -> div_mach_int
        | OP_Modulo -> mod_mach_int
        | OP_BitwiseAnd -> and_mach_int
        | OP_BitwiseOr -> or_mach_int
        | OP_BitwiseXor -> xor_mach_int
        | OP_ShiftLeft -> fun a b -> shift_left_mach_int a (int_of_mach_int b)
        | OP_ShiftRight -> fun a b -> shift_right_mach_int a (int_of_mach_int b)
        | OP_LogicalAnd -> fun a b -> mach_int_of_bool (bool_of_mach_int a && bool_of_mach_int b)
        | OP_LogicalOr -> fun a b -> mach_int_of_bool (bool_of_mach_int a || bool_of_mach_int b)
        | OP_Equal -> fun a b -> mach_int_of_bool (eq_mach_int a b)
        | OP_NotEqual -> fun a b -> mach_int_of_bool (not (eq_mach_int a b))
        | OP_Less -> fun a b -> mach_int_of_bool (lt_mach_int a b)
        | OP_LessEqual -> fun a b -> mach_int_of_bool (le_mach_int a b)
        | OP_Greater -> fun a b -> mach_int_of_bool (gt_mach_int a b)
        | OP_GreaterEqual -> fun a b -> mach_int_of_bool (ge_mach_int a b)

        | _ -> die (Unimplemented ("binary operator `" ^ (Token.string_of_binop op) ^ "' for integer constants"))
      in

      let float_fun a b =
        match op with
        | OP_Add -> FloatValue (a +. b)
        | OP_Subtract -> FloatValue (a -. b)
        | OP_Multiply -> FloatValue (a *. b)
        | OP_Divide -> FloatValue (a /. b)
        | OP_Modulo -> FloatValue (mod_float a b)
        | OP_LogicalAnd -> IntValue (mach_int_of_bool (a <> 0.0 && b <> 0.0))
        | OP_LogicalOr -> IntValue (mach_int_of_bool (a <> 0.0 || b <> 0.0))
        | OP_Equal -> IntValue (mach_int_of_bool (a = b))
        | OP_NotEqual -> IntValue (mach_int_of_bool (a <> b))
        | OP_Less -> IntValue (mach_int_of_bool (a < b))
        | OP_LessEqual -> IntValue (mach_int_of_bool (a <= b))
        | OP_Greater -> IntValue (mach_int_of_bool (a > b))
        | OP_GreaterEqual -> IntValue (mach_int_of_bool (a >= b))

        | OP_BitwiseAnd
        | OP_BitwiseOr
        | OP_BitwiseXor
        | OP_ShiftLeft
        | OP_ShiftRight ->
            die (Unimplemented ("bitwise binary operator `" ^ (Token.string_of_binop op) ^ "' for floating point constants"))

        | _ -> die (Unimplemented ("binary operator `" ^ (Token.string_of_binop op) ^ "' for floating point constants"))
      in

      begin match lhs, rhs with
      | IntValue a, IntValue b -> IntValue (int_fun a b)
      | FloatValue a, FloatValue b -> float_fun a b
      | _ -> die (Unimplemented "eval_arithmetic")
      end


let make_int value =
  { e = TypedExpression (
       { t = BasicType SInt;
         t_sloc = Location.dummy;
       },
       IntValue value,
       { e = IntegerLiteral (LIT_Dec, string_of_mach_int value, None);
         e_sloc = Location.dummy;
       }
     );
    e_sloc = Location.dummy;
  }


let is_zero = function
  | { e = TypedExpression (_, IntValue value, _) } -> value = zero_mach_int
  | _ -> false
