open Ast
open Big_int
open Constant


let parse_int kind str =
  IntValue (big_int_of_string str)


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
      let bool_of_big_int a = not (eq_big_int a zero_big_int) in
      let big_int_of_bool a = if a then unit_big_int else zero_big_int in

      let int_fun =
        match op with
        | OP_Add -> add_big_int
        | OP_Subtract -> sub_big_int
        | OP_Multiply -> mult_big_int
        | OP_Divide -> div_big_int
        | OP_Modulo -> mod_big_int
        | OP_BitwiseAnd -> and_big_int
        | OP_BitwiseOr -> or_big_int
        | OP_BitwiseXor -> xor_big_int
        | OP_ShiftLeft -> fun a b -> shift_left_big_int a (int_of_big_int b)
        | OP_ShiftRight -> fun a b -> shift_right_big_int a (int_of_big_int b)
        | OP_LogicalAnd -> fun a b -> big_int_of_bool (bool_of_big_int a && bool_of_big_int b)
        | OP_LogicalOr -> fun a b -> big_int_of_bool (bool_of_big_int a || bool_of_big_int b)
        | OP_Equal -> fun a b -> big_int_of_bool (eq_big_int a b)
        | OP_NotEqual -> fun a b -> big_int_of_bool (not (eq_big_int a b))
        | OP_Less -> fun a b -> big_int_of_bool (lt_big_int a b)
        | OP_LessEqual -> fun a b -> big_int_of_bool (le_big_int a b)
        | OP_Greater -> fun a b -> big_int_of_bool (gt_big_int a b)
        | OP_GreaterEqual -> fun a b -> big_int_of_bool (ge_big_int a b)

        | _ -> die (Unimplemented ("binary operator `" ^ (Token.string_of_binop op) ^ "' for integer constants"))
      in

      let float_fun a b =
        match op with
        | OP_Add -> FloatValue (a +. b)
        | OP_Subtract -> FloatValue (a -. b)
        | OP_Multiply -> FloatValue (a *. b)
        | OP_Divide -> FloatValue (a /. b)
        | OP_Modulo -> FloatValue (mod_float a b)
        | OP_LogicalAnd -> IntValue (big_int_of_bool (a <> 0.0 && b <> 0.0))
        | OP_LogicalOr -> IntValue (big_int_of_bool (a <> 0.0 || b <> 0.0))
        | OP_Equal -> IntValue (big_int_of_bool (a = b))
        | OP_NotEqual -> IntValue (big_int_of_bool (a <> b))
        | OP_Less -> IntValue (big_int_of_bool (a < b))
        | OP_LessEqual -> IntValue (big_int_of_bool (a <= b))
        | OP_Greater -> IntValue (big_int_of_bool (a > b))
        | OP_GreaterEqual -> IntValue (big_int_of_bool (a >= b))

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
  TypedExpression (
    BasicType SInt,
    IntValue value,
    IntegerLiteral (Traits.empty_position, LIT_Dec, string_of_big_int value, None)
  )


let is_zero = function
  | TypedExpression (_, IntValue value, _) -> value = zero_big_int
  | _ -> false
