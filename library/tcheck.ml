open Ast


let int_type = function
  | None -> BasicType SInt
  | Some suffix ->
      let rec kinds bts = function
        | 0 -> bts
        | pos ->
            let pos = pos - 1 in
            match suffix.[pos] with
            | 'u' | 'U' -> kinds (BT_Unsigned :: bts) pos
            | 'l' | 'L' -> kinds (BT_Long :: bts) pos
            | 'i' | 'I' -> die (Unimplemented "imaginary integer constants")
            | c ->  die (Unimplemented ("integer suffix '" ^ Char.escaped c ^ "'"))
      in
      BasicType (Basic_type.of_list (kinds [BT_Int] (String.length suffix)))


let float_type = function
  | None -> BasicType Double
  | Some suffix ->
      let rec kinds bts = function
        | 0 -> bts
        | pos ->
            let pos = pos - 1 in
            match suffix.[pos] with
            | 'f' | 'F' -> kinds (BT_Float :: bts) pos
            | 'l' | 'L' -> kinds (BT_Long :: bts) pos
            | 'i' | 'I' -> die (Unimplemented "imaginary floating point constants")
            | 'q' | 'Q' -> die (Unimplemented "'q' suffix on floating point literals")
            | 'd' | 'D' -> die (Unimplemented "'d' suffix on floating point literals")
            | c ->  die (Unimplemented ("floating point suffix '" ^ Char.escaped c ^ "'"))
      in

      let bts =
        match kinds [] (String.length suffix) with
        (* If there is a "float" in the type, that's it. *)
        | bts when List.mem BT_Float bts -> bts
        (* Otherwise, add a "double". *)
        | bts -> BT_Double :: bts
      in

      BasicType (Basic_type.of_list bts)


let string_type length kind chars =
  let bt =
    match kind with
    | LIT_String -> Char
    | LIT_WString -> WCharT
  in

  (* '\0'-terminator *)
  let length = length + 1 in

  ArrayType (
    Some (
      TypedExpression (
        Platform.size_t,
        Constant.IntValue (Big_int.big_int_of_int length),
        IntegerLiteral (Traits.empty_position, LIT_Dec, string_of_int length, None))),
    BasicType bt)
