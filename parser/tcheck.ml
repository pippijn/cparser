open Ast


let int_type = function
  | None ->
      { t = BasicType SInt;
        t_sloc = Location.dummy;
      }
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
      { t = BasicType (Basic_type.of_list (kinds [BT_Int] (String.length suffix)));
        t_sloc = Location.dummy;
      }


let float_type = function
  | None ->
      { t = BasicType Double;
        t_sloc = Location.dummy;
      }
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

      { t = BasicType (Basic_type.of_list bts);
        t_sloc = Location.dummy;
      }


let string_type length kind chars =
  let bt =
    match kind with
    | LIT_String -> Char
    | LIT_WString -> WCharT
  in

  (* '\0'-terminator *)
  let length = length + 1 in

  { t = ArrayType (
       Some {
         e = IntegerLiteral (LIT_Dec, string_of_int length, None);
         e_sloc = Location.dummy;
         e_type = Platform.size_t;
         e_cval = Constant.IntValue (Mach_int.mach_int_of_int length);
       },
       { t = BasicType bt;
         t_sloc = Location.dummy;
       }
     );
    t_sloc = Location.dummy;
  }
