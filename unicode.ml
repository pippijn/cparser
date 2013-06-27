let utf8_char chr =
  let continuation n = ((chr lsr (6 * n)) land 0x3f) lor 0x80 in
  let leading n mask =  (chr lsr (6 * n))            lor mask in

  let chars =
    if chr <= 0x007f then (* one octet *)
      [
        char_of_int chr;
      ]
    else if chr <= 0x07ff then (* two octets *)
      [
        char_of_int (leading 1 0xc0);
        char_of_int (continuation 0);
      ]
    else if chr <= 0xffff then (* three octets *)
      [
        char_of_int (leading 2 0xe0);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      ]
    else if chr <= 0x1fffff then (* four octets *)
      [
        char_of_int (leading 3 0xf0);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      ]
    else if chr <= 0x3ffffff then (* five octets *)
      [
        char_of_int (leading 4 0xf8);
        char_of_int (continuation 3);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      ]
    else if chr <= 0x7fffffff then (* six octets *)
      [
        char_of_int (leading 5 0xfc);
        char_of_int (continuation 4);
        char_of_int (continuation 3);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      ]
    else
      raise (Invalid_argument "utf8_char")
  in
  ExString.of_list chars


let utf32_char = function
  | [c1] ->
      (((c1 land 0xff) lsl (6 * 0)) land 0x000000ff)
  | [c1; c2] ->
      (((c2 land 0xff) lsl (6 * 0)) land 0x0000003f) +
      (((c1 land 0xff) lsl (6 * 1)) land 0x000007ff)
  | [c1; c2; c3] ->
      (((c3 land 0xff) lsl (6 * 0)) land 0x0000003f) +
      (((c2 land 0xff) lsl (6 * 1)) land 0x00000fff) +
      (((c1 land 0xff) lsl (6 * 2)) land 0x0000ffff)
  | [c1; c2; c3; c4] ->
      (((c4 land 0xff) lsl (6 * 0)) land 0x0000003f) +
      (((c3 land 0xff) lsl (6 * 1)) land 0x00000fff) +
      (((c2 land 0xff) lsl (6 * 2)) land 0x0003ffff) +
      (((c1 land 0xff) lsl (6 * 3)) land 0x001fffff)
  (* FIXME: I don't know whether these are correct (probably not). *)
  | [c1; c2; c3; c4; c5] ->
      (((c5 land 0xff) lsl (6 * 0)) land 0x0000003f) +
      (((c4 land 0xff) lsl (6 * 1)) land 0x00000fff) +
      (((c3 land 0xff) lsl (6 * 2)) land 0x0000ffff) +
      (((c2 land 0xff) lsl (6 * 3)) land 0x001fffff) +
      (((c1 land 0xff) lsl (6 * 3)) land 0x03ffffff)
  | [c1; c2; c3; c4; c5; c6] ->
      (((c6 land 0xff) lsl (6 * 0)) land 0x0000003f) +
      (((c5 land 0xff) lsl (6 * 1)) land 0x00000fff) +
      (((c4 land 0xff) lsl (6 * 2)) land 0x0000ffff) +
      (((c3 land 0xff) lsl (6 * 3)) land 0x001fffff) +
      (((c2 land 0xff) lsl (6 * 3)) land 0x03ffffff) +
      (((c1 land 0xff) lsl (6 * 3)) land 0x7fffffff)
  | _ -> failwith "utf32_char"


type utf8_state =
  | Initial
  | Continuation of int * int list

let utf8_length d =
  if d >= 0x00 && d < 0x80 then 0 else
  if d >= 0xc0 && d < 0xe0 then 1 else
  if d >= 0xe0 && d < 0xf0 then 2 else
  if d >= 0xf0 && d < 0xf8 then 3 else
  if d >= 0xf8 && d < 0xfc then 4 else
  if d >= 0xfc && d < 0xfe then 5 else
  failwith "utf8_length"

let utf32_of_utf8 s =
  let rec next_utf32 (state, utf32) c =
    let d = int_of_char c in

    match state with
    | Initial when d >= 0x80 -> Continuation (utf8_length d, [d]), utf32

    | Continuation (0, chars) ->
        next_utf32 (Initial, utf32_char (List.rev chars) :: utf32) c
    | Continuation (n, chars) ->
        Continuation (n - 1, d :: chars), utf32

    | Initial ->
        Initial, utf32_char [d] :: utf32
  in

  match ExString.fold_left next_utf32 (Initial, []) s with
  | Initial, utf32 ->
      List.rev utf32
  | Continuation (0, chars), utf32 ->
      List.rev (utf32_char (List.rev chars) :: utf32)
  | _ -> failwith "utf32_of_utf8"


let utf8_of_utf32 s =
  let buf = Buffer.create (List.length s) in
  List.iter (fun c ->
    Buffer.add_string buf (utf8_char c)
  ) s;
  Buffer.contents buf
