open Ucslib

type state =
  | Initial
  | EscapeStart
  | OctEscape of int list
  | HexEscape of int list
  | UcsEscape of int * int list
  | Utf8Char of int * char list


let int_of_digit = function
  | '0' -> 0
  | '1' -> 1
  | '2' -> 2
  | '3' -> 3
  | '4' -> 4
  | '5' -> 5
  | '6' -> 6
  | '7' -> 7
  | '8' -> 8
  | '9' -> 9
  | 'a' -> 10
  | 'b' -> 11
  | 'c' -> 12
  | 'd' -> 13
  | 'e' -> 14
  | 'f' -> 15
  | _ -> raise ExPervasives.Impossible


let parse strlist =
  let length =
    List.fold_left (fun len str ->
      len + (String.length str)
    ) 0 strlist
  in
  let buf = Buffer.create length in

  let add_char =
    Buffer.add_char buf
  in

  let add_string =
    Buffer.add_string buf
  in

  let add_chr num =
    try
      add_char (char_of_int num)
    with Invalid_argument "char_of_int" ->
      raise (Invalid_argument (Printf.sprintf "character code out of range; value = %d" num))
  in

  let add_uni num =
    try
      add_string (Unicode.utf8s_of_utf32 (Unicode.adopt_utf32 num) :> string)
    with Invalid_argument "uchar_of_int" ->
      raise (Invalid_argument (Printf.sprintf "universal character code out of range; value = %d" num))
  in

  let combine_digits radix num = 
    snd (List.fold_left (fun (multiplier, num) digit ->
      let newnum = num + digit * multiplier in
      let newmultiplier = multiplier * radix in

      if newnum < num || newmultiplier < multiplier then
        raise (Invalid_argument "character code out of range");

      newmultiplier, newnum
    ) (1, 0) num)
  in

  let len =
    List.fold_left (fun len str ->
      let start = String.index str '"' + 1 in
      let str = String.sub str start (String.length str - start - 1) in

      let add = function
        | Initial -> ()
        | EscapeStart -> failwith "unterminated string escape"
        | OctEscape num -> add_chr (combine_digits 8 num)
        | HexEscape num -> add_chr (combine_digits 16 num)
        | UcsEscape (_, num) -> add_uni (combine_digits 16 num)
        | Utf8Char (_, chr) -> add_string (ExString.of_list (List.rev chr))
      in

      let len, state =
        ExString.fold_left (fun (len, state) c ->
          let rec handle (len, state) c =
            match state with
            | Initial ->
                begin match c with
                | '\\' -> len, EscapeStart
                | c ->
                    match Unicode.utf8_length c with
                    | 0 -> 
                        add_char c; len + 1, Initial
                    | cont ->
                        len, Utf8Char (cont, [c])
                end

            | Utf8Char (0, chars) as state ->
                add state;
                handle (len + 1, Initial) c

            | Utf8Char (bytec, chars) ->
                let d = int_of_char c in
                if d >= 0x80 && d < 0xc0 then
                  len, Utf8Char (bytec - 1, c :: chars)
                else
                  raise (Invalid_argument ("invalid UTF-8 sequence: " ^ (Char.escaped c)))

            | EscapeStart ->
                begin match c with
                | '\\' | '"' as c -> add_char c; len + 1, Initial

                | 'a' -> add_chr 7; len + 1, Initial
                | 'b' -> add_chr 8; len + 1, Initial
                | 'e' -> add_chr 27; len + 1, Initial
                | 'f' -> add_chr 12; len + 1, Initial
                | 'n' -> add_chr 10; len + 1, Initial
                | 'r' -> add_chr 13; len + 1, Initial
                | 't' -> add_chr 9; len + 1, Initial
                | 'v' -> add_chr 11; len + 1, Initial

                | 'u' -> len, UcsEscape (4, [])
                | 'U' -> len, UcsEscape (8, [])
                | 'x' -> len, HexEscape []

                | '0' .. '7' as c ->
                    len, OctEscape [int_of_digit c]

                | c -> failwith ("invalid escape sequence: " ^ (Char.escaped c))
                end

            | OctEscape num ->
                begin match c with
                | '0' .. '7' as c ->
                    len, OctEscape (int_of_digit c :: num)
                | c ->
                    add state;
                    handle (len + 1, Initial) c
                end

            | HexEscape num ->
                begin match Char.lowercase c with
                | '0' .. '9' | 'a' .. 'f' as c ->
                    len, HexEscape (int_of_digit c :: num)
                | c ->
                    add state;
                    handle (len + 1, Initial) c
                end

            | UcsEscape (0, num) as state ->
                add state;
                handle (len + 1, Initial) c

            | UcsEscape (digitc, num) ->
                  begin match Char.lowercase c with
                  | '0' .. '9' | 'a' .. 'f' as lc ->
                      len, UcsEscape (digitc - 1, int_of_digit lc :: num)
                  | c -> failwith ("invalid universal character code: " ^ (ExString.of_char c))
                  end
          in
          handle (len, state) c

        ) (len, Initial) str
      in

      add state;

      match state with
      | Initial
      | EscapeStart ->
          len
      | OctEscape _
      | HexEscape _
      | UcsEscape _
      | Utf8Char _ ->
          len + 1

    ) 0 strlist
  in

  len, Buffer.contents buf
