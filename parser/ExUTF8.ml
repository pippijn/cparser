type t = string
  deriving (Show)


let uchar_of_int chr =
  let continuation n = ((chr lsr (6 * n)) land 0x3f) lor 0x80 in
  let leading n mask =  (chr lsr (6 * n))            lor mask in

  let chars =
    if chr <= 0x007F then (* one octet *)
      [|
        char_of_int chr;
      |]
    else if chr <= 0x07FF then (* two octets *)
      [|
        char_of_int (leading 1 0xc0);
        char_of_int (continuation 0);
      |]
    else if chr <= 0xFFFF then (* three octets *)
      [|
        char_of_int (leading 2 0xe0);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      |]
    else if chr <= 0x1FFFFF then (* four octets *)
      [|
        char_of_int (leading 3 0xf0);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      |]
    else if chr <= 0x3FFFFFF then (* five octets *)
      [|
        char_of_int (leading 4 0xf8);
        char_of_int (continuation 3);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      |]
    else if chr <= 0x7FFFFFFF then (* six octets *)
      [|
        char_of_int (leading 5 0xfc);
        char_of_int (continuation 4);
        char_of_int (continuation 3);
        char_of_int (continuation 2);
        char_of_int (continuation 1);
        char_of_int (continuation 0);
      |]
    else
      raise (Invalid_argument "uchar_of_int")
  in
  ExString.of_array chars


let adopt s = s
let to_string s = s

let length = String.length
