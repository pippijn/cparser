type position = Lexing.position = {
  pos_fname : string;
  pos_lnum : int;
  pos_bol : int;
  pos_cnum : int;
} deriving (Show)

type t = position * position
  deriving (Show)

let dummy = Lexing.dummy_pos, Lexing.dummy_pos
