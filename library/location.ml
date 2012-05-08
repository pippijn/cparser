open Lexing
open Sexplib.Sexp

let sexp_of_position pos =
  let {
    pos_fname = fname;
    pos_lnum = lnum;
    pos_bol = bol;
    pos_cnum = cnum;
  } = pos in
  List [
    Atom "position";
    Atom fname;
    Atom (string_of_int lnum);
    Atom (string_of_int bol);
    Atom (string_of_int cnum);
  ]

let position_of_sexp sx =
  match sx with
  | List [
      Atom "position";
      Atom fname;
      Atom lnum;
      Atom bol;
      Atom cnum;
    ] ->
      {
        pos_fname = fname;
        pos_lnum = int_of_string lnum;
        pos_bol = int_of_string bol;
        pos_cnum = int_of_string cnum;
      }
  | sexp -> Sexplib.Conv_error.unexpected_stag "ast.ml.position" sexp

type t = position * position with sexp

let dummy = dummy_pos, dummy_pos
