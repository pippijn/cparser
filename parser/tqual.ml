open Sexplib.Conv

(** {6 Type qualifiers} *)

type type_qualifier =
  | TQ_Const		(** [const] *)
  | TQ_Volatile		(** [volatile] *)
  | TQ_Complex		(** [_Complex] *)
  | TQ_Restrict		(** [restrict] *)
  deriving (Show)

type type_qualifiers = type_qualifier list
  deriving (Show)


let empty = []
let is_empty tq = tq = []

let list_of tqs = tqs

let remove_compatible tq = tq
let add tq tqs = tqs @ tq

let is_const    = List.mem TQ_Const
let is_volatile = List.mem TQ_Volatile
let is_complex  = List.mem TQ_Complex
let is_restrict = List.mem TQ_Restrict
