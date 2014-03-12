open Sexplib.Conv

(** {6 Storage classes} *)

type storage_class =
  (* Global storage classes *)
  | SC_Extern		(** [extern] *)
  | SC_Static		(** [static] *)
  | SC_Inline		(** [inline] *)
  | SC_Thread		(** [__thread] *)
  | SC_Typedef		(** [typedef] *)
  (* Local storage classes *)
  | SC_Auto		(** [auto] *)
  | SC_Register		(** [register] *)
  deriving (Show)

type storage_classes = storage_class list
  deriving (Show)


let empty = []
let is_empty sc = sc = []

let list_of scs = scs

let add scs sc = sc :: scs

let is_extern   = List.mem SC_Extern
let is_static   = List.mem SC_Static
let is_inline   = List.mem SC_Inline
let is_thread   = List.mem SC_Thread
let is_typedef  = List.mem SC_Typedef
let is_auto     = List.mem SC_Auto
let is_register = List.mem SC_Register
