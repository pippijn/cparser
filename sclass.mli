type storage_class =
  | SC_Extern
  | SC_Static
  | SC_Inline
  | SC_Thread
  | SC_Typedef
  | SC_Auto
  | SC_Register

type storage_classes


val storage_classes_of_sexp : Sexplib.Sexp.t -> storage_classes
val sexp_of_storage_classes : storage_classes -> Sexplib.Sexp.t

val empty : storage_classes
val is_empty : storage_classes -> bool

val list_of : storage_classes -> storage_class list

val add : storage_classes -> storage_class -> storage_classes

val is_extern   : storage_classes -> bool
val is_static   : storage_classes -> bool
val is_inline   : storage_classes -> bool
val is_thread   : storage_classes -> bool
val is_typedef  : storage_classes -> bool
val is_auto     : storage_classes -> bool
val is_register : storage_classes -> bool
