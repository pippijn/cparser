(** The [Platform] module contains all properties required to accurately
    calculate size and alignment for all types, including compound types
    such as [struct] and [union], and to correctly type a program. On
    initialisation, the platform description is read from an S-Expression
    file {i platform/$MACHTYPE/$OSTYPE.lsp}. The machine and operating
    system types are read from the environment. It defaults to a platform
    specified at installation time {i platform/default/default.lsp}. *)

(** Byte order in which the CPU handles its data.
    An example is given for the representation of the
    16-bit hexadecimal integer 0x1234. *)
type endianness =
  | LittleEndian
    (** Representation: 0x4321. Used on most desktop CPUs such as x86. *)
  | MiddleEndian
    (** Representation: 0x3412. Used on PDP computers. *)
  | BigEndian
    (** Representation: 0x1234. Used on embedded CPUs such as AVR, PowerPC
        and ARM in big endian mode. *)

(** {6 Platform information} *)


val name : string
  (** Platform name of the form {i MACHTYPE}-{i OSTYPE}. For example on GNU/Linux
      on a 32 bit Intel machine, the name should be {i i386-linux-gnu}. The
      platform file will be stored as {i i386/linux-gnu.lsp}. *)

val underscore_names : bool
  (** Whether the platform C compiler prefixes external symbols with underscores. *)

val endian : endianness
  (** Byte order on the target platform. *)


(** {6 Type size and alignment} *)


val max_alignment : int
  (** The maximum type alignment requirement. *)

val char_bit : int
  (** How many bits the C [char] type has. *)
val char_is_signed : bool
  (** Whether the C [char] type is signed or unsigned. [true] means signed. *)

val sizeof_short : int
  (** The result of [sizeof(short)]. The number of [char]s that fit into a [short]. *)
val alignof_short : int
  (** Alignment requirement for [short] in [char] sizes. *)

val sizeof_int : int
  (** The result of [sizeof(int)]. The number of [char]s that fit into an [int]. *)
val alignof_int : int
  (** Alignment requirement for [int] in [char] sizes. *)

val sizeof_long : int
  (** The result of [sizeof(long)]. The number of [char]s that fit into a [long]. *)
val alignof_long : int
  (** Alignment requirement for [long] in [char] sizes. *)

val sizeof_long_long : int
  (** The result of [sizeof(long long)]. The number of [char]s that fit into a [long long]. *)
val alignof_long_long : int
  (** Alignment requirement for [long long] in [char] sizes. *)

val sizeof_float : int
  (** The result of [sizeof(float)]. The number of [char]s that fit into a [float]. *)
val alignof_float : int
  (** Alignment requirement for [float] in [char] sizes. *)

val sizeof_double : int
  (** The result of [sizeof(double)]. The number of [char]s that fit into a [double]. *)
val alignof_double : int
  (** Alignment requirement for [double] in [char] sizes. *)

val sizeof_long_double : int
  (** The result of [sizeof(long double)]. The number of [char]s that fit into a [long double]. *)
val alignof_long_double : int
  (** Alignment requirement for [long double] in [char] sizes. *)

val sizeof_object_pointer : int
  (** The result of [sizeof( void* )]. The number of [char]s that fit into an object pointer. *)
val alignof_object_pointer : int
  (** Alignment requirement for object pointers in [char] sizes. *)

val sizeof_function_pointer : int
  (** The result of [sizeof(void( * )())]. The number of [char]s that fit into a function pointer. *)
val alignof_function_pointer : int
  (** Alignment requirement for function pointers in [char] sizes. *)


(** {6 Compiler types} *)


val size_t : Ast.ctype
  (** AST representation of C typedef [size_t] as defined in [<stddef.h>].
      This is the result type of the [sizeof] operator. *)

val ptrdiff_t : Ast.ctype
  (** AST representation of C typedef [ptrdiff_t] as defined in [<stddef.h>].
      This is the result type of the [-] operator on two pointers. *)

val wchar_t : Ast.ctype
  (** AST representation of C typedef [wchar_t] as defined in [<wchar.h>].
      This is the type of [L'a']. *)


(** {6 External commands} *)


val preprocessor : string
  (** Command to run as preprocessor for input code.
      It accepts a C source file as input and writes the preprocessed code to
      its standard output. *)

val compiler : string
  (** Command to run as compiler for generated C code. It reads the input from
      its standard input. Standard output is not redirected. *)


(** {6 Utilities} *)


val print_version : unit -> unit
  (** Print a summary of all the values given above to stdout. *)
