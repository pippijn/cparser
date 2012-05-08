open Ast
open Sexplib.Conv
open Sexplib.Sexp


type version = {
  major : int;
  minor : int;
}
let version = {
  major = 0;
  minor = 1;
}


type endianness =
  | LittleEndian
  | MiddleEndian
  | BigEndian
  with sexp


let string_of_endianness = function
  | LittleEndian -> "little"
  | MiddleEndian -> "middle"
  | BigEndian -> "big"


type machine = {
  name				: string;

  underscore_names		: bool;

  endian			: endianness;

  max_alignment			: int;

  char_bit			: int;
  char_is_signed		: bool;

  sizeof_short			: int;
  alignof_short			: int;
  sizeof_int			: int;
  alignof_int			: int;
  sizeof_long			: int;
  alignof_long			: int;
  sizeof_long_long		: int;
  alignof_long_long		: int;
  sizeof_float			: int;
  alignof_float			: int;
  sizeof_double			: int;
  alignof_double		: int;
  sizeof_long_double		: int;
  alignof_long_double		: int;
  sizeof_object_pointer		: int;
  alignof_object_pointer	: int;
  sizeof_function_pointer	: int;
  alignof_function_pointer	: int;

  size_t			: Ast.ctype;
  ptrdiff_t			: Ast.ctype;
  wchar_t			: Ast.ctype;

  preprocessor			: string;
  compiler			: string;
} with sexp


let default = {
  name				= "LP64 machine";

  underscore_names		= false;

  endian			= LittleEndian;

  max_alignment			= 16;

  char_bit			= 8;
  char_is_signed		= true;

  sizeof_short			= 2;
  alignof_short			= 2;
  sizeof_int			= 4;
  alignof_int			= 4;
  sizeof_long			= 8;
  alignof_long			= 8;
  sizeof_long_long		= 8;
  alignof_long_long		= 8;
  sizeof_float			= 4;
  alignof_float			= 4;
  sizeof_double			= 8;
  alignof_double		= 8;
  sizeof_long_double		= 16;
  alignof_long_double		= 16;
  sizeof_object_pointer		= 8;
  alignof_object_pointer	= 8;
  sizeof_function_pointer	= 8;
  alignof_function_pointer	= 8;

  size_t			= BasicType ULong;
  ptrdiff_t			= BasicType SLong;
  wchar_t			= BasicType SInt;

  preprocessor			= "/usr/lib/gcc-snapshot/bin/gcc-snapshot -Wfatal-errors -xc -E";
  compiler			= "/usr/lib/gcc-snapshot/bin/gcc-snapshot -Wfatal-errors -xc -fsyntax-only -";
}


let machine =
  try
    let machtype = Sys.getenv "MACHTYPE" in
    let ostype = Sys.getenv "OSTYPE" in
    let sexp = load_sexp ("extra/platform/" ^ machtype ^ "/" ^ ostype ^ ".lsp") in
    machine_of_sexp sexp
  with _ ->
    default


let name = machine.name

let underscore_names = machine.underscore_names

let endian = machine.endian

let max_alignment = machine.max_alignment

let char_bit = machine.char_bit
let char_is_signed = machine.char_is_signed

let sizeof_short = machine.sizeof_short
let alignof_short = machine.alignof_short
let sizeof_int = machine.sizeof_int
let alignof_int = machine.alignof_int
let sizeof_long = machine.sizeof_long
let alignof_long = machine.alignof_long
let sizeof_long_long = machine.sizeof_long_long
let alignof_long_long = machine.alignof_long_long
let sizeof_float = machine.sizeof_float
let alignof_float = machine.alignof_float
let sizeof_double = machine.sizeof_double
let alignof_double = machine.alignof_double
let sizeof_long_double = machine.sizeof_long_double
let alignof_long_double = machine.alignof_long_double
let sizeof_object_pointer = machine.sizeof_object_pointer
let alignof_object_pointer = machine.alignof_object_pointer
let sizeof_function_pointer = machine.sizeof_function_pointer
let alignof_function_pointer = machine.alignof_function_pointer

let size_t = machine.size_t
let ptrdiff_t = machine.ptrdiff_t
let wchar_t = machine.wchar_t

let preprocessor = machine.preprocessor
let compiler = machine.compiler


let print_version () =
  let open Printf in
  printf "fcc %d.%d for %s\n" version.major version.minor machine.name;
  printf "-- type sizes --\n";
  printf " type       | char | short | int | long | llong | float | double | ldouble | ptr | fptr\n";
  let print_size name =
    let name = name ^ "-bits" in
      printf  " %-10s |  %3d |   %3d | %3d |  %3d |   %3d |   %3d |    %3d |     %3d | %3d |  %3d\n"
        name
  in
  print_size
    "size"
    (machine.char_bit)
    (machine.char_bit * machine.sizeof_short)
    (machine.char_bit * machine.sizeof_int)
    (machine.char_bit * machine.sizeof_long)
    (machine.char_bit * machine.sizeof_long_long)
    (machine.char_bit * machine.sizeof_float)
    (machine.char_bit * machine.sizeof_double)
    (machine.char_bit * machine.sizeof_long_double)
    (machine.char_bit * machine.sizeof_object_pointer)
    (machine.char_bit * machine.sizeof_function_pointer)
    ;
  print_size
    "align"
    (machine.char_bit)
    (machine.char_bit * machine.alignof_short)
    (machine.char_bit * machine.alignof_int)
    (machine.char_bit * machine.alignof_long)
    (machine.char_bit * machine.alignof_long_long)
    (machine.char_bit * machine.alignof_float)
    (machine.char_bit * machine.alignof_double)
    (machine.char_bit * machine.alignof_long_double)
    (machine.char_bit * machine.alignof_object_pointer)
    (machine.char_bit * machine.alignof_function_pointer)
    ;
  printf "-- other platform specific information --\n";
  printf " maximum alignment      : %d bits\n" (machine.char_bit * machine.max_alignment);
  printf " external name mangling : %s\n" (if machine.underscore_names then "leading underscore" else "plain");
  printf " char signedness        : %s\n" (if machine.char_is_signed then "signed" else "unsigned");
  printf " endianness             : %s endian\n" (string_of_endianness machine.endian);
  printf " size_t                 : %s\n" (Codegen.code_of_type machine.size_t);
  printf " ptrdiff_t              : %s\n" (Codegen.code_of_type machine.ptrdiff_t);
  printf " wchar_t                : %s\n" (Codegen.code_of_type machine.wchar_t);
