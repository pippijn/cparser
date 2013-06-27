open Ast
open C_tokens
open Sclass
open Tqual


let make_table map =
  let table = Hashtbl.create (Array.length map) in
  Array.iter (fun (kwd, tok) -> Hashtbl.add table kwd tok) map;
  table

let token_table = make_table [|
  (* XXX: these are built-in typedefs *)
  "__m64",				TK_TYPEDEF_NAME "__m64";
  "__m256",				TK_TYPEDEF_NAME "__m256";
  "__m256i",				TK_TYPEDEF_NAME "__m256i";
  "__m256d",				TK_TYPEDEF_NAME "__m256d";
  "__int128_t",				TK_TYPEDEF_NAME "__int128_t";
  "__uint128_t",			TK_TYPEDEF_NAME "__uint128_t";

  "signed",				KW_SIGNED;
  "__signed",				KW_SIGNED;
  "__signed__",				KW_SIGNED;
  "unsigned",				KW_UNSIGNED;
  "__unsigned",				KW_UNSIGNED;
  "__unsigned__",			KW_UNSIGNED;
  "_Bool",				KW_BOOL;
  "char",				KW_CHAR;
  "short",				KW_SHORT;
  "int",				KW_INT;
  "long",				KW_LONG;
  "float",				KW_FLOAT;
  "double",				KW_DOUBLE;
  "void",				KW_VOID;

  "const",				KW_CONST;
  "__const",				KW_CONST;
  "__const__",				KW_CONST;
  "volatile",				KW_VOLATILE;
  "__volatile",				KW_VOLATILE;
  "__volatile__",			KW_VOLATILE;
  "restrict",				KW_RESTRICT;
  "__restrict",				KW_RESTRICT;
  "__restrict__",			KW_RESTRICT;

  "auto",				KW_AUTO;
  "extern",				KW_EXTERN;
  "inline",				KW_INLINE;
  "__inline",				KW_INLINE;
  "__inline__",				KW_INLINE;
  "register",				KW_REGISTER;
  "static",				KW_STATIC;
  "__thread",				KW_THREAD;
  "typedef",				KW_TYPEDEF;

  "enum",				KW_ENUM;
  "struct",				KW_STRUCT;
  "union",				KW_UNION;
  "__fc_datatype",			KW_DATATYPE;

  "break",				KW_BREAK;
  "case",				KW_CASE;
  "continue",				KW_CONTINUE;
  "default",				KW_DEFAULT;
  "do",					KW_DO;
  "else",				KW_ELSE;
  "for",				KW_FOR;
  "goto",				KW_GOTO;
  "if",					KW_IF;
  "return",				KW_RETURN;
  "sizeof",				KW_SIZEOF;
  "switch",				KW_SWITCH;
  "while",				KW_WHILE;

  "_Complex",				KW_COMPLEX;
  "__complex",				KW_COMPLEX;
  "__complex__",			KW_COMPLEX;
  "__alignof",				KW_ALIGNOF;
  "__alignof__",			KW_ALIGNOF;
  "asm",				KW_ASM;
  "__asm",				KW_ASM;
  "__asm__",				KW_ASM;
  "__builtin_offsetof",			KW_OFFSETOF;
  "__builtin_types_compatible_p",	KW_TYPES_COMPATIBLE_P;
  "__builtin_va_arg",			KW_VA_ARG;
  "__builtin_va_list",			KW_VA_LIST;
  "__builtin_ms_va_list",		KW_VA_LIST;
  "__builtin_sysv_va_list",		KW_VA_LIST;
  "__extension__",			KW_EXTENSION;
  "_fastcall",				KW_FASTCALL;
  "__fastcall",				KW_FASTCALL;
  "__imag",				KW_IMAG;
  "__imag__",				KW_IMAG;
  "__label__",				KW_LABEL;
  "__real",				KW_REAL;
  "__real__",				KW_REAL;
  "typeof",				KW_TYPEOF;
  "__typeof",				KW_TYPEOF;
  "__typeof__",				KW_TYPEOF;
|]

let attribute_table = make_table [|
  "alias",				KW_ATTR_ALIAS;
  "aligned",				KW_ATTR_ALIGNED;
  "alloc_size",				KW_ATTR_ALLOC_SIZE;
  "altivec",				KW_ATTR_ALTIVEC;
  "always_inline",			KW_ATTR_ALWAYS_INLINE;
  "artificial",				KW_ATTR_ARTIFICIAL;
  "back_switch",			KW_ATTR_BACK_SWITCH;
  "based",				KW_ATTR_BASED;
  "below100",				KW_ATTR_BELOW100;
  "callee_pop_aggregate_return",	KW_ATTR_CALLEE_POP_AGGREGATE_RETURN;
  "cb",					KW_ATTR_CB;
  "cdecl",				KW_ATTR_CDECL;
  "cleanup",				KW_ATTR_CLEANUP;
  "cold",				KW_ATTR_COLD;
  "const",				KW_ATTR_CONST;
  "common",				KW_ATTR_COMMON;
  "constructor",			KW_ATTR_CONSTRUCTOR;
  "deprecated",				KW_ATTR_DEPRECATED;
  "destructor",				KW_ATTR_DESTRUCTOR;
  "disinterrupt",			KW_ATTR_DISINTERRUPT;
  "dllexport",				KW_ATTR_DLLEXPORT;
  "dllimport",				KW_ATTR_DLLIMPORT;
  "eightbit_data",			KW_ATTR_EIGHTBIT_DATA;
  "exception_handler",			KW_ATTR_EXCEPTION_HANDLER;
  "externally_visible",			KW_ATTR_EXTERNALLY_VISIBLE;
  "far",				KW_ATTR_FAR;
  "fastcall",				KW_ATTR_FASTCALL;
  "fast_interrupt",			KW_ATTR_FAST_INTERRUPT;
  "flatten",				KW_ATTR_FLATTEN;
  "force_align_arg_pointer",		KW_ATTR_FORCE_ALIGN_ARG_POINTER;
  "format_arg",				KW_ATTR_FORMAT_ARG;
  "format",				KW_ATTR_FORMAT;
  "function_vector",			KW_ATTR_FUNCTION_VECTOR;
  "gcc_struct",				KW_ATTR_GCC_STRUCT;
  "gnu_inline",				KW_ATTR_GNU_INLINE;
  "hot",				KW_ATTR_HOT;
  "ifunc",				KW_ATTR_IFUNC;
  "interrupt_handler",			KW_ATTR_INTERRUPT_HANDLER;
  "interrupt_thread",			KW_ATTR_INTERRUPT_THREAD;
  "interrupt",				KW_ATTR_INTERRUPT;
  "io",					KW_ATTR_IO;
  "isr",				KW_ATTR_ISR;
  "kspisusp",				KW_ATTR_KSPISUSP;
  "l1_data_A",				KW_ATTR_L1_DATA_A;
  "l1_data_B",				KW_ATTR_L1_DATA_B;
  "l1_data",				KW_ATTR_L1_DATA;
  "l1_text",				KW_ATTR_L1_TEXT;
  "l2",					KW_ATTR_L2;
  "leaf",				KW_ATTR_LEAF;
  "long_call",				KW_ATTR_LONG_CALL;
  "longcall",				KW_ATTR_LONGCALL;
  "malloc",				KW_ATTR_MALLOC;
  "may_alias",				KW_ATTR_MAY_ALIAS;
  "mips16",				KW_ATTR_MIPS16;
  "model",				KW_ATTR_MODEL;
  "mode",				KW_ATTR_MODE;
  "ms_abi",				KW_ATTR_MS_ABI;
  "ms_hook_prologue",			KW_ATTR_MS_HOOK_PROLOGUE;
  "ms_struct",				KW_ATTR_MS_STRUCT;
  "naked",				KW_ATTR_NAKED;
  "near",				KW_ATTR_NEAR;
  "nesting",				KW_ATTR_NESTING;
  "nmi_handler",			KW_ATTR_NMI_HANDLER;
  "noclone",				KW_ATTR_NOCLONE;
  "nocommon",				KW_ATTR_NOCOMMON;
  "noinline",				KW_ATTR_NOINLINE;
  "no_instrument_function",		KW_ATTR_NO_INSTRUMENT_FUNCTION;
  "nomips16",				KW_ATTR_NOMIPS16;
  "nonnull",				KW_ATTR_NONNULL;
  "noreturn",				KW_ATTR_NORETURN;
  "no_split_stack",			KW_ATTR_NO_SPLIT_STACK;
  "nothrow",				KW_ATTR_NOTHROW;
  "notshared",				KW_ATTR_NOTSHARED;
  "optimize",				KW_ATTR_OPTIMIZE;
  "OS_main",				KW_ATTR_OS_MAIN;
  "OS_task",				KW_ATTR_OS_TASK;
  "packed",				KW_ATTR_PACKED;
  "pcs",				KW_ATTR_PCS;
  "progmem",				KW_ATTR_PROGMEM;
  "pure",				KW_ATTR_PURE;
  "regparm",				KW_ATTR_REGPARM;
  "resbank",				KW_ATTR_RESBANK;
  "returns_twice",			KW_ATTR_RETURNS_TWICE;
  "saveall",				KW_ATTR_SAVEALL;
  "save_volatiles",			KW_ATTR_SAVE_VOLATILES;
  "section",				KW_ATTR_SECTION;
  "selectany",				KW_ATTR_SELECTANY;
  "sentinel",				KW_ATTR_SENTINEL;
  "shared",				KW_ATTR_SHARED;
  "short_call",				KW_ATTR_SHORT_CALL;
  "shortcall",				KW_ATTR_SHORTCALL;
  "signal",				KW_ATTR_SIGNAL;
  "sp_switch",				KW_ATTR_SP_SWITCH;
  "spu_vector",				KW_ATTR_SPU_VECTOR;
  "sseregparm",				KW_ATTR_SSEREGPARM;
  "stdcall",				KW_ATTR_STDCALL;
  "syscall_linkage",			KW_ATTR_SYSCALL_LINKAGE;
  "sysv_abi",				KW_ATTR_SYSV_ABI;
  "target",				KW_ATTR_TARGET;
  "thiscall",				KW_ATTR_THISCALL;
  "tiny_data",				KW_ATTR_TINY_DATA;
  "tiny",				KW_ATTR_TINY;
  "tls_model",				KW_ATTR_TLS_MODEL;
  "transparent_union",			KW_ATTR_TRANSPARENT_UNION;
  "trap_exit",				KW_ATTR_TRAP_EXIT;
  "unused",				KW_ATTR_UNUSED;
  "used",				KW_ATTR_USED;
  "vector_size",			KW_ATTR_VECTOR_SIZE;
  "version_id",				KW_ATTR_VERSION_ID;
  "visibility",				KW_ATTR_VISIBILITY;
  "vliw",				KW_ATTR_VLIW;
  "warn_unused_result",			KW_ATTR_WARN_UNUSED_RESULT;
  "weakref",				KW_ATTR_WEAKREF;
  "weak",				KW_ATTR_WEAK;
|]


let string_of_token = function
  | KW_ATTRIBUTE -> "__attribute__"
  | KW_DECLSPEC -> "__declspec"

  | KW_ATTR_ALIAS -> "__alias__"
  | KW_ATTR_ALIGNED -> "__aligned__"
  | KW_ATTR_ALLOC_SIZE -> "__alloc_size__"
  | KW_ATTR_ALTIVEC -> "__altivec__"
  | KW_ATTR_ALWAYS_INLINE -> "__always_inline__"
  | KW_ATTR_ARTIFICIAL -> "__artificial__"
  | KW_ATTR_BACK_SWITCH -> "__back_switch__"
  | KW_ATTR_BASED -> "__based__"
  | KW_ATTR_BELOW100 -> "__below100__"
  | KW_ATTR_CALLEE_POP_AGGREGATE_RETURN -> "__callee_pop_aggregate_return__"
  | KW_ATTR_CB -> "__cb__"
  | KW_ATTR_CDECL -> "__cdecl__"
  | KW_ATTR_CLEANUP -> "__cleanup__"
  | KW_ATTR_COLD -> "__cold__"
  | KW_ATTR_COMMON -> "__common__"
  | KW_ATTR_CONST -> "__const__"
  | KW_ATTR_CONSTRUCTOR -> "__constructor__"
  | KW_ATTR_DEPRECATED -> "__deprecated__"
  | KW_ATTR_DESTRUCTOR -> "__destructor__"
  | KW_ATTR_DISINTERRUPT -> "__disinterrupt__"
  | KW_ATTR_DLLEXPORT -> "__dllexport__"
  | KW_ATTR_DLLIMPORT -> "__dllimport__"
  | KW_ATTR_EIGHTBIT_DATA -> "__eightbit_data__"
  | KW_ATTR_EXCEPTION_HANDLER -> "__exception_handler__"
  | KW_ATTR_EXTERNALLY_VISIBLE -> "__externally_visible__"
  | KW_ATTR_FAR -> "__far__"
  | KW_ATTR_FASTCALL -> "__fastcall__"
  | KW_ATTR_FAST_INTERRUPT -> "__fast_interrupt__"
  | KW_ATTR_FLATTEN -> "__flatten__"
  | KW_ATTR_FORCE_ALIGN_ARG_POINTER -> "__force_align_arg_pointer__"
  | KW_ATTR_FORMAT -> "__format__"
  | KW_ATTR_FORMAT_ARG -> "__format_arg__"
  | KW_ATTR_FUNCTION_VECTOR -> "__function_vector__"
  | KW_ATTR_GCC_STRUCT -> "__gcc_struct__"
  | KW_ATTR_GNU_INLINE -> "__gnu_inline__"
  | KW_ATTR_HOT -> "__hot__"
  | KW_ATTR_IFUNC -> "__ifunc__"
  | KW_ATTR_INTERRUPT -> "__interrupt__"
  | KW_ATTR_INTERRUPT_HANDLER -> "__interrupt_handler__"
  | KW_ATTR_INTERRUPT_THREAD -> "__interrupt_thread__"
  | KW_ATTR_IO -> "__io__"
  | KW_ATTR_ISR -> "__isr__"
  | KW_ATTR_KSPISUSP -> "__kspisusp__"
  | KW_ATTR_L1_DATA -> "__l1_data__"
  | KW_ATTR_L1_DATA_A -> "__l1_data_a__"
  | KW_ATTR_L1_DATA_B -> "__l1_data_b__"
  | KW_ATTR_L1_TEXT -> "__l1_text__"
  | KW_ATTR_L2 -> "__l2__"
  | KW_ATTR_LEAF -> "__leaf__"
  | KW_ATTR_LONGCALL -> "__longcall__"
  | KW_ATTR_LONG_CALL -> "__long_call__"
  | KW_ATTR_MALLOC -> "__malloc__"
  | KW_ATTR_MAY_ALIAS -> "__may_alias__"
  | KW_ATTR_MIPS16 -> "__mips16__"
  | KW_ATTR_MODE -> "__mode__"
  | KW_ATTR_MODEL -> "__model__"
  | KW_ATTR_MS_ABI -> "__ms_abi__"
  | KW_ATTR_MS_HOOK_PROLOGUE -> "__ms_hook_prologue__"
  | KW_ATTR_MS_STRUCT -> "__ms_struct__"
  | KW_ATTR_NAKED -> "__naked__"
  | KW_ATTR_NEAR -> "__near__"
  | KW_ATTR_NESTING -> "__nesting__"
  | KW_ATTR_NMI_HANDLER -> "__nmi_handler__"
  | KW_ATTR_NOCLONE -> "__noclone__"
  | KW_ATTR_NOCOMMON -> "__nocommon__"
  | KW_ATTR_NOINLINE -> "__noinline__"
  | KW_ATTR_NO_INSTRUMENT_FUNCTION -> "__no_instrument_function__"
  | KW_ATTR_NOMIPS16 -> "__nomips16__"
  | KW_ATTR_NONNULL -> "__nonnull__"
  | KW_ATTR_NORETURN -> "__noreturn__"
  | KW_ATTR_NO_SPLIT_STACK -> "__no_split_stack__"
  | KW_ATTR_NOTHROW -> "__nothrow__"
  | KW_ATTR_NOTSHARED -> "__notshared__"
  | KW_ATTR_OPTIMIZE -> "__optimize__"
  | KW_ATTR_OS_MAIN -> "__os_main__"
  | KW_ATTR_OS_TASK -> "__os_task__"
  | KW_ATTR_PACKED -> "__packed__"
  | KW_ATTR_PCS -> "__pcs__"
  | KW_ATTR_PROGMEM -> "__progmem__"
  | KW_ATTR_PURE -> "__pure__"
  | KW_ATTR_REGPARM -> "__regparm__"
  | KW_ATTR_RESBANK -> "__resbank__"
  | KW_ATTR_RETURNS_TWICE -> "__returns_twice__"
  | KW_ATTR_SAVEALL -> "__saveall__"
  | KW_ATTR_SAVE_VOLATILES -> "__save_volatiles__"
  | KW_ATTR_SECTION -> "__section__"
  | KW_ATTR_SELECTANY -> "__selectany__"
  | KW_ATTR_SENTINEL -> "__sentinel__"
  | KW_ATTR_SHARED -> "__shared__"
  | KW_ATTR_SHORTCALL -> "__shortcall__"
  | KW_ATTR_SHORT_CALL -> "__short_call__"
  | KW_ATTR_SIGNAL -> "__signal__"
  | KW_ATTR_SP_SWITCH -> "__sp_switch__"
  | KW_ATTR_SPU_VECTOR -> "__spu_vector__"
  | KW_ATTR_SSEREGPARM -> "__sseregparm__"
  | KW_ATTR_STDCALL -> "__stdcall__"
  | KW_ATTR_SYSCALL_LINKAGE -> "__syscall_linkage__"
  | KW_ATTR_SYSV_ABI -> "__sysv_abi__"
  | KW_ATTR_TARGET -> "__target__"
  | KW_ATTR_THISCALL -> "__thiscall__"
  | KW_ATTR_TINY -> "__tiny__"
  | KW_ATTR_TINY_DATA -> "__tiny_data__"
  | KW_ATTR_TLS_MODEL -> "__tls_model__"
  | KW_ATTR_TRANSPARENT_UNION -> "__transparent_union__"
  | KW_ATTR_TRAP_EXIT -> "__trap_exit__"
  | KW_ATTR_UNUSED -> "__unused__"
  | KW_ATTR_USED -> "__used__"
  | KW_ATTR_VECTOR_SIZE -> "__vector_size__"
  | KW_ATTR_VERSION_ID -> "__version_id__"
  | KW_ATTR_VISIBILITY -> "__visibility__"
  | KW_ATTR_VLIW -> "__vliw__"
  | KW_ATTR_WARN_UNUSED_RESULT -> "__warn_unused_result__"
  | KW_ATTR_WEAK -> "__weak__"
  | KW_ATTR_WEAKREF -> "__weakref__"

  | KW_ALIGNOF -> "__alignof"
  | KW_ASM -> "__asm"
  | KW_COMPLEX -> "_Complex"
  | KW_EXTENSION -> "__extension__"
  | KW_FASTCALL -> "__fastcall"
  | KW_IMAG -> "__imag"
  | KW_INLINE -> "__inline"
  | KW_REAL -> "__real"
  | KW_RESTRICT -> "__restrict"
  | KW_TYPEOF -> "__typeof"
  | KW_OFFSETOF -> "__builtin_offsetof"
  | KW_TYPES_COMPATIBLE_P -> "__builtin_types_compatible_p"
  | KW_VA_ARG -> "__builtin_va_arg"
  | KW_VA_LIST -> "__builtin_va_list"

  | KW_AUTO -> "auto"
  | KW_BREAK -> "break"
  | KW_CASE -> "case"
  | KW_BOOL -> "_Bool"
  | KW_INTN bits -> "__int" ^ (string_of_int bits)
  | KW_CHAR -> "char"
  | KW_CONST -> "const"
  | KW_CONTINUE -> "continue"
  | KW_DEFAULT -> "default"
  | KW_DO -> "do"
  | KW_DOUBLE -> "double"
  | KW_ELSE -> "else"
  | KW_ENUM -> "enum"
  | KW_EXTERN -> "extern"
  | KW_FLOAT -> "float"
  | KW_FLOATN bits -> "__float" ^ (string_of_int bits)
  | KW_DECIMALN bits -> "_Decimal" ^ (string_of_int bits)
  | KW_FOR -> "for"
  | KW_GOTO -> "goto"
  | KW_IF -> "if"
  | KW_INT -> "int"
  | KW_LABEL -> "__label__"
  | KW_LONG -> "long"
  | KW_REGISTER -> "register"
  | KW_RETURN -> "return"
  | KW_SHORT -> "short"
  | KW_SIGNED -> "signed"
  | KW_SIZEOF -> "sizeof"
  | KW_STATIC -> "static"
  | KW_THREAD -> "__thread"
  | KW_STRUCT -> "struct"
  | KW_DATATYPE -> "__fc_datatype"
  | KW_SWITCH -> "switch"
  | KW_TYPEDEF -> "typedef"
  | KW_UNION -> "union"
  | KW_UNSIGNED -> "unsigned"
  | KW_VOID -> "void"
  | KW_VOLATILE -> "volatile"
  | KW_WCHAR_T -> "wchar_t"
  | KW_WHILE -> "while"

  | TK_AND -> "&"
  | TK_ANDAND -> "&&"
  | TK_AND_EQ -> "&="
  | TK_ARROW -> "->"
  | TK_CARET -> "^"
  | TK_CARET_EQ -> "^="
  | TK_COLON -> ":"
  | TK_COMMA -> ","
  | TK_DEC -> "--"
  | TK_ELLIPSIS -> "..."
  | TK_EQEQ -> "=="
  | TK_EQUALS -> "="
  | TK_EXMARK -> "!"
  | TK_GREATER -> ">"
  | TK_GREATER_EQ -> ">="
  | TK_GTGT -> ">>"
  | TK_GTGT_EQ -> ">>="
  | TK_INC -> "++"
  | TK_LBRACE -> "{"
  | TK_LBRACK -> "("
  | TK_LESS -> "<"
  | TK_LESS_EQ -> "<="
  | TK_LSQBRACK -> "["
  | TK_LTLT -> "<<"
  | TK_LTLT_EQ -> "<<="
  | TK_MINUS -> "-"
  | TK_MINUS_EQ -> "-="
  | TK_NE -> "!="
  | TK_PERCENT -> "%"
  | TK_PERCENT_EQ -> "%="
  | TK_PERIOD -> "."
  | TK_PIPE -> "|"
  | TK_PIPE_EQ -> "|="
  | TK_PIPEPIPE -> "||"
  | TK_PLUS -> "+"
  | TK_PLUS_EQ -> "+="
  | TK_QMARK -> "?"
  | TK_RBRACE -> "}"
  | TK_RBRACK -> ")"
  | TK_RSQBRACK -> "]"
  | TK_SEMICOLON -> ";"
  | TK_SLASH -> "/"
  | TK_SLASH_EQ -> "/="
  | TK_STAR -> "*"
  | TK_STAR_EQ -> "*="
  | TK_TILDE -> "~"

  | TK_STRING_LITERAL lit
  | TK_TYPEDEF_NAME lit
  | TK_WCHAR_CONSTANT lit
  | TK_WSTRING_LITERAL lit
  | TK_INCLUDE lit
  | WC_DECL lit
  | WC_EXPR lit
  | WC_TYPE lit
  | TK_CHAR_CONSTANT lit
  | TK_IDENTIFIER lit ->
      lit

  | TK_FLOATING_CONSTANT (lit, suffix)
  | TK_HEX_FLOATING_CONSTANT (lit, suffix)

  | TK_INTEGER_CONSTANT (lit, suffix)
  | TK_HEX_CONSTANT (lit, suffix)
  | TK_BIN_CONSTANT (lit, suffix)
  | TK_OCTAL_CONSTANT (lit, suffix) ->
      begin match suffix with
      | None -> lit
      | Some suffix -> lit ^ suffix
      end

  | EOF -> "<eof>"


let token_name = function
  | EOF -> "EOF"
  | KW_ALIGNOF -> "KW_ALIGNOF"
  | KW_ASM -> "KW_ASM"
  | KW_ATTR_ALIAS -> "KW_ATTR_ALIAS"
  | KW_ATTR_ALIGNED -> "KW_ATTR_ALIGNED"
  | KW_ATTR_ALLOC_SIZE -> "KW_ATTR_ALLOC_SIZE"
  | KW_ATTR_ALTIVEC -> "KW_ATTR_ALTIVEC"
  | KW_ATTR_ALWAYS_INLINE -> "KW_ATTR_ALWAYS_INLINE"
  | KW_ATTR_ARTIFICIAL -> "KW_ATTR_ARTIFICIAL"
  | KW_ATTR_BACK_SWITCH -> "KW_ATTR_BACK_SWITCH"
  | KW_ATTR_BASED -> "KW_ATTR_BASED"
  | KW_ATTR_BELOW100 -> "KW_ATTR_BELOW100"
  | KW_ATTR_CALLEE_POP_AGGREGATE_RETURN -> "KW_ATTR_CALLEE_POP_AGGREGATE_RETURN"
  | KW_ATTR_CB -> "KW_ATTR_CB"
  | KW_ATTR_CDECL -> "KW_ATTR_CDECL"
  | KW_ATTR_CLEANUP -> "KW_ATTR_CLEANUP"
  | KW_ATTR_COLD -> "KW_ATTR_COLD"
  | KW_ATTR_COMMON -> "KW_ATTR_COMMON"
  | KW_ATTR_CONST -> "KW_ATTR_CONST"
  | KW_ATTR_CONSTRUCTOR -> "KW_ATTR_CONSTRUCTOR"
  | KW_ATTR_DEPRECATED -> "KW_ATTR_DEPRECATED"
  | KW_ATTR_DESTRUCTOR -> "KW_ATTR_DESTRUCTOR"
  | KW_ATTR_DISINTERRUPT -> "KW_ATTR_DISINTERRUPT"
  | KW_ATTR_DLLEXPORT -> "KW_ATTR_DLLEXPORT"
  | KW_ATTR_DLLIMPORT -> "KW_ATTR_DLLIMPORT"
  | KW_ATTR_EIGHTBIT_DATA -> "KW_ATTR_EIGHTBIT_DATA"
  | KW_ATTR_EXCEPTION_HANDLER -> "KW_ATTR_EXCEPTION_HANDLER"
  | KW_ATTR_EXTERNALLY_VISIBLE -> "KW_ATTR_EXTERNALLY_VISIBLE"
  | KW_ATTR_FAR -> "KW_ATTR_FAR"
  | KW_ATTR_FASTCALL -> "KW_ATTR_FASTCALL"
  | KW_ATTR_FAST_INTERRUPT -> "KW_ATTR_FAST_INTERRUPT"
  | KW_ATTR_FLATTEN -> "KW_ATTR_FLATTEN"
  | KW_ATTR_FORCE_ALIGN_ARG_POINTER -> "KW_ATTR_FORCE_ALIGN_ARG_POINTER"
  | KW_ATTR_FORMAT_ARG -> "KW_ATTR_FORMAT_ARG"
  | KW_ATTR_FORMAT -> "KW_ATTR_FORMAT"
  | KW_ATTR_FUNCTION_VECTOR -> "KW_ATTR_FUNCTION_VECTOR"
  | KW_ATTR_GCC_STRUCT -> "KW_ATTR_GCC_STRUCT"
  | KW_ATTR_GNU_INLINE -> "KW_ATTR_GNU_INLINE"
  | KW_ATTR_HOT -> "KW_ATTR_HOT"
  | KW_ATTRIBUTE -> "KW_ATTRIBUTE"
  | KW_ATTR_IFUNC -> "KW_ATTR_IFUNC"
  | KW_ATTR_INTERRUPT_HANDLER -> "KW_ATTR_INTERRUPT_HANDLER"
  | KW_ATTR_INTERRUPT -> "KW_ATTR_INTERRUPT"
  | KW_ATTR_INTERRUPT_THREAD -> "KW_ATTR_INTERRUPT_THREAD"
  | KW_ATTR_IO -> "KW_ATTR_IO"
  | KW_ATTR_ISR -> "KW_ATTR_ISR"
  | KW_ATTR_KSPISUSP -> "KW_ATTR_KSPISUSP"
  | KW_ATTR_L1_DATA_A -> "KW_ATTR_L1_DATA_A"
  | KW_ATTR_L1_DATA_B -> "KW_ATTR_L1_DATA_B"
  | KW_ATTR_L1_DATA -> "KW_ATTR_L1_DATA"
  | KW_ATTR_L1_TEXT -> "KW_ATTR_L1_TEXT"
  | KW_ATTR_L2 -> "KW_ATTR_L2"
  | KW_ATTR_LEAF -> "KW_ATTR_LEAF"
  | KW_ATTR_LONG_CALL -> "KW_ATTR_LONG_CALL"
  | KW_ATTR_LONGCALL -> "KW_ATTR_LONGCALL"
  | KW_ATTR_MALLOC -> "KW_ATTR_MALLOC"
  | KW_ATTR_MAY_ALIAS -> "KW_ATTR_MAY_ALIAS"
  | KW_ATTR_MIPS16 -> "KW_ATTR_MIPS16"
  | KW_ATTR_MODE -> "KW_ATTR_MODE"
  | KW_ATTR_MODEL -> "KW_ATTR_MODEL"
  | KW_ATTR_MS_ABI -> "KW_ATTR_MS_ABI"
  | KW_ATTR_MS_HOOK_PROLOGUE -> "KW_ATTR_MS_HOOK_PROLOGUE"
  | KW_ATTR_MS_STRUCT -> "KW_ATTR_MS_STRUCT"
  | KW_ATTR_NAKED -> "KW_ATTR_NAKED"
  | KW_ATTR_NEAR -> "KW_ATTR_NEAR"
  | KW_ATTR_NESTING -> "KW_ATTR_NESTING"
  | KW_ATTR_NMI_HANDLER -> "KW_ATTR_NMI_HANDLER"
  | KW_ATTR_NOCLONE -> "KW_ATTR_NOCLONE"
  | KW_ATTR_NOCOMMON -> "KW_ATTR_NOCOMMON"
  | KW_ATTR_NOINLINE -> "KW_ATTR_NOINLINE"
  | KW_ATTR_NO_INSTRUMENT_FUNCTION -> "KW_ATTR_NO_INSTRUMENT_FUNCTION"
  | KW_ATTR_NOMIPS16 -> "KW_ATTR_NOMIPS16"
  | KW_ATTR_NONNULL -> "KW_ATTR_NONNULL"
  | KW_ATTR_NORETURN -> "KW_ATTR_NORETURN"
  | KW_ATTR_NO_SPLIT_STACK -> "KW_ATTR_NO_SPLIT_STACK"
  | KW_ATTR_NOTHROW -> "KW_ATTR_NOTHROW"
  | KW_ATTR_NOTSHARED -> "KW_ATTR_NOTSHARED"
  | KW_ATTR_OPTIMIZE -> "KW_ATTR_OPTIMIZE"
  | KW_ATTR_OS_MAIN -> "KW_ATTR_OS_MAIN"
  | KW_ATTR_OS_TASK -> "KW_ATTR_OS_TASK"
  | KW_ATTR_PACKED -> "KW_ATTR_PACKED"
  | KW_ATTR_PCS -> "KW_ATTR_PCS"
  | KW_ATTR_PROGMEM -> "KW_ATTR_PROGMEM"
  | KW_ATTR_PURE -> "KW_ATTR_PURE"
  | KW_ATTR_REGPARM -> "KW_ATTR_REGPARM"
  | KW_ATTR_RESBANK -> "KW_ATTR_RESBANK"
  | KW_ATTR_RETURNS_TWICE -> "KW_ATTR_RETURNS_TWICE"
  | KW_ATTR_SAVEALL -> "KW_ATTR_SAVEALL"
  | KW_ATTR_SAVE_VOLATILES -> "KW_ATTR_SAVE_VOLATILES"
  | KW_ATTR_SECTION -> "KW_ATTR_SECTION"
  | KW_ATTR_SELECTANY -> "KW_ATTR_SELECTANY"
  | KW_ATTR_SENTINEL -> "KW_ATTR_SENTINEL"
  | KW_ATTR_SHARED -> "KW_ATTR_SHARED"
  | KW_ATTR_SHORT_CALL -> "KW_ATTR_SHORT_CALL"
  | KW_ATTR_SHORTCALL -> "KW_ATTR_SHORTCALL"
  | KW_ATTR_SIGNAL -> "KW_ATTR_SIGNAL"
  | KW_ATTR_SP_SWITCH -> "KW_ATTR_SP_SWITCH"
  | KW_ATTR_SPU_VECTOR -> "KW_ATTR_SPU_VECTOR"
  | KW_ATTR_SSEREGPARM -> "KW_ATTR_SSEREGPARM"
  | KW_ATTR_STDCALL -> "KW_ATTR_STDCALL"
  | KW_ATTR_SYSCALL_LINKAGE -> "KW_ATTR_SYSCALL_LINKAGE"
  | KW_ATTR_SYSV_ABI -> "KW_ATTR_SYSV_ABI"
  | KW_ATTR_TARGET -> "KW_ATTR_TARGET"
  | KW_ATTR_THISCALL -> "KW_ATTR_THISCALL"
  | KW_ATTR_TINY_DATA -> "KW_ATTR_TINY_DATA"
  | KW_ATTR_TINY -> "KW_ATTR_TINY"
  | KW_ATTR_TLS_MODEL -> "KW_ATTR_TLS_MODEL"
  | KW_ATTR_TRANSPARENT_UNION -> "KW_ATTR_TRANSPARENT_UNION"
  | KW_ATTR_TRAP_EXIT -> "KW_ATTR_TRAP_EXIT"
  | KW_ATTR_UNUSED -> "KW_ATTR_UNUSED"
  | KW_ATTR_USED -> "KW_ATTR_USED"
  | KW_ATTR_VECTOR_SIZE -> "KW_ATTR_VECTOR_SIZE"
  | KW_ATTR_VERSION_ID -> "KW_ATTR_VERSION_ID"
  | KW_ATTR_VISIBILITY -> "KW_ATTR_VISIBILITY"
  | KW_ATTR_VLIW -> "KW_ATTR_VLIW"
  | KW_ATTR_WARN_UNUSED_RESULT -> "KW_ATTR_WARN_UNUSED_RESULT"
  | KW_ATTR_WEAK -> "KW_ATTR_WEAK"
  | KW_ATTR_WEAKREF -> "KW_ATTR_WEAKREF"
  | KW_AUTO -> "KW_AUTO"
  | KW_BOOL -> "KW_BOOL"
  | KW_BREAK -> "KW_BREAK"
  | KW_CASE -> "KW_CASE"
  | KW_CHAR -> "KW_CHAR"
  | KW_COMPLEX -> "KW_COMPLEX"
  | KW_CONST -> "KW_CONST"
  | KW_CONTINUE -> "KW_CONTINUE"
  | KW_DECIMALN _ -> "KW_DECIMALN"
  | KW_DECLSPEC -> "KW_DECLSPEC"
  | KW_DEFAULT -> "KW_DEFAULT"
  | KW_DO -> "KW_DO"
  | KW_DOUBLE -> "KW_DOUBLE"
  | KW_ELSE -> "KW_ELSE"
  | KW_ENUM -> "KW_ENUM"
  | KW_EXTENSION -> "KW_EXTENSION"
  | KW_EXTERN -> "KW_EXTERN"
  | KW_FASTCALL -> "KW_FASTCALL"
  | KW_FLOATN _ -> "KW_FLOATN"
  | KW_FLOAT -> "KW_FLOAT"
  | KW_FOR -> "KW_FOR"
  | KW_GOTO -> "KW_GOTO"
  | KW_IF -> "KW_IF"
  | KW_IMAG -> "KW_IMAG"
  | KW_INLINE -> "KW_INLINE"
  | KW_INTN _ -> "KW_INTN"
  | KW_INT -> "KW_INT"
  | KW_LABEL -> "KW_LABEL"
  | KW_LONG -> "KW_LONG"
  | KW_OFFSETOF -> "KW_OFFSETOF"
  | KW_REAL -> "KW_REAL"
  | KW_REGISTER -> "KW_REGISTER"
  | KW_RESTRICT -> "KW_RESTRICT"
  | KW_RETURN -> "KW_RETURN"
  | KW_SHORT -> "KW_SHORT"
  | KW_SIGNED -> "KW_SIGNED"
  | KW_SIZEOF -> "KW_SIZEOF"
  | KW_STATIC -> "KW_STATIC"
  | KW_STRUCT -> "KW_STRUCT"
  | KW_DATATYPE -> "KW_DATATYPE"
  | KW_SWITCH -> "KW_SWITCH"
  | KW_THREAD -> "KW_THREAD"
  | KW_TYPEDEF -> "KW_TYPEDEF"
  | KW_TYPEOF -> "KW_TYPEOF"
  | KW_TYPES_COMPATIBLE_P -> "KW_TYPES_COMPATIBLE_P"
  | KW_UNION -> "KW_UNION"
  | KW_UNSIGNED -> "KW_UNSIGNED"
  | KW_VA_ARG -> "KW_VA_ARG"
  | KW_VA_LIST -> "KW_VA_LIST"
  | KW_VOID -> "KW_VOID"
  | KW_VOLATILE -> "KW_VOLATILE"
  | KW_WCHAR_T -> "KW_WCHAR_T"
  | KW_WHILE -> "KW_WHILE"
  | TK_ANDAND -> "TK_ANDAND"
  | TK_AND_EQ -> "TK_AND_EQ"
  | TK_AND -> "TK_AND"
  | TK_ARROW -> "TK_ARROW"
  | TK_CARET_EQ -> "TK_CARET_EQ"
  | TK_CARET -> "TK_CARET"
  | TK_CHAR_CONSTANT _ -> "TK_CHAR_CONSTANT"
  | TK_COLON -> "TK_COLON"
  | TK_COMMA -> "TK_COMMA"
  | TK_DEC -> "TK_DEC"
  | TK_ELLIPSIS -> "TK_ELLIPSIS"
  | TK_EQEQ -> "TK_EQEQ"
  | TK_EQUALS -> "TK_EQUALS"
  | TK_EXMARK -> "TK_EXMARK"
  | TK_FLOATING_CONSTANT _ -> "TK_FLOATING_CONSTANT"
  | TK_HEX_FLOATING_CONSTANT _ -> "TK_HEX_FLOATING_CONSTANT"
  | TK_GREATER_EQ -> "TK_GREATER_EQ"
  | TK_GREATER -> "TK_GREATER"
  | TK_GTGT_EQ -> "TK_GTGT_EQ"
  | TK_GTGT -> "TK_GTGT"
  | TK_HEX_CONSTANT _ -> "TK_HEX_CONSTANT"
  | TK_BIN_CONSTANT _ -> "TK_BIN_CONSTANT"
  | TK_IDENTIFIER _ -> "TK_IDENTIFIER"
  | TK_INCLUDE _ -> "TK_INCLUDE"
  | TK_INC -> "TK_INC"
  | TK_INTEGER_CONSTANT _ -> "TK_INTEGER_CONSTANT"
  | TK_LBRACE -> "TK_LBRACE"
  | TK_LBRACK -> "TK_LBRACK"
  | TK_LESS_EQ -> "TK_LESS_EQ"
  | TK_LESS -> "TK_LESS"
  | TK_LSQBRACK -> "TK_LSQBRACK"
  | TK_LTLT_EQ -> "TK_LTLT_EQ"
  | TK_LTLT -> "TK_LTLT"
  | TK_MINUS_EQ -> "TK_MINUS_EQ"
  | TK_MINUS -> "TK_MINUS"
  | TK_NE -> "TK_NE"
  | TK_OCTAL_CONSTANT _ -> "TK_OCTAL_CONSTANT"
  | TK_PERCENT_EQ -> "TK_PERCENT_EQ"
  | TK_PERCENT -> "TK_PERCENT"
  | TK_PERIOD -> "TK_PERIOD"
  | TK_PIPE_EQ -> "TK_PIPE_EQ"
  | TK_PIPEPIPE -> "TK_PIPEPIPE"
  | TK_PIPE -> "TK_PIPE"
  | TK_PLUS_EQ -> "TK_PLUS_EQ"
  | TK_PLUS -> "TK_PLUS"
  | TK_QMARK -> "TK_QMARK"
  | TK_RBRACE -> "TK_RBRACE"
  | TK_RBRACK -> "TK_RBRACK"
  | TK_RSQBRACK -> "TK_RSQBRACK"
  | TK_SEMICOLON -> "TK_SEMICOLON"
  | TK_SLASH_EQ -> "TK_SLASH_EQ"
  | TK_SLASH -> "TK_SLASH"
  | TK_STAR_EQ -> "TK_STAR_EQ"
  | TK_STAR -> "TK_STAR"
  | TK_STRING_LITERAL _ -> "TK_STRING_LITERAL"
  | TK_TILDE -> "TK_TILDE"
  | TK_TYPEDEF_NAME _ -> "TK_TYPEDEF_NAME"
  | TK_WCHAR_CONSTANT _ -> "TK_WCHAR_CONSTANT"
  | TK_WSTRING_LITERAL _ -> "TK_WSTRING_LITERAL"
  | WC_DECL _ -> "WC_DECL"
  | WC_EXPR _ -> "WC_EXPR"
  | WC_TYPE _ -> "WC_TYPE"


let name_of_token tok =
  match tok with
  | KW_INTN bits
  | KW_DECIMALN bits
  | KW_FLOATN bits ->
      (token_name tok) ^ " " ^ (string_of_int bits)

  | TK_INCLUDE lit
  | WC_DECL lit
  | WC_EXPR lit
  | WC_TYPE lit
  | TK_CHAR_CONSTANT lit
  | TK_IDENTIFIER lit
  | TK_STRING_LITERAL lit
  | TK_TYPEDEF_NAME lit
  | TK_WCHAR_CONSTANT lit
  | TK_WSTRING_LITERAL lit ->
      (token_name tok) ^ " " ^ lit

  | TK_FLOATING_CONSTANT (lit, suffix)
  | TK_HEX_FLOATING_CONSTANT (lit, suffix)
  | TK_HEX_CONSTANT (lit, suffix)
  | TK_BIN_CONSTANT (lit, suffix)
  | TK_INTEGER_CONSTANT (lit, suffix)
  | TK_OCTAL_CONSTANT (lit, suffix) ->
      (token_name tok) ^ " " ^
      begin match suffix with
      | None -> lit
      | Some suffix -> lit ^ suffix
      end

  | _ -> token_name tok


let token_of_basic_type = function
  | BT_Default -> TK_IDENTIFIER ""
  | BT_Void -> KW_VOID
  | BT_Bool -> KW_BOOL
  | BT_Char -> KW_CHAR
  | BT_WCharT -> KW_WCHAR_T
  | BT_Int -> KW_INT
  | BT_Float -> KW_FLOAT
  | BT_Double -> KW_DOUBLE
  | BT_Signed -> KW_SIGNED
  | BT_Unsigned -> KW_UNSIGNED
  | BT_Short -> KW_SHORT
  | BT_Long -> KW_LONG
  | BT_VaList -> KW_VA_LIST
  | BT_IntN bits -> KW_INTN bits
  | BT_FloatN bits -> KW_FLOATN bits
  | BT_DecimalN bits -> KW_DECIMALN bits
  | BT_Ellipsis -> TK_ELLIPSIS
let string_of_basic_type bt = string_of_token (token_of_basic_type bt)


let token_of_storage_class = function
  | SC_Static -> KW_STATIC
  | SC_Thread -> KW_THREAD
  | SC_Extern -> KW_EXTERN
  | SC_Register -> KW_REGISTER
  | SC_Auto -> KW_AUTO
  | SC_Inline -> KW_INLINE
  | SC_Typedef -> KW_TYPEDEF
let string_of_storage_class sc = string_of_token (token_of_storage_class sc)


let token_of_type_qualifier = function
  | TQ_Const -> KW_CONST
  | TQ_Volatile -> KW_VOLATILE
  | TQ_Complex -> KW_COMPLEX
  | TQ_Restrict -> KW_RESTRICT
let string_of_type_qualifier tq = string_of_token (token_of_type_qualifier tq)


let token_of_sue_kind = function
  | SUE_Struct -> KW_STRUCT
  | SUE_Union -> KW_UNION
  | SUE_Enum -> KW_ENUM
let string_of_sue_kind kind = string_of_token (token_of_sue_kind kind)


let token_of_ternop = function
  (* Conditional expressions *)
  | OP_Conditional -> TK_QMARK, TK_COLON
let string_of_ternop op =
  let l, r = token_of_ternop op in
  string_of_token l, string_of_token r


let token_of_binop = function
  (* Assignment expressions *)
  | OP_Assign -> TK_EQUALS
  | OP_MultiplyAssign -> TK_STAR_EQ
  | OP_DivideAssign -> TK_SLASH_EQ
  | OP_ModuloAssign -> TK_PERCENT_EQ
  | OP_AddAssign -> TK_PLUS_EQ
  | OP_SubtractAssign -> TK_MINUS_EQ
  | OP_ShiftLeftAssign -> TK_LTLT_EQ
  | OP_ShiftRightAssign -> TK_GTGT_EQ
  | OP_BitwiseAndAssign -> TK_AND_EQ
  | OP_BitwiseXorAssign -> TK_CARET_EQ
  | OP_BitwiseOrAssign -> TK_PIPE_EQ

  (* Binary expressions *)
  | OP_Comma -> TK_COMMA
  | OP_Multiply -> TK_STAR
  | OP_Divide -> TK_SLASH
  | OP_Modulo -> TK_PERCENT
  | OP_Add -> TK_PLUS
  | OP_Subtract -> TK_MINUS
  | OP_ShiftLeft -> TK_LTLT
  | OP_ShiftRight -> TK_GTGT
  | OP_Less -> TK_LESS
  | OP_Greater -> TK_GREATER
  | OP_LessEqual -> TK_LESS_EQ
  | OP_GreaterEqual -> TK_GREATER_EQ
  | OP_Equal -> TK_EQEQ
  | OP_NotEqual -> TK_NE
  | OP_BitwiseAnd -> TK_AND
  | OP_BitwiseXor -> TK_CARET
  | OP_BitwiseOr -> TK_PIPE
  | OP_LogicalAnd -> TK_ANDAND
  | OP_LogicalOr -> TK_PIPEPIPE
  | OP_Ellipsis -> TK_ELLIPSIS
let string_of_binop op = string_of_token (token_of_binop op)


let token_of_unop = function
  (* Unary expressions *)
  | OP_PostIncrement
  | OP_PreIncrement -> TK_INC
  | OP_PostDecrement
  | OP_PreDecrement -> TK_DEC
  | OP_AddressOf -> TK_AND
  | OP_AddressOfLabel -> TK_ANDAND
  | OP_Dereference -> TK_STAR
  | OP_Identity -> TK_PLUS
  | OP_Negate -> TK_MINUS
  | OP_BitwiseNot -> TK_TILDE
  | OP_LogicalNot -> TK_EXMARK
  | OP_Imag -> KW_IMAG
  | OP_Real -> KW_REAL
let string_of_unop op = string_of_token (token_of_unop op)


let token_of_pseudoop = function
  | OP_Sizeof -> KW_SIZEOF
  | OP_Alignof -> KW_ALIGNOF

  | OP_Cast -> failwith "cast"

  | OP_MemberAccess -> TK_PERIOD
  | OP_PointerAccess -> TK_ARROW

  | OP_HighestPrecedence -> failwith "high prec pseudo-operator has no token"
  | OP_LowestPrecedence -> failwith "low prec pseudo-operator has no token"
  | OP_FunctionCall -> failwith "function call"
  | OP_ArrayAccess -> failwith "array access"
  | OP_ArrayLabelledInitialiser -> failwith "array labelled initialiser"
  | OP_DesignatedInitialiser -> failwith "designated initialiser"
let string_of_pseudoop op = string_of_token (token_of_pseudoop op)
