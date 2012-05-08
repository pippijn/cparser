type entry_point =
  | EP_Decl
  | EP_Expr
  | EP_Stmt
  | EP_Type
  | EP_Unit

type cmd_options = {
  ast : bool ref;
  ast_pp : bool ref;
  cflags : string ref;
  codegen : bool ref;
  compile : bool ref;
  entry : entry_point ref;
  files : string list ref;
  globals : bool ref;
  merr : bool ref;
  output : string ref;
  stacktrace : bool ref;
  string : bool ref;
  syntax_only : bool ref;
  tokenise : bool ref;
  tokens : bool ref;
  validate : bool ref;
  verbose : bool ref;
  w : bool ref;
}

let opts = {
  ast_pp = ref false;
  ast = ref false;
  cflags = ref "-w";
  codegen = ref false;
  compile = ref false;
  entry = ref EP_Unit;
  files = ref [];
  globals = ref false;
  merr = ref false;
  output = ref "-";
  stacktrace = ref false;
  string = ref false;
  syntax_only = ref false;
  tokenise = ref false;
  tokens = ref false;
  validate = ref false;
  verbose = ref false;
  w = ref false;
}

let add_file file =
  if Xfail.preprocessor_fail file then
    ()
  else
    opts.files := file :: !(opts.files)

let cmd =
  let parse_cflags = function
    | "" -> opts.cflags :=     " -m64 " ^ !(opts.cflags)
    | s  -> opts.cflags := s ^ " -m32 " ^ !(opts.cflags)
  in
Arg.align Arg.([
  ("-tokens",		Set opts.tokens,			" Print all tokens to stdout as they are read");
  ("-ast",		Set opts.ast,				" Print AST as S-expressions after parsing");
  ("-ast-pp",		Set opts.ast_pp,			" Print AST as S-expressions after transformations");
  ("-codegen",		Set opts.codegen,			" Generate C code");
  ("-compile",		Set opts.compile,			" Pass generated C code to the system C compiler for validation");
  ("-nocatch",		Set opts.stacktrace,			" Print stack trace on parser exceptions");
  ("-verbose",		Set opts.verbose,			" Print each file name as it is processed");
  ("-validate",		Set opts.validate,			" Parse back generated C code and compare tree");
  ("-Wglobal",		Set opts.globals,			" Print a list of global variables");
  ("-fsyntax-only",	Set opts.syntax_only,			" Parsing only, no other passes or checks");
  ("-w",		Set opts.w,				" Ignore errors from semantic checks");

  ("-tokenise",		Set opts.tokenise,			" Only tokenise and print token names");
  ("-string",		Set opts.string,			" Parse command line arguments as code");
  ("-merr",		Set opts.merr,				" Print syntax errors in terms of token/state pairs");

  ("-decl",		Unit (fun () -> opts.entry := EP_Decl),	" Parse as declaration");
  ("-expr",		Unit (fun () -> opts.entry := EP_Expr),	" Parse as expression");
  ("-stmt",		Unit (fun () -> opts.entry := EP_Stmt),	" Parse as statement");
  ("-type",		Unit (fun () -> opts.entry := EP_Type),	" Parse as type");
  ("-unit",		Unit (fun () -> opts.entry := EP_Unit),	" Parse as translation unit");

  ("-cflags",		String parse_cflags,			"<flags>  Pass extra flags to the C compiler");

  ("-v",		Unit (Platform.print_version),		" Output version and target platform");

  ("-o",		Set_string opts.output,			"<file>  Write code to file");
  ("-",			Unit (fun () -> add_file "-"),		" Read code from stdin");

  ("--",		Rest add_file,				" End of option parameters");
])


let () = Arg.parse cmd add_file "Usage: fcc <options> <files>\nOptions are:"

let ast = !(opts.ast)
let ast_pp = !(opts.ast_pp)
let cflags = !(opts.cflags)
let codegen = !(opts.codegen)
let compile = !(opts.compile)
let entry = !(opts.entry)
let files = List.rev !(opts.files)
let globals = !(opts.globals)
let merr = !(opts.merr)
let output = !(opts.output)
let stacktrace = !(opts.stacktrace)
let string = !(opts.string)
let syntax_only = !(opts.syntax_only)
let tokenise = !(opts.tokenise)
let tokens = !(opts.tokens)
let validate = !(opts.validate)
let verbose = !(opts.verbose)
let w = !(opts.w)
