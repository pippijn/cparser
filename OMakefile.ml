install Program ".DEFAULT" [
  (* Target *)
  Name		"cparse";

  (* Sources *)
  Modules [
    "Assign_names";
    "Ast";
    "Attributes";
    "Basic_type";
    "C_lexer";
    "C_parser";
    "C_tokens";
    "C_errors";
    "Codegen";
    "Colour";
    "Constant";
    "Const_eval";
    "Conversions";
    "Cstring";
    "Csymtab";
    "Debug";
    "Decls";
    "Dynparse";
    "Errors_missing";
    "ExList";
    "ExPervasives";
    "ExString";
    "ExUTF8";
    "Fcc";
    "Fcc1";
    "Frontend";
    "Initialiser";
    "Lexer_hack";
    "Location";
    "Normalise";
    "Operator";
    "Option";
    "Parse_util";
    "Platform";
    "Predicates";
    "Prelude";
    "Printing";
    "Sclass";
    "Settings";
    "Sue";
    "Symtab";
    "Tcheck";
    "Token";
    "Tqual";
    "Traits";
    "Type";
    "Typecheck";
    "Types";
    "Verify_tree";
    "Visit";
    "Warn_globals";
    "Xfail";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "merr";
    "sexplib.syntax";
    "ucs";
  ];

  (* Camlp4 *)
  Flags [
    "ExUTF8.ml",	"-syntax camlp4o";
    "ast.ml",		"-syntax camlp4o";
    "attributes.ml",	"-syntax camlp4o";
    "constant.ml",	"-syntax camlp4o";
    "tqual.ml",		"-syntax camlp4o";
    "platform.ml",	"-syntax camlp4o";
    "location.ml",	"-syntax camlp4o";
    "sclass.ml",	"-syntax camlp4o";
  ];

  Var ("RUNMERR", "cparse.native -merr -");
]
