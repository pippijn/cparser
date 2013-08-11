install Program ".DEFAULT" [
  (* Target *)
  Name		"fcc_js";

  (* Sources *)
  Modules [
    "Fcc_js";
    "Js_interface";
    "Js_updater";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cparser";
    "hm";
    "js_of_ocaml";
    "js_of_ocaml.syntax";
    "lwt";
  ];

  (* Camlp4 *)
  Flags [
    "js_interface.ml",	"-syntax camlp4o";
  ];

  (* Only byte-code *)
  Var ("OCAML_BYTE", "true");
  Var ("OCAML_NATIVE", "false");

  Rule (".DEFAULT", "$(Name).js", []);
]
