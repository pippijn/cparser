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

  Rule (".PHONY", "upload", []);
  Rule ("upload", "$(Name).js index.html missing.js", [
    "chmod 644 $^";
    "scp $^ $'ra:public_html/files/up/parser/'";
  ]);
]
