install Library ".DEFAULT" [
  (* Target *)
  Name		"js_intf";
  Description	"JavaScript interface to DynParse";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Js_interface";
    "Js_updater";
  ];

  (* Library dependencies *)
  OCamlRequires [
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

  Rule ("upload", "missing.js", [
    "chmod 644 $^";
    "scp $^ $'ra:public_html/files/up/parser/'";
  ]);
]
