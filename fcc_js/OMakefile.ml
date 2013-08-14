build Program ".DEFAULT" [
  (* Target *)
  Name		"fcc_js";

  (* Sources *)
  Modules [
    "Fcc_js";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cparser";
    "js_intf";
  ];

  (* Only byte-code *)
  Var ("OCAML_BYTE", "true");
  Var ("OCAML_NATIVE", "false");

  Rule ("upload", "$(Name).js", [
    "chmod 644 $^";
    "scp $^ $'ra:public_html/files/up/parser/'";
  ]);
]
