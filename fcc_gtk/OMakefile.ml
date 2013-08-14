install Program ".DEFAULT" [
  (* Target *)
  Name		"fcc_gtk";

  (* Sources *)
  Modules [
    "Fcc_ide";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cparser";
    "gtk_intf";
  ];
]
