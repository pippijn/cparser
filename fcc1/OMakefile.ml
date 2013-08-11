install Program ".DEFAULT" [
  (* Target *)
  Name		"fcc1";

  (* Sources *)
  Modules [
    "Fcc1";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cparser";
  ];
]
