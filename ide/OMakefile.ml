install Program ".DEFAULT" [
  (* Target *)
  Name		"ide";

  (* Sources *)
  Modules [
    "Ide";
    "Ide_interface";
    "Ide_ui";
    "Ide_updater";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "cparser";
    "hm";
    "lablgtk2.glade";
    "lablgtksourceview2.gtksourceview2";
  ];
]
