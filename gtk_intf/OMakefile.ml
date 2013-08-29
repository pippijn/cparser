install Library ".DEFAULT" [
  (* Target *)
  Name		"gtk_intf";
  Description	"GTK+ interface to DynParse";
  Version	"0.1";

  (* Sources *)
  Modules [
    "Ide_interface";
    "Ide_ui";
    "Ide_updater";
  ];

  (* Library dependencies *)
  OCamlRequires [
    "lablgtk2.glade";
    "lablgtk2.sourceview2";
  ];
]
