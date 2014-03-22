let main =
  Printexc.record_backtrace true;
  Gc.set {
    (Gc.get ()) with
    Gc.minor_heap_size = 8 * 1024 * 1024;
  };

  try
    List.iter Fcc.compile Settings.files
  with
  | Fcc.ExitStatus status ->
      exit status
