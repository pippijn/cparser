let cmd =
  let open Arg in
  align [
    ("-c",	Unit (fun () -> let module T = Ide_interface.Make (Dynparse) in ()), " start the C IDE");
    ("-hm",	Unit (fun () -> let module T = Ide_interface.Make (Hm_dynparse) in ()), " start the Hindley-Milner IDE");
  ]

let () = Arg.parse cmd (fun _ -> ()) "Usage: ide [-c|-hm]"
