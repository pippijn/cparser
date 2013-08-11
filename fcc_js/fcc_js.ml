type lang =
  | C
  | HindleyMilner


let lang =
  match Url.Current.get_fragment () with
  | "hm" -> HindleyMilner
  | "c"  -> C
  | _ -> HindleyMilner


let () =
  match lang with
  | C ->
      let module T = Js_interface.Make (Dynparse) in ()
  | HindleyMilner ->
      let module T = Js_interface.Make (Hm_dynparse) in ()
