module Html = Dom_html
module To = Html.CoerceTo


let (>>=) = Lwt.bind

let byId doc id =
  let node = doc##getElementById (Js.string id) in
  Js.Opt.get node (fun () -> assert false)


let coerce node ty =
  Js.coerce node ty (fun _ -> assert false)

let (>!) = coerce


module Make (P : Js_updater.Parse) = struct

  let onload _ =
    let doc = Html.document in
    let byId = byId doc in

    let input = byId "input" >! To.textarea in
    let error = byId "error" in
    let sexp = byId "sexp" in
    let output = byId "output" in
    let missing = byId "missing" in

    (byId "title")##innerHTML <- Js.string P.title;
    (byId "code_title")##innerHTML <- Js.string P.code_title;
    (byId "sexp_title")##innerHTML <- Js.string P.sexp_title;
    (byId "missing_title")##innerHTML <- Js.string P.missing_title;
    (byId "output_title")##innerHTML <- Js.string P.output_title;

    input##innerHTML <- Js.string P.example;

    let rec dyn_parse old_text n =
      let text = Js.to_string input##value in
      let n =
        if text <> old_text then begin
          Js.Opt.iter (error##firstChild) (Dom.removeChild error);

          P.parse 80 text
            (fun s -> sexp##innerHTML <- Js.string s)
            (fun s -> output##innerHTML <- Js.string s)
            (fun s -> error##innerHTML <- Js.string s)
            (fun codes ->
              missing##innerHTML <- Js.string (
                List.fold_left (fun msgs code ->
                  "\"" ^ (String.escaped code) ^ "\",\n" ^ msgs
                ) "" codes
              );
            );

          20
        end else
          max 0 (n - 1)
      in
      Lwt_js.sleep (if n = 0 then 0.5 else 0.1) >>= fun () ->
        dyn_parse text n
    in
    begin try
      ignore (dyn_parse "" 0)
    with e ->
      error##innerHTML <- Js.string ("Internal error: " ^ (Js.to_string (Json.output e)))
    end;
    Js._false


  let _ = Html.window##onload <- Html.handler onload

end
