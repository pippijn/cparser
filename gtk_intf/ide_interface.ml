module Make (P : Ide_updater.Parse) = struct

  let font_name = "DejaVu Sans Mono 8"
  let lang_mime_type = P.lang_mime_type

  let language_manager = GSourceView2.source_language_manager ~default:true
  let lang = language_manager#guess_language ~content_type:lang_mime_type ()


  class editor () = object (self)
    inherit Ide_ui.window ()

    initializer
      let source_view parent =
        GSourceView2.source_view
          ~auto_indent:true
          ~insert_spaces_instead_of_tabs:true ~tab_width:2
          ~show_line_numbers:true
          ~smart_home_end:`ALWAYS
          ~packing:parent#add
          ()
      in

      let code = source_view codewindow in
      let output = source_view outputwindow in

      List.iter (fun w -> w#misc#modify_font_by_name font_name) [
        sexp;
        (code :> GText.view);
        (output :> GText.view);
      ];

      List.iter (fun w ->
        w#source_buffer#set_language lang;
        w#source_buffer#set_highlight_syntax true;
      ) [
        code;
        output;
      ];

      output#set_editable false;

      window#set_title P.title;

      let add_missing =
        let clear, append, set =
          let cols = new GTree.column_list in
          let code = cols#add Gobject.Data.string in
          let store = GTree.tree_store cols in

          let column = GTree.view_column ~title:P.missing_title ()
              ~renderer:(GTree.cell_renderer_text[], ["text", code]) in
          ignore (missing#append_column column);
          missing#set_model (Some (store :> GTree.model));

          store#clear, store#append, store#set ~column:code
        in

        fun codes ->
          clear ();
          List.iter (fun code -> set ~row:(append ()) code) codes
      in

      ignore (window#connect#destroy (fun _ -> GMain.quit ()));

      (*
      let ctrl = ref false in
      ignore (window#event#connect#key_press (fun event ->
        begin match GdkEvent.get_type event with
        | `KEY_PRESS ->
            begin match GdkEvent.Key.keyval event with
            | k -> Printf.printf "press %d\n" k; flush stdout
            end
        | `KEY_RELEASE ->
            begin match GdkEvent.Key.keyval event with
            | k -> Printf.printf "release %d\n" k; flush stdout
            end
        end;
        false
      ));
      *)

      ignore (code#source_buffer#connect#changed (fun () ->
        error#set_text "";
        P.parse 0 (code#buffer#get_text ())
          (fun s -> sexp#buffer#set_text s)
          (fun s -> output#buffer#set_text s)
          (fun s -> error#set_text s)
          add_missing
      ));

      code#buffer#set_text P.example
  end


  let main =
    ignore (GMain.init ());
    let window = new editor () in
    window#window#show ();
    GMain.main ()

end
