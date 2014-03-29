open Ast


let print_var decl =
  let open Lexing in
  let loc = fst (decl.d_sloc) in
  let name = Decls.decl_name decl in
  Printf.printf "%s:%d: %s\n" loc.pos_fname loc.pos_lnum name


let warn_about = function
  | "_IO_2_1_stdin_"
  | "_IO_2_1_stdout_"
  | "_IO_2_1_stderr_"
  | "sys_errlist"
  | "sys_nerr"
  | "c_stdin"
  | "c_stdout"
  | "c_stderr"
  | "stdin"
  | "stdout"
  | "stderr" -> false
  | _ -> true


let rec collect_globals globs = function
  | { d = DeclaringList (decls) } :: tl ->
      collect_globals (collect_globals globs decls) tl
  (* SUE declarations *)
  | { d = TypedDecl (_, _, SUEType _, { d = EmptyDecl }, _, _) } :: tl
  (* Function declarations *)
  | { d = TypedDecl (_, _, FunctionType (_, _), _, _, _) } :: tl ->
      collect_globals globs tl
  | { d = TypedDecl (_, sclasses, _, decl, _, _) } :: tl ->
      let globs =
        if not (Sclass.is_typedef sclasses) && warn_about (Decls.decl_name decl) then
          decl :: globs
        else
          globs
      in
      collect_globals globs tl
  | { d = ToplevelAsm _ } :: tl
  | { d = FunctionDefinition _ } :: tl ->
      collect_globals globs tl
  | hd :: _ ->
      die (Parse_error ("invalid global declaration type", hd));
  | [] ->
      globs


let process = function
  | { d = TranslationUnit (decls) } ->
      let globs = collect_globals [] decls in
      if Settings.globals then begin
        print_string "--- global variables ---\n";
        List.iter print_var globs
      end;
      true
  | n -> true
