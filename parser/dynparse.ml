exception Syntax_error of C_tokens.token * int * Lexing.position


let title = "C parser example"
let missing_title = "Code samples for missing error messages"
let code_title = "C code"
let sexp_title = "Parse tree"
let output_title = "Deparsed"

let example = "\
main ()
{
  return 0;
}"

let lang_mime_type = "text/x-c"


let parse_string text =
  let lexbuf = Lexing.from_string text in
  try
    C_parser.parse_unit (C_lexer.token (C_lexer.state ())) lexbuf
  with C_parser.StateError (token, state) ->
    let open Lexing in
    raise (Syntax_error (token, state, lexeme_start_p lexbuf))


let wrap icols wcols text =
  if wcols = 0 then
    text
  else
    let buf =
      let len = String.length text in
      Buffer.create (len + (len / wcols))
    in

    let rec indent = function
      | 0 -> ()
      | n -> Buffer.add_char buf ' '; indent (n - 1)
    in

    ignore (ExString.fold_left (fun col c ->
      Buffer.add_char buf c;
      if col > wcols && c = ' ' then begin
        Buffer.add_char buf '\n';
        indent icols;
        icols
      end else
        col + 1
    ) icols text);

    Buffer.contents buf


let string_of_pos pos =
  let open Lexing in
  let pos_fname = "input.c" in
  Printf.sprintf "%s:%d:%d" pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


let parse wcols code sexp output error missing =
  begin try

    Csymtab.reset ();
    let tu = parse_string code in

    sexp (Ast.Show_decl.show tu);
    output (Codegen.code_of_unit tu);

    let tu =
      if not Mach_int.is_big then
        Frontend.run_passes tu
      else
        tu
    in

    sexp (Ast.Show_decl.show tu);
    output (Codegen.code_of_unit tu);

  with
  | C_lexer.Lexing_error (pos, c) ->
      let err = Printf.sprintf "%s: unrecognised character '%s'" (string_of_pos pos) c in
      error err
  | Syntax_error (token, state, pos) ->
      let open Lexing in
      let msg = C_errors.message state token in
      let pos = string_of_pos pos in
      let err = Printf.sprintf "%s: %s" pos (wrap (String.length pos + 2) wcols msg) in

      (* Add to missing-list if no error message available *)
      if msg.[0] = '(' then
        Errors_missing.add state token code;

      error err
  | Ast.ASTError e ->
      let format_error pos_of code_of msg = function
        | [expr] ->
            let pos = string_of_pos (fst (pos_of expr)) in
            Printf.sprintf "%s: %s" pos (wrap (String.length pos) wcols msg)
        | exprs ->
            Printf.sprintf "%s%s" msg (List.fold_left (fun msg expr ->
              let pos = string_of_pos (fst (pos_of expr)) in
              Printf.sprintf "%s\n- %s: %s" msg pos (code_of expr)
            ) "" exprs)
      in

      begin
        let open Ast in
        let open Traits in
        let open Codegen in
        match e with
        | Expression_error (e, section, exprs) ->
            error (format_error (fun expr -> expr.e_sloc) code_of_expr e exprs)
        | Declaration_error (e, section, decls) ->
            error (format_error (fun decl -> decl.d_sloc) code_of_decl e decls)
        | Type_error (e, section, types) ->
            error (format_error (fun ctyp -> Location.dummy) code_of_type e types)
        | Statement_error (e, section, stmts) ->
            error (format_error (fun stmt -> stmt.s_sloc) code_of_stmt e stmts)

        | Parse_error (e, decl) ->
            error (format_error (fun decl -> decl.d_sloc) code_of_decl e [decl])

        | Unimplemented (e) ->
            error (e)
      end
  | exn ->
      error (
        "Internal error: " ^ Printexc.to_string exn ^ "\n"
        ^ Printexc.get_backtrace ()
      )
  end;

  let codes = Errors_missing.get () in
  missing codes
