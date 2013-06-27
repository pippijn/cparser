let printf = Printf.printf
let fprintf = Printf.fprintf
let sprintf = Printf.sprintf


exception ExitStatus of int

let exit_success = ExitStatus 0
let exit_failure = ExitStatus 1


let curfile = ref ""
let handle_return = function
  | Unix.WEXITED 0 ->
      ()
  | Unix.WEXITED status ->
      (*printf "  \"%s\", ();\n" !curfile;*)
      raise (ExitStatus status)
  | Unix.WSIGNALED signum ->
      printf "child was killed by signal %d\n" signum;
      raise exit_failure
  | Unix.WSTOPPED signum ->
      printf "child was stopped by signal %d\n" signum;
      raise exit_failure


(*************************************************
 * We can generate either an AST or a token list *
 *************************************************)

type result =
  | TokenList of C_tokens.token list
  | AST_Decl of Ast.declaration
  | AST_Expr of Ast.expression
  | AST_Stmt of Ast.statement
  | AST_Type of Ast.ctype
  | AST_Unit of Ast.declaration


(********************
 * C compiler flags *
 ********************)

(* Preprocessor flags *)
let cppflags =
  "-I/home/pippijn/code/git/lang/aldor/include " ^
  "-I/home/pippijn/code/git/lang/aldor/_build " ^
  "-I/home/pippijn/code/git/lang/aldor/_build/include " ^
  "-I/usr/local/include/csmith-2.1.0 " ^
  "-DHAVE_CONFIG_H " ^
  "-DPKGLIBEXECDIR='\"/usr/local/libexec/aldor-compiler\"'"



(*******************************
 * Processing strings or files *
 *******************************)

let process_string parse s =
  let open Lexing in
  let lexbuf = Lexing.from_string s in
  lexbuf.lex_curr_p <- {
    lexbuf.lex_curr_p with
    pos_fname = "<string>"
  };
  parse lexbuf


let process_file parse file =
  let pp = Platform.preprocessor ^ " " ^ cppflags ^ " " ^ Settings.cflags ^ " '" ^ file ^ "'" in
  (*printf "Preprocessing with %s\n" pp;*)
  let cin = Unix.open_process_in pp in
  let result = try
    let lexbuf = Lexing.from_channel cin in

    let open Lexing in
    lexbuf.lex_curr_p <- {
      lexbuf.lex_curr_p with
      pos_fname = if file = "-" then "<stdin>" else file
    };

    if Settings.verbose then begin
      Printf.fprintf stderr "processing %s\n" file;
      flush stderr
    end;
    flush stdout;
    parse lexbuf
  with
  | e ->
      handle_return (Unix.close_process_in cin);
      raise e
  in
  handle_return (Unix.close_process_in cin);
  result



(*********************************************
 * Exception wrapper to print exception data *
 *********************************************)

let string_of_position pos =
  let open Lexing in
  let column = pos.pos_cnum - pos.pos_bol + 1 in
  sprintf "%s:%d:%d:" pos.pos_fname pos.pos_lnum column


let print_source pos =
  try
    let open Lexing in
    let line =
      if Settings.string then
        !curfile
      else
        let file = pos.pos_fname in
        let fh = open_in file in
        let rec input_line_at fh = function
          | 1 -> input_line fh
          | n -> ignore (input_line fh); input_line_at fh (n - 1)
        in
        input_line_at fh pos.pos_lnum
    in

    let column = pos.pos_cnum - pos.pos_bol in
    printf "%s\n%*s%s\n" line column "" (Colour.green "^")

  with Sys_error _ -> ()


let print_error pos message =
  begin match ExString.nsplit message "\n" with
  | msg :: notes ->
      printf "%s %s %s\n" (Colour.white (string_of_position pos)) (Colour.red "error:") (Colour.white msg);
      List.iter (
        printf "%s %s %s\n" (Colour.white (string_of_position pos)) (Colour.grey "note:")
      ) notes;
  | [] -> failwith "no error message"
  end;
  print_source pos;
  print_string "1 error generated.\n"


let exnwrap fn lexbuf arg =
  if Settings.stacktrace then
    fn lexbuf arg
  else
    try
      fn lexbuf arg
    with
    | C_lexer.Lexing_error (pos, c) ->
        printf "%s unrecognised character '%s'\n" (Colour.white (string_of_position pos)) c;
        print_source pos;
        raise exit_failure
    | C_lexer.Eof ->
        print_string "attempted to read past end-of-file\n";
        raise exit_failure
    | C_parser.Error ->
        let pos = Lexing.lexeme_start_p lexbuf in
        let message = "could not recover from syntax errors" in
        print_error pos message;
        raise exit_failure
    | C_parser.StateError (token, state) ->
        if not Settings.merr then
          let pos = Lexing.lexeme_start_p lexbuf in
          let message = C_errors.message state token in
          print_error pos message
        else
          printf "(%d, %s)\n" state (Token.token_name token);
        raise exit_failure
    | Sys_error (msg) ->
        printf "%s: %s\n" Sys.argv.(0) msg;
        raise exit_failure


let astexnwrap fn arg =
  if Settings.stacktrace then
    fn arg
  else try
    fn arg
  with
  | Ast.ASTError (exn) ->
      let iso = function
        | None -> ""
        (*| Some section -> "ISO/IEC 9899:1999 (E) " ^ section ^ ": "*)
        | Some section -> "(ISO " ^ section ^ ") "
      in

      let open Printing in
      begin match exn with
      | Ast.Parse_error (msg, node) ->
          printf "Parse error: %s " msg;
          print_decl node;
          raise exit_failure
      | Ast.Declaration_error (msg, section, nodes) ->
          printf "Declaration error: %s%s\n" (iso section) msg;
          List.iter (fun node ->
            print_string "- ";
            print_decl node
          ) nodes;
          raise exit_failure
      | Ast.Expression_error (msg, section, nodes) ->
          printf "Expression error: %s%s\n" (iso section) msg;
          List.iter (fun node ->
            print_string "- ";
            print_expr node
          ) nodes;
          raise exit_failure
      | Ast.Statement_error (msg, section, nodes) ->
          printf "Statement error: %s%s\n" (iso section) msg;
          List.iter (fun node ->
            print_string "- ";
            print_stmt node
          ) nodes;
          raise exit_failure
      | Ast.Type_error (msg, section, nodes) ->
          printf "Type error: %s%s\n" (iso section) msg;
          List.iter (fun node ->
            print_string "- ";
            print_string (Codegen.code_of_type node);
            print_newline ()
          ) nodes;
          raise exit_failure
      | Ast.Unimplemented msg ->
          printf "Unimplemented: %s\n" msg;
          raise exit_failure
      end



(**************************************************
 * Completely tokenise a lexbuf and return a list *
 **************************************************)

let rec next_token next tokens =
  match next () with
  | C_tokens.EOF -> List.rev (C_tokens.EOF :: tokens)
  | tok -> next_token next (tok :: tokens)

let tokenise lexbuf =
  let next = C_lexer.token (C_lexer.state ()) in
  TokenList (next_token (fun () -> next lexbuf) [])



(******************************************************************
 * Call one of the C_parser interface functions to produce an AST *
 ******************************************************************)

let entry_point tokeniser lexbuf = let open Settings in function
  | EP_Decl -> AST_Decl (C_parser.parse_decl tokeniser lexbuf)
  | EP_Expr -> AST_Expr (C_parser.parse_expr tokeniser lexbuf)
  | EP_Stmt -> AST_Stmt (C_parser.parse_stmt tokeniser lexbuf)
  | EP_Type -> AST_Type (C_parser.parse_type tokeniser lexbuf)
  | EP_Unit -> AST_Unit (C_parser.parse_unit tokeniser lexbuf)

let parse entry lexbuf =
  let token = C_lexer.token (C_lexer.state ()) in

  let tokeniser =
    if Settings.tokens then
      Debug.token token
    else
      token
  in

  astexnwrap (exnwrap (entry_point tokeniser) lexbuf) entry



(*************************************************************************
 * Generate C code and optionally pass it to the compiter for validation *
 *************************************************************************)

let codegen fails tu =
  let code = Codegen.code_of_unit tu in
  if not fails then begin
    if Settings.compile then
      let cc = Platform.compiler ^ " " ^ Settings.cflags in
      let cout = Unix.open_process_out cc in
      output_string cout code;
      handle_return (Unix.close_process_out cout)
    else
      let out =
        if Settings.output = "-" then
          stdout
        else
          open_out Settings.output
      in
      output_string out code;
      if out != stdout then
        close_out out
  end



(*********************************************************
 * Make a token string for passing to menhir --interpret *
 *********************************************************)

let string_of_token_list tokens =
  List.fold_left
    (fun toks tok -> toks ^ " " ^ tok)
    "test:"
    (List.map Token.token_name tokens)

let print_tokens tokens =
  print_string ((string_of_token_list tokens) ^ "\n")



(*************************************************
 * Toplevel entry point, parses strings or files *
 *************************************************)

(*let units = ref []*)

let maybe_print codegen print node =
  if Settings.ast then
    print node;
  if Settings.codegen then
    printf "%s\n" (codegen node)


let compile file =
  curfile := file;

  let processor =
    if Settings.tokenise then
      tokenise
    else
      parse Settings.entry
  in

  let process =
    if Settings.string then
      process_string processor
    else
      process_file processor
  in

  let result = process file in

  match result with
  | AST_Unit (tu) ->
      if not (Settings.syntax_only || Settings.merr) then begin
        if Settings.ast then
          Printing.print_decl tu;

        let tu = astexnwrap Frontend.run_passes tu in

        if Settings.ast_pp then
          Printing.print_decl tu;

        (*units := tu :: !units;*)

        if Settings.codegen then begin
          astexnwrap (codegen (Xfail.compiler_fail file)) tu
        end;

        if Settings.validate then
          begin match process_string (parse Settings.EP_Unit) (Codegen.code_of_unit tu) with
          | AST_Unit (tu2) ->
              if Traits.clear_deep_decl tu2 <> Traits.clear_deep_decl tu then
                Ast.die (Ast.Parse_error ("reparsing yielded different tree", tu2))
          | _ -> raise ExPervasives.Impossible
          end
      end;

      Gc.compact ()

  | AST_Decl (decl) -> maybe_print Codegen.code_of_decl Printing.print_decl decl
  | AST_Expr (expr) -> maybe_print Codegen.code_of_expr Printing.print_expr expr
  | AST_Stmt (stmt) -> maybe_print Codegen.code_of_stmt Printing.print_stmt stmt
  | AST_Type (ty  ) -> maybe_print Codegen.code_of_type Printing.print_type ty

  | TokenList (tokens) ->
      print_tokens tokens
