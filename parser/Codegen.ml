open Ast
open C_tokens

let iter = List.iter


type decl_context =
  | Left
  | Right


type node_kind =
  | Type of ctyp
  | Decl of decl
  | Stmt of stmt
  | Unit of decl
  | Expr of expr


type context = {
  mutable last : char;
  mutable pos : Lexing.position;
}



let nonempty = function
  | _::_ -> true
  | [] -> false


let output_basic_types p bts =
  iter (fun bt -> p (Printing.string_of_basic_type bt)) bts

let output_type_qualifiers p tqs =
  iter (fun tq -> p (Printing.string_of_type_qualifier tq)) (Tqual.list_of tqs)

let output_storage_classes p sclasses =
  iter (fun sc -> p (Printing.string_of_storage_class sc)) (Sclass.list_of sclasses)


let op_assign = Operator.BinaryOperator OP_Assign


let output_tq p = function
  | QualifiedType (tqs, _) ->
      output_type_qualifiers p tqs
  | _ -> ()


let output_sep output_node p sep lst =
  let hd_or_empty = function
    | hd::_ -> [hd]
    | [] -> []
  in
  let tl_or_empty = function
    | _::tl -> tl
    | [] -> []
  in
  iter output_node (hd_or_empty lst);
  iter (fun a -> p sep; output_node a) (tl_or_empty lst)


let opt = Option.may


(* The main pretty print function. This holds the two shared output functions
  
     p : string -> unit
       Output a string. Takes care that tokens, which need whitespace between
       them in order to be considered separate, will be separated.
  
     pl : bool -> position -> unit
       Set the output position. Prints "#line" directives or additional
       whitespace (spaces, newlines) as required. The boolean argument
       specifies whether it is the node start (true) or end position (false).
  
   This function holds the actual pretty printing functions for each AST node. *)
let output_toplevel (p : string -> unit) (pl : bool -> Lexing.position -> unit) =
  let t tok = p (Token.string_of_token tok) in

  let is_anon s =
    let anon_prefix = "__anon_" in
    let anon_len = String.length anon_prefix in
    String.length s > anon_len &&
    String.sub s 0 anon_len = anon_prefix
  in

  (* Toplevel nodes and declarations *)
  let rec output_node node =
    let pos = Traits.pos_of_decl node in
    let output_start_pos () =    pl true  (fst pos) in
    let output_end_pos   = lazy (pl false (snd pos)) in

    output_start_pos ();
    begin match node with
    | TranslationUnit decls ->
        iter output_node decls
    | PreprocessorDirective (_, dir) ->
        p "\n";
        p dir;
        p "\n"
    | DeclaringList (_, decls) ->
        output_decl_list decls; p ";"
    | TypedDecl _ as decl ->
        output_decl_list [decl]
    | IdentifierDeclarator (trs, id) ->
        opt output_attrs (Attributes.attribute_opt trs);
        if not (is_anon id) then
          p id
    | FunctionDefinition (_, decl, body) ->
        output_node decl;
        output_stmt body
    | WildcardDecl (_, wc) ->
        p wc
    | AsmSpecifier (_, template) ->
        t KW_ASM;
        p "("; iter p template; p ")"

    | Enumerator (_, name, None) ->
        p name
    | Enumerator (_, name, Some value) ->
        p name; p "="; output_expr Operator.highest value

    | ToplevelAsm (_, code) ->
        t KW_ASM;
        p "("; iter p code; p ")"; p ";";

    | EmptyDecl ->
        ()

    | StructDeclarator _ as n ->
        die (Declaration_error ("found struct declarator outside aggregate definition", None, [n]))

    | SyntaxError _ as n ->
        die (Declaration_error ("cannot output code with syntax errors", None, [n]))
    end;
    Lazy.force output_end_pos


  and output_attrs attrs =
    match attrs with
    | [] -> ()
    | attrs ->
        t KW_ATTRIBUTE;
        p "("; p "(";
        output_sep (fun (kind, exprs) ->
          p ("__" ^ kind ^ "__");
          begin match exprs with
          | [] -> ()
          | exprs -> p "("; output_sep (output_expr Operator.highest) p "," exprs; p ")"
          end;
        ) p "," attrs;
        p ")"; p ")"


  (* Statements *)
  and output_stmt node =
    let pos = node.s_sloc in
    let output_start_pos () =  pl true  (fst pos) in
    let output_end_pos = lazy (pl false (snd pos)) in

    let l sep = function
      | hd :: tl ->
          p hd;
          iter (fun l -> p sep; p l) tl
      | [] -> ()
    in

    let output_expr = output_expr Operator.highest in

    output_start_pos ();
    begin match node.s with
    | CompoundStatement (_, stmts) ->
        p "{";
        iter output_stmt stmts;
        Lazy.force output_end_pos;
        p "}"
    | IfStatement (cond, then_stmt, { s = EmptyStmt }) ->
        t KW_IF;
        p "("; output_expr cond; p ")";
        output_stmt then_stmt
    | IfStatement (cond, then_stmt, else_stmt) ->
        t KW_IF;
        p "("; output_expr cond; p ")";
        output_stmt then_stmt;
        t KW_ELSE;
        output_stmt else_stmt
    | DoWhileStatement (body, cond) ->
        t KW_DO;
        output_stmt body;
        t KW_WHILE;
        p "("; output_expr cond; p ")";
        p ";"
    | WhileStatement (cond, body) ->
        t KW_WHILE;
        p "("; output_expr cond; p ")";
        output_stmt body
    | ForStatement (init, cond, next, body) ->
        t KW_FOR;
        p "(";
        opt output_expr init;
        p ";";
        opt output_expr cond;
        p ";";
        opt output_expr next;
        p ")";
        output_stmt body
    | SwitchStatement (expr, cases) ->
        t KW_SWITCH;
        p "("; output_expr expr; p ")";
        p "{"; output_stmt cases; p "}"
    | CaseStatement (expr) ->
        t KW_CASE; output_expr expr; p ":"
    | DefaultStatement ->
        t KW_DEFAULT; p ":"
    | LabelledStatement (label, stmt) ->
        p label; p ":"; output_stmt stmt
    | LocalLabel (labels) ->
        t KW_LABEL; l "," labels; p ";"
    | GotoStatement (Identifier (_, label)) ->
        t KW_GOTO; p label; p ";"
    (* special case for computed goto *)
    | GotoStatement (UnaryExpression (_, OP_Dereference, expr)) ->
        t KW_GOTO; p "*"; output_expr expr; p ";"
    | GotoStatement (_) ->
        die (Statement_error ("invalid argument to `goto'", None, [node]))
    | BreakStatement ->
        t KW_BREAK; p ";"
    | ContinueStatement ->
        t KW_CONTINUE; p ";"
    | ReturnStatement expr ->
        t KW_RETURN; opt output_expr expr; p ";"
    | ExpressionStatement expr ->
        opt output_expr expr; p ";"
    | DeclarationStatement decl ->
        output_node decl
    | AsmStatement (volatile, code, in_regs, out_regs, clobber, labels) ->
        let output_asm_arg (AsmArgument (_, constr, expr)) =
          iter p constr;
          p "("; output_expr expr; p ")"
        in

        t KW_ASM;
        if volatile then
          t KW_VOLATILE;
        if nonempty labels then
          t KW_GOTO;
        p "(";
        iter p code;
        if nonempty in_regs || nonempty out_regs || nonempty clobber || nonempty labels then begin
          p ":"; output_sep output_asm_arg p "," in_regs;
          if nonempty out_regs || nonempty clobber || nonempty labels then begin
            p ":"; output_sep output_asm_arg p "," out_regs;
            if nonempty clobber || nonempty labels then begin
              p ":"; iter (iter p) clobber;
              if nonempty labels then begin
                p ":"; l "," labels
              end
            end
          end
        end;
        p ")";
        p ";"

    | EmptyStmt -> die (Statement_error ("output_stmt", None, [node]))
    end;
    Lazy.force output_end_pos


  (* Expressions *)
  and output_expr outer_op inner =
    let open Operator in

    let pos = Traits.pos_of_expr inner in
    let output_start_pos () =  pl true  (fst pos) in
    let output_end_pos = lazy (pl false (snd pos)) in

    let rec printer = function
      (* Expression type information is ignored in code generation *)
      | TypedExpression (_, _, expr) ->
          printer expr

      | TernaryExpression (_, op, cond, then_expr, else_expr) ->
          let ternop = TernaryOperator op in
          ternop, fun () ->
            let op1, op2 = Printing.string_of_ternop op in
            output_expr ternop cond;
            p op1;
            opt (output_expr ternop) then_expr;
            p op2;
            output_expr ternop else_expr

      | BinaryExpression (_, op, lhs, rhs) ->
          let binop = BinaryOperator op in
          binop, fun () ->
            output_expr binop lhs;
            p (Printing.string_of_binop op);
            output_expr binop rhs

      | UnaryExpression (_, op, expr) ->
          let unop = UnaryOperator op in
          unop, if Operator.is_prefix op then
            fun () ->
              p (Printing.string_of_unop op);
              output_expr unop expr
          else
            fun () ->
              output_expr unop expr;
              p (Printing.string_of_unop op)

      | ArrayLabelledInitialiser (_, index, init) ->
          let binop = PseudoOperator OP_ArrayLabelledInitialiser in
          binop, fun () ->
            p "["; output_expr Operator.lowest index; p "]";
            p "="; output_expr binop init

      | DesignatedInitialiser (_, designator, init) ->
          let binop = PseudoOperator OP_DesignatedInitialiser in
          binop, fun () ->
            output_expr binop designator;
            p "="; output_expr binop init

      | FunctionCall (_, callee, args) ->
          let binop = PseudoOperator OP_FunctionCall in
          binop, fun () ->
            output_expr binop callee;
            p "(";
            (* XXX: function arguments are assignment_expressions *)
            output_sep (output_expr op_assign) p "," args;
            p ")"

      | ArrayAccess (_, expr, index) ->
          let binop = PseudoOperator OP_ArrayAccess in
          binop, fun () ->
            output_expr binop expr;
            p "[";
            (* print index with lowest precedence, as it is already bracketed with [] *)
            output_expr Operator.lowest index;
            p "]"

      | PointerAccess (_, expr, member) ->
          let binop = PseudoOperator OP_PointerAccess in
          binop, fun () ->
            output_expr binop expr;
            p "->";
            p member

      | MemberAccess (_, expr, member) ->
          let binop = PseudoOperator OP_MemberAccess in
          binop, fun () ->
            output_expr binop expr;
            p ".";
            p member

      | AlignofExpr (_, expr) ->
          let unop = PseudoOperator OP_Alignof in
          unop, fun () ->
            t KW_ALIGNOF;
            output_expr unop expr
      | AlignofType (_, expr) ->
          let unop = PseudoOperator OP_Alignof in
          unop, fun () ->
            t KW_ALIGNOF;
            p "(";
            output_type expr;
            p ")"

      | SizeofExpr (_, expr) ->
          let unop = PseudoOperator OP_Sizeof in
          unop, fun () ->
            t KW_SIZEOF;
            output_expr unop expr
      | SizeofType (_, expr) ->
          let unop = PseudoOperator OP_Sizeof in
          unop, fun () ->
            t KW_SIZEOF;
            p "(";
            output_type expr;
            p ")"

      | Cast (_, ty, expr) ->
          let unop = PseudoOperator OP_Cast in
          unop, fun () ->
            p "(";
            output_type ty;
            p ")";
            output_expr unop expr

      | Offsetof (_, ty, member) ->
          let unop = Operator.highest in
          unop, fun () ->
            t KW_OFFSETOF;
            p "(";
            output_type ty;
            p ",";
            output_expr unop member;
            p ")"

      | TypesCompatibleP (_, ty1, ty2) ->
          let unop = Operator.highest in
          unop, fun () ->
            t KW_TYPES_COMPATIBLE_P;
            p "(";
            output_type ty1;
            p ",";
            output_type ty2;
            p ")"

      | VaArg (_, ap, ty) ->
          let unop = Operator.highest in
          unop, fun () ->
            t KW_VA_ARG;
            p "(";
            output_expr unop ap;
            p ",";
            output_type ty;
            p ")"

      | CompoundLiteral (_, ty, init) ->
          let binop = Operator.highest in
          binop, fun () ->
            p "("; output_type ty; p ")";
            output_expr binop init

      | BraceExpression (_, stmt) ->
          Operator.highest, fun () ->
            p "("; output_stmt stmt; p ")"

      | WildcardExpr (_, wc) ->
          Operator.highest, fun () -> p wc

      | CharLiteral (_, _, s)
      | Identifier (_, s) ->
          Operator.highest, fun () -> p s
      | IntegerLiteral (_, _, s, None)
      | FloatingLiteral (_, _, s, None) ->
          Operator.highest, fun () -> p s
      | IntegerLiteral (_, _, s, Some suffix)
      | FloatingLiteral (_, _, s, Some suffix) ->
          Operator.highest, fun () -> p (s ^ suffix)
      | StringLiteral (_, _, s) ->
          Operator.highest, fun () -> iter p s

      | InitialiserList (_, inits) ->
          let binop = Operator.highest in
          binop, fun () ->
            p "{";
            output_sep (output_expr binop) p "," inits;
            Lazy.force output_end_pos;
            p "}"

      | MemberDesignator (member) ->
          Operator.highest, fun () ->
            iter (fun m -> p "."; p m) member
    in

    let inner_op, out = printer inner in

    let need_bracket =
      let inner_prec = precedence_of_operator inner_op in
      let outer_prec = precedence_of_operator outer_op in

      let force_bracket = 
        match inner_op, outer_op with
        | BinaryOperator subop, BinaryOperator (OP_BitwiseXor | OP_BitwiseOr) ->
            begin match subop with
            | OP_Add | OP_Subtract | OP_BitwiseOr | OP_BitwiseXor -> true
            | _ -> false
            end
        | _ -> false
      in

      force_bracket || (
        inner_prec >= 0 &&
        outer_prec >= 0 &&
        inner_prec >= outer_prec &&

        (inner_op <> outer_op || not (Operator.is_associative inner_op))
      )
    in

    let output_bracket expr = p "("; expr (); p ")" in

    output_start_pos ();

    if need_bracket then
      output_bracket out
    else
      out ();

    Lazy.force output_end_pos


  (* Struct/union/enum members. Produces a function that outputs nothing for
   * an empty list and "{ members }" for a list of members. *)
  and output_sue (spos, epos) kind =
    let out =
      match kind with
      (* make sure enums are separated by "," and there is no comma
       * at the end of the last enumerator *)
      | SUE_Enum -> output_sep output_node p ","
      | _ -> iter output_node
    in function
    | [] -> ()
      | members ->
          pl true spos;
          p "{";
          out members;
          pl false epos;
          p "}"


  (* output_partial_type should be called twice: first with context==Left
   * to output type components appearing before the declared identifier,
   * then with context==Right to output type components appearing after.
   * sc is the storage class, which is emitted before all components in
   * the Left context. *)
  and output_partial_type =
    let unqual = function
      | QualifiedType (_, unqual) ->
          unqual
      | ty -> ty
    in

    let is_array_or_func_type = function
      | ArrayType _
      | FunctionType _ -> true
      | _ -> false
    in

    function
    | Left ->
        begin function
          | PartialBasicType bts ->
              output_basic_types p bts
          | BasicType bt ->
              output_basic_types p (Basic_type.to_list bt)
          | TypedefType id ->
              p id
          | SUEType (trs, kind, tag, members) ->
              p (Printing.string_of_sue_kind kind);
              opt output_attrs (Attributes.attribute_opt trs);
              if not (is_anon tag) then
                p tag;
              output_sue (Attributes.position trs) kind members
          | QualifiedType (tqs, base) ->
              output_partial_type Left base; output_type_qualifiers p tqs

          | PointerType (base) when is_array_or_func_type base ->
              output_partial_type Left (unqual base); p "("; output_tq p base; p "*"
          | PointerType (base) ->
              output_partial_type Left (unqual base);        output_tq p base; p "*"

          | FunctionType (retty, params) ->
              output_partial_type Left retty

          | ArrayType (_, base) ->
              output_partial_type Left base

          | TypeofExpr (expr) ->
              p (Token.string_of_token KW_TYPEOF); p "("; output_expr Operator.highest expr; p ")"

          | TypeofType (ty) ->
              p (Token.string_of_token KW_TYPEOF); p "("; output_type ty; p ")"

          | EmptyType -> ()

          | WildcardType _ as n -> die (Type_error ("output_partial_type Left", None, [n]))
        end
    | Right ->
        begin function
          | PartialBasicType _
          | BasicType _
          | TypedefType _
          | SUEType _ ->
              ()
          | QualifiedType (_, base) ->
              output_partial_type Right base

          | PointerType (base) when is_array_or_func_type base ->
              p ")"; output_partial_type Right base
          | PointerType (base) ->
                     output_partial_type Right base

          | FunctionType (retty, params) ->
              p "("; output_sep output_node p "," params; p ")"; output_partial_type Right retty

          | ArrayType (arity, base) ->
              p "["; opt (output_expr Operator.highest) arity; p "]"; output_partial_type Right base

          | TypeofExpr _
          | TypeofType _ ->
              ()

          | EmptyType -> ()

          | WildcardType _ as n -> die (Type_error ("output_partial_type Right", None, [n]))
        end


  (* Abstract declarators/type names *)
  and output_type ty =
    output_partial_type Left ty;
    output_partial_type Right ty


  (* Typed declarators *)
  and output_decl decl ty =
    output_partial_type Left ty;
    output_node decl;
    output_partial_type Right ty


  (* Output a declaring_list. Only the first declarator has the full type.
   * The following declarators miss the base type.
   *
   * E.g.
   *
   *   int a, b;
   *
   * in our tree is actually "int a, int b", so we take special care here.
   *)
  and output_decl_list =
    (* Output only base type, e.g. `int' in `int *[3]' or `int ()' *)
    let output_base_type ty = output_type (Types.base_type ty) in

    (* Output typed declarator without base type *)
    let output_decl_unbased decl ty = output_decl decl (Types.unbase_type ty) in

    (* Optionally output initialiser with "=" *)
    let opt_output_init = function
      | None -> ()
      | Some node -> p "="; output_expr op_assign node
    in

    let comma_separated out first tl =
      (* The first declarator has no ',' before it *)
      out first;
      iter (fun decl -> p ","; out decl) tl
    in

    function
    | TypedDecl (_, sclasses, ty, decl, asm, init) as first :: tl ->
        let out = function
          | TypedDecl (_, _, ty, decl, asm, init) ->
              output_decl_unbased decl ty;
              output_node asm;
              opt_output_init init
          | n -> die (Declaration_error ("output_decl_list", None, [n]))
        in
        output_storage_classes p sclasses;
        output_base_type ty;

        comma_separated out first tl

    | StructDeclarator (_, decl, bitfield) as first :: tl ->
        let out =
          let output_declr decl = output_decl_unbased (Decls.decl_decl decl) (Decls.decl_type decl) in
          function
          | StructDeclarator (_, decl, None) ->
              output_declr decl;
          | StructDeclarator (_, decl, Some bitfield) ->
              output_declr decl;
              p ":"; output_expr op_assign bitfield;
          | n -> die (Declaration_error ("output_decl_list", None, [n]))
        in
        output_base_type (Decls.decl_type decl);

        comma_separated out first tl

    | decl :: _ -> die (Declaration_error ("invalid node in DeclaringList", None, [decl]))
    | [] -> die (Declaration_error ("empty declaring list", None, []))


  (* Output just one declaration. Useful in error messages. *)
  and output_single_decl = function
    | TypedDecl (_, sclasses, ty, decl, asm, init) ->
        output_storage_classes p sclasses;
        output_type (Types.base_type ty);
        output_decl decl (Types.unbase_type ty)
    | StructDeclarator (_, decl, None) ->
        output_single_decl decl
    | StructDeclarator (_, decl, Some bitfield) ->
        output_single_decl decl;
        p ":"; output_expr op_assign bitfield;
    | FunctionDefinition _ as fdef ->
        output_node fdef
    | Enumerator _ as enum ->
        output_node enum
    | decl -> die (Declaration_error ("invalid declaration in output_single_decl", None, [decl]))


  in function
    | Expr expr -> output_expr Operator.highest expr
    | Decl decl -> output_single_decl decl
    | Stmt stmt -> output_stmt stmt
    | Type ty   -> output_type ty
    | Unit tu   -> output_node tu; p "\n"



let add_string buf ctx =
  let need_space = function
    | '^'
    | '='
    | '|'
    | ' '
    | '-'
    | ','
    | ';'
    | ':'
    | '!'
    | '?'
    | '/'
    | '.'
    | '"'
    | '('
    | ')'
    | '['
    | ']'
    | '{'
    | '}'
    | '*'
    | '\''
    | '&'
    | '%'
    | '+'
    | '>'
    | '<'
    | '\n' -> false
    | _ -> true
  in
  let want_space = function
    | '^'
    | '='
    | '|'
    | ':'
    | '?'
    | '/'
    | '%'
    | '+' -> true
    |  _  -> false
  in
  let isspace = function
    | ' '
    | '\n' -> true
    | c -> false
  in
  let need_space = function
    | '+', '+' -> true
    | '-', '-' -> true
    | ',',  b  -> not (isspace b)
    | ')', (')' | ';' | ',' | '[') -> false
    | ')',  b  -> true
    |  a , ('(' | '{' | '}') -> not (isspace a || a = '(')
    |  a ,  b  ->
        want_space a ||
        want_space b ||
        need_space a && need_space b
  in
  function
  | "" -> ()
  | s ->
      let open Lexing in
      let first = s.[0] in

      let add_space = function
        | true ->
            Buffer.add_char buf ' ';
            ctx.last <- ' ';
            1
        | false -> 0
      in

      (*Printf.printf "need_space %c %c\n" ctx.last first;*)
      let added = (add_space (need_space (ctx.last, first)))
                + (add_space (s = "..."))
      in

      ctx.last <- s.[String.length s - 1];
      Buffer.add_string buf s;

      let lnum = ctx.pos.pos_lnum + if s = "\n" then 1 else 0 in
      let cnum = ctx.pos.pos_cnum + String.length s + added in
      let bol = if s = "\n" then cnum else ctx.pos.pos_bol in
      ctx.pos <- {
        ctx.pos with
        pos_lnum = lnum;
        pos_bol = bol;
        pos_cnum = cnum;
      }


let set_source_position buf ctx is_start pos =
  let open Lexing in
  let inc_pos () =
    ctx.pos <- {
      ctx.pos with
      pos_cnum = ctx.pos.pos_cnum + 1;
    }
  in
  let inc_line () =
    ctx.pos <- {
      ctx.pos with
      pos_lnum = ctx.pos.pos_lnum + 1;
      pos_bol = pos.pos_bol;
      pos_cnum = pos.pos_bol + (if is_start then 0 else 1);
    }
  in

  if pos <> dummy_pos then begin
    if ctx.pos.pos_fname <> pos.pos_fname then
      let directive =
        Printf.sprintf "# %d \"%s\"\n" pos.pos_lnum pos.pos_fname
      in
      if Buffer.length buf > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf directive
    else if ctx.pos.pos_lnum > pos.pos_lnum then
      let directive =
        Printf.sprintf "# %d\n" pos.pos_lnum
      in
      if Buffer.length buf > 0 then Buffer.add_char buf '\n';
      Buffer.add_string buf directive
    else begin
      while ctx.pos.pos_lnum < pos.pos_lnum do
        Buffer.add_char buf '\n';
        ctx.last <- '\n';
        inc_line ()
      done;

      while ctx.pos.pos_cnum - ctx.pos.pos_bol < pos.pos_cnum - pos.pos_bol do
        Buffer.add_char buf ' ';
        ctx.last <- ' ';
        inc_pos ()
      done
    end;

    ctx.pos <- pos
  end


let output indent fn =
  let open Lexing in
  let buf = Buffer.create (512) in
  let ctx = {
    last = ' ';
    pos = {
      dummy_pos with 
      pos_lnum = 1;
      pos_cnum = 0;
    };
  } in
  let p = add_string buf ctx in
  let pl =
    if indent then
      set_source_position buf ctx
    else
      (fun is_start pos -> ())
  in
  fn p pl;
  Buffer.contents buf


let output_fn node p pl =
  output_toplevel p pl node


let code_of_decl decl = output false (output_fn (Decl decl))
let code_of_expr expr = output false (output_fn (Expr expr))
let code_of_stmt stmt = output false (output_fn (Stmt stmt))
let code_of_type ty   = output false (output_fn (Type ty  ))
let code_of_unit tu   = output true  (output_fn (Unit tu  ))
