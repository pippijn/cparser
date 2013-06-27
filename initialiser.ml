open Ast
open Prelude


let rec is_link_time_constant = function
  | TypedExpression (ty, Constant.NonConst, expr) ->
      is_link_time_constant expr

  (* A compile-time constant value is also a link-time constant. *)
  | TypedExpression _ -> true

  (* Check for pointer arithmetic involving constant addresses. *)
  | BinaryExpression (_, (OP_Add | OP_Subtract), lhs, rhs) ->
      is_link_time_constant lhs && is_link_time_constant rhs

  | expr -> die (Expression_error ("invalid constant", None, [expr]))


let rec require_constant_initialiser = function
  | InitialiserList (_, inits) ->
      List.iter require_constant_initialiser inits

  | TypedExpression _ as expr ->
      if not (is_link_time_constant expr) then
        die (Expression_error ("initialiser must be constant", Some "6.7.8p4", [expr]))

  | init -> die (Expression_error ("invalid constant initialiser", None, [init]))


let rec is_initialiser = function
  | TypedExpression (_, _, expr) -> is_initialiser expr
  | InitialiserList _ -> true
  | IntegerLiteral _ -> false
  | init -> die (Expression_error ("is_initialiser", None, [init]))


let ensure_initialiser_expr init decl =
  if is_initialiser init then
    init
  else
    die (Declaration_error ("brace-enclosed initialiser required", None, [decl]))


let sue_match_init_list =
  let sue_match_field = function
    | field, init ->
        ignore (Conversions.assignment init (Decls.decl_type field));
        Printf.printf "- field:\n";
        Printing.print_decl field;
        Printf.printf "- init:\n";
        Printing.print_expr init;
  in

  let match_lists = function
    | init :: inits, field :: fields ->
        sue_match_field (field, init)
    | [], _ -> () (* Done. *)
    | inits, [] -> die (Expression_error ("excess initialisers for struct", None, inits))
  in
  
  fun fields decl initialiser toplevel ->
    Printf.printf "init:\n";
    Printing.print_expr initialiser;
    Printf.printf "sue:\n";
    List.iter Printing.print_decl fields;

    match initialiser with
    | InitialiserList (_, inits) ->
        match_lists (inits, fields)
    | init -> die (Expression_error ("sue_match_init_list", None, [init]))


(*
 * Recursively walk the initialisation list
 *   A) Verify that its shape matches the LHS
 *   B) Infer the size of unsized arrays
 *   C) Perform assignment conversions
 *
 * Decl is passed (from the root) to provide a location for error messages
 *)
let check_init_list decl dtype init toplevel =
  if Type.is_scalar dtype then
    match init with
    | TypedExpression (_, _, expr) ->
        ignore (Conversions.assignment init dtype)
    | init -> die (Expression_error ("is_initialiser", None, [init]))
  else if Type.is_array dtype then
    die (Type_error ("unimplemented array", None, [dtype]))
  else if Type.is_struct dtype then
    if not (is_initialiser init) then
      ignore (Conversions.assignment init dtype)
    else
      sue_match_init_list (Sue.member_decls dtype) decl (ensure_initialiser_expr init decl) toplevel
  else if Type.is_union dtype then
    die (Type_error ("unimplemented union", None, [dtype]))
  else
    die (Type_error ("expression cannot have initialiser", None, [dtype]))


(** Check declaration initialiser according to ISO 6.7.8.
    @param decl is the declaration being checked.
    @param dtype is the resolved declaration type.
    @param toplevel is a boolean indicating whether the declaration is at
                    global scope ([true]) or within a function ([false]). *)
let check_decl_init decl dtype toplevel =
  (* Deconstruct declaration. *)
  match decl with
  | TypedDecl (trs, sc, ty, untyped, asm, Some init) ->
      (* §3
       * The type of the entity to be initialized shall be an array of unknown size or an object type
       * that is not a variable length array type.
       *)
      if not (Type.is_object dtype || Type.is_unsized_array dtype) then
        die (Declaration_error ("must be object type or an incomplete array", Some "6.7.8p3", [decl]));

      (* §4
       * All the expressions in an initializer for an object that has static storage duration shall be
       * constant expressions or string literals.
       *
       * Implementation note: toplevel declarations have static storage duration.
       *)
      if toplevel || Sclass.is_static sc then
        require_constant_initialiser init;

      (* §5
       * If the declaration of an identifier has block scope, and the identifier has external or
       * internal linkage, the declaration shall have no initializer for the identifier.
       *)
      if Sclass.is_extern sc then
        if toplevel then
          die (Declaration_error ("inadvisible to initialise extern declaration", Some "6.7.8p5", [decl]))
        else
          die (Declaration_error ("cannot initialise extern declaration", Some "6.7.8p5", [decl]));

      (* Recursively walk the initialiser list. *)
      check_init_list decl dtype init true


  | decl -> die (Declaration_error ("unexpected declaration", None, [decl]))
