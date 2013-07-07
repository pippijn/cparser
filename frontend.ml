open CorePervasives

(************************************************
 * Run all passes on the translation unit's AST *
 ************************************************)

let run_passes tu =
  (* Verify parse *)
  assert (Verify_tree.verify tu);

  (* Prepare tree for subsequent passes *)
  let tu = tu
    |> Normalise.normalise_unit
    |> Assign_names.assign_names
  in

  (* Semantic checks. *)
  let tu =
    try
      (* Full semantic check including types and constant values. *)
      let tu = Typecheck.tcheck_unit tu in
      Csymtab.print ();

      tu
    with e ->
      if not Settings.w then
        raise e
      else
        tu
  in

  (* Information passes *)
  assert (Verify_tree.verify tu);
  assert (Warn_globals.process tu);

  tu
