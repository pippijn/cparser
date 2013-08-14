(** The [Symtab] module implements a symbol table for the various identifier
    name spaces defined by the C standard.

    @see "6.2.3" Name spaces of identifiers *)

(** If more than one declaration of a particular identifier is visible at any
    point in a translation unit, the syntactic context disambiguates uses that
    refer to different entities.  Thus, there are separate name spaces for
    various categories of identifiers, as defined in [symtype]. *)
type symtype =
  | Label
    (** {i label names} (disambiguated by the syntax of the label declaration and use) *)

  | Tag
    (** the {i tags} of structures, unions, and enumerations (disambiguated by
        following any of the keywords struct, union, or enum) *)

  | Member
    (** the {i members} of structures or unions; each structure or union has a
        separate name space for its members (disambiguated by the type of the
        expression used to access the member via the . or -> operator) *)

  | Ordinary
    (** all other identifiers, called {i ordinary identifiers} (declared in ordinary
        declarators or as enumeration constants). *)

(** A symbol is fully described by its scope identification, the identifier
    used to name the symbol, the name space and the AST node in which it was
    declared. *)
type 'a sym = {
  scope : string;
  name : string;
  namespace : symtype;
  decl : 'a;
}

exception Name_clash of (*name*)string
  (** Exception raised when a symbol is to be inserted and the named symbol
      already exists. The argument is the conflicting symbol name. *)

type 'a t
  (** Abstract type for the symbol table. *)

val create : unit -> 'a t
  (** Create a new symbol table. The table will have entered the "global" scope. *)

val reset : 'a t -> unit
  (** Reset the symbol table. It will be in the state as after [create]. *)


(** {6 Scope management} *)


val enter_scope : 'a t -> string -> unit
  (** [enter_scope symtab name] enters the scope named by [name]. The active
      scope will be stacked and can be restored by a call to [leave_scope].
      If the scope did not exist, yet, it will be created. Besides the temporary
      order created by this function, scopes are not ordered and can be entered
      in any order.

      The global scope is called "global" by convention. This does not mean that
      a client cannot use a different name for its global scope. *)

val leave_scope : 'a t -> unit
  (** Leave the most recently entered scope. The active scope will be the one
      that was active before the last call to [enter_scope].
      @raise Failure in an attempt a call to [leave_scope] if the current scope
      is the global scope. *)


(** {6 Insert and lookup} *)


val insert : 'a t -> string -> symtype -> 'a -> unit
  (** [insert symtab name kind decl] insert a symbol into the currently active
      scope. A subsequent call to [lookup name kind] will return [decl].  Leaving
      and re-entering the scope will make the entered symbol visible, again. It
      is an error to inserting more than one symbol with the same name and kind.
      Use [replace] to modify a symbol.
      @raise Name_clash if the symbol already exists in the current scope. *)

val insert_into_scope : 'a t -> 'a sym -> unit
  (** Insert a symbol into a named scope, regardless of which scope is currently
      active. This function does not affect the active scope.
      @raise Name_clash if the symbol already exists in the target scope. *)

val replace : 'a t -> string -> symtype -> 'a -> unit
  (** [replace symtab name kind decl] replaces the stored AST node of an
      existing symbol with [decl].
      @raise Not_found if the symbol did not exist, yet. *)

val replace_in_scope : 'a t -> 'a sym -> unit
  (** [replace_in_scope] does for [replace] what [insert_into_scope] does for
      [insert]. *)

val lookup : 'a t -> string -> symtype -> 'a
  (** [lookup symtab name kind] looks up a symbol by identifier and name space.
      The lookup considers all currently stacked scopes.
      @return the [decl] argument in a previous call to [insert name kind decl].
      @raise Not_found if there is no symbol with the given name and name space. *)

val lookup_in_scope : 'a t -> string -> string -> symtype -> 'a
  (** [lookup_in_scope symtab scope name symtype] finds a symbol in a given
      scope, regardless of what the current scope is. This lookup does not
      consider any scopes except the one named by [scope].
      @raise Not_found if the named scope does not exist or it does not contain
      a symbol with the given name and name space. *)


(** {6 Debugging} *)


val print : ('a -> string) -> 'a t -> unit
  (** Print the content of the symbol table to standard output. *)
