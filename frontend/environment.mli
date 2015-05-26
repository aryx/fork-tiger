(*s: frontend/environment.mli *)
(*s: environment.mli *)
(*s: type Environment.vartype *)
type vartype =
  (* basic types *)
  | UNIT
  | INT
  | STRING

  (* aggregate types *)
  | ARRAY  of vartype
  | RECORD of (Symbol.symbol * vartype) list

  (* other types *)
  (*s: [[Environment.vartype]] cases *)
  | NIL
  | ANY
  (*x: [[Environment.vartype]] cases *)
  | NAME   of Symbol.symbol
  (*e: [[Environment.vartype]] cases *)
(*e: type Environment.vartype *)
(*s: type Environment.enventry *)
type 'ty enventry =
    VarEntry of (Frame.access * 'ty)
  | FunEntry of (Symbol.symbol * string option * Frame.frame * 'ty list * 'ty)
(*e: type Environment.enventry *)
(*s: type Environment.t *)
type t = {
    (* type definitions *)
    tenv        : vartype Symbol.table;
    (* value definitions *)
    venv        : vartype enventry Symbol.table;
    (*s: [[Environment.t]] other fields *)
    xenv        : int Symbol.table;

    frame       : Frame.frame;

    break_label : Tree.label option;
    (*x: [[Environment.t]] other fields *)
    exn_label   : Tree.label option
    (*e: [[Environment.t]] other fields *)
  }
(*e: type Environment.t *)
(*x: environment.mli *)
(*s: signature function Environment.new_env *)
val new_env : (string * vartype) list ->
              (string * string option * vartype list * vartype) list -> t
(*e: signature function Environment.new_env *)
(*x: environment.mli *)
val new_scope : t -> t
val frame     : t -> Frame.frame
val new_frame : t -> Symbol.symbol -> t
(*x: environment.mli *)
val lookup_type  : t -> Symbol.symbol -> Ast.pos -> vartype
val lookup_value : t -> Symbol.symbol -> Ast.pos -> vartype enventry
val lookup_exn   : t -> Symbol.symbol -> Ast.pos -> int
(*x: environment.mli *)
val enter_type  : t -> Symbol.symbol -> vartype -> unit
val enter_exn   : t -> Symbol.symbol -> unit
val enter_fun   : t -> Symbol.symbol -> string option ->
                  vartype list -> vartype  -> t
val enter_param : t -> Symbol.symbol -> vartype -> bool -> unit
val enter_local : t -> Symbol.symbol -> vartype -> bool -> Frame.access
(*x: environment.mli *)
val break_label     : t -> Tree.label
val new_break_label : t -> t
(*x: environment.mli *)
val exn_label     : t -> Tree.label option
val new_exn_label : t -> t
(*e: environment.mli *)
(*e: frontend/environment.mli *)
