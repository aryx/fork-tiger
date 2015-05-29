(*s: frontend/environment.mli *)
(*s: type Environment.vartype *)
type vartype =
  (* basic types *)
  | UNIT
  | INT
  | STRING

  (* aggregate types *)
  | ARRAY  of vartype
  | RECORD of (Ast.name * vartype) list

  (* other types *)
  (*s: [[Environment.vartype]] cases *)
  | NIL
  (*x: [[Environment.vartype]] cases *)
  | ANY
  (*x: [[Environment.vartype]] cases *)
  | NAME   of Ast.typename
  (*e: [[Environment.vartype]] cases *)
(*e: type Environment.vartype *)
(*s: type Environment.enventry *)
type 'ty enventry =
    VarEntry of (Frame.access * 'ty)
  | FunEntry of (Tree.label * string option * Frame.frame * 'ty list * 'ty)
(*e: type Environment.enventry *)
(*s: type Environment.t *)
type t = {
    (* type definitions *)
    tenv        : vartype Symbol.table;
    (* value definitions *)
    venv        : vartype enventry Symbol.table;
    (*s: [[Environment.t]] other fields *)
    frame       : Frame.frame;
    (*x: [[Environment.t]] other fields *)
    break_label : Tree.label option;
    (*x: [[Environment.t]] other fields *)
    exn_label   : Tree.label option;
    (*x: [[Environment.t]] other fields *)
    xenv        : int Symbol.table;
    (*e: [[Environment.t]] other fields *)
  }
(*e: type Environment.t *)

(*s: signature function Environment.new_env *)
val new_env : (string * vartype) list ->
              (string * string option * vartype list * vartype) list -> t
(*e: signature function Environment.new_env *)

(*s: environment.mli *)
val new_scope : t -> t
val frame     : t -> Frame.frame
val new_frame : t -> Symbol.symbol -> t
(*x: environment.mli *)
(*s: signature function Environment.lookup_type *)
val lookup_type  : t -> Ast.typename -> Ast.pos -> vartype
(*e: signature function Environment.lookup_type *)
(*s: signature function Environment.lookup_value *)
val lookup_value : t -> Symbol.symbol -> Ast.pos -> vartype enventry
(*e: signature function Environment.lookup_value *)
(*s: signature function Environment.lookup_exn *)
val lookup_exn   : t -> Ast.name -> Ast.pos -> int
(*e: signature function Environment.lookup_exn *)
(*x: environment.mli *)
(*s: signature function Environment.enter_type *)
val enter_type  : t -> Ast.typename -> vartype -> unit
(*e: signature function Environment.enter_type *)
(*s: signature function Environment.enter_fun *)
val enter_fun   : 
  t -> Ast.name -> string option -> vartype list -> vartype  -> t
(*e: signature function Environment.enter_fun *)
(*s: signature function Environment.enter_param *)
val enter_param : t -> Ast.name -> vartype -> bool -> unit
(*e: signature function Environment.enter_param *)
(*s: signature function Environment.enter_local *)
val enter_local : t -> Ast.name -> vartype -> bool -> Frame.access
(*e: signature function Environment.enter_local *)
(*s: signature function Environment.enter_exn *)
val enter_exn   : t -> Ast.name -> unit
(*e: signature function Environment.enter_exn *)
(*x: environment.mli *)
val break_label     : t -> Tree.label
val new_break_label : t -> t
(*x: environment.mli *)
val exn_label     : t -> Tree.label option
val new_exn_label : t -> t
(*e: environment.mli *)
(*e: frontend/environment.mli *)
