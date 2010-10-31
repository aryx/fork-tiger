# 36 "environment.nw"
type 'a enventry =
    VarEntry of (Frame.access * 'a)
  | FunEntry of (Symbol.symbol * string option * Frame.frame * 'a list * 'a)
# 16 "environment.nw"
type 'a t = {
    tenv        : 'a Symbol.table;
    venv        : 'a enventry Symbol.table;
    xenv        : int Symbol.table;
    frame       : Frame.frame;
    break_label : Tree.label option;
    exn_label   : Tree.label option
  }
# 58 "environment.nw"
val new_env : (string * 'a) list ->
              (string * string option * 'a list * 'a) list -> 'a t
# 65 "environment.nw"
val new_scope : 'a t -> 'a t
val frame     : 'a t -> Frame.frame
val new_frame : 'a t -> Symbol.symbol -> 'a t
# 75 "environment.nw"
val lookup_type  : 'a t -> Symbol.symbol -> Ast.pos -> 'a
val lookup_value : 'a t -> Symbol.symbol -> Ast.pos -> 'a enventry
val lookup_exn   : 'a t -> Symbol.symbol -> Ast.pos -> int
# 86 "environment.nw"
val enter_type  : 'a t -> Symbol.symbol -> 'a -> unit
val enter_exn   : 'a t -> Symbol.symbol -> unit
val enter_fun   : 'a t -> Symbol.symbol -> string option ->
                  'a list -> 'a  -> 'a t
val enter_param : 'a t -> Symbol.symbol -> 'a -> bool -> unit
val enter_local : 'a t -> Symbol.symbol -> 'a -> bool -> Frame.access
# 97 "environment.nw"
val break_label     : 'a t -> Tree.label
val new_break_label : 'a t -> 'a t
# 104 "environment.nw"
val exn_label     : 'a t -> Tree.label option
val new_exn_label : 'a t -> 'a t
