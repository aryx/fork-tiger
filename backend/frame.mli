(*s: frame.mli *)
type frame
type access =
    Temp  of Tree.label
  | Stack of frame * int * bool
(*x: frame.mli *)
val fp    : frame -> Tree.exp
val name  : frame -> Tree.label
val level : frame -> int
(*x: frame.mli *)
val base_frame : frame
val new_frame  : Tree.label -> frame -> frame
(*x: frame.mli *)
val alloc_param  : frame  -> Tree.label -> bool -> access
val alloc_local  : frame  -> Tree.label -> bool -> access
val alloc_temp   : frame  -> Tree.label -> bool -> access
val alloc_string : string -> Tree.label
(*x: frame.mli *)
val output_header  : frame -> unit
val output_footer  : frame -> unit
val output_strings : unit  -> unit
(*e: frame.mli *)
