(*s: frontend/frame.mli *)
(*s: frame.mli *)
type frame
(*s: type Frame.access *)
type access =
    Stack of frame * int * bool
(*e: type Frame.access *)
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

val alloc_temp   : frame  -> Tree.label -> bool -> unit
val alloc_string : string -> Tree.label
(*x: frame.mli *)
(*s: signature function Frame.output_header *)
val output_header  : frame -> unit
(*e: signature function Frame.output_header *)
(*s: signature function Frame.output_footer *)
val output_footer  : frame -> unit
(*e: signature function Frame.output_footer *)

val output_strings : unit  -> unit
(*e: frame.mli *)
(*e: frontend/frame.mli *)
