# 21 "frame.nw"
type frame
type access =
    Temp  of Tree.label
  | Stack of frame * int * bool
# 30 "frame.nw"
val fp    : frame -> Tree.exp
val name  : frame -> Tree.label
val level : frame -> int
# 42 "frame.nw"
val base_frame : frame
val new_frame  : Tree.label -> frame -> frame
# 48 "frame.nw"
val alloc_param  : frame  -> Tree.label -> bool -> access
val alloc_local  : frame  -> Tree.label -> bool -> access
val alloc_temp   : frame  -> Tree.label -> bool -> access
val alloc_string : string -> Tree.label
# 58 "frame.nw"
val output_header  : frame -> unit
val output_footer  : frame -> unit
val output_strings : unit  -> unit
