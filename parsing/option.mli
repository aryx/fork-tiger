(*s: parsing/option.mli *)
(*s: option.mli *)
val parse_cmdline : unit -> unit

val print_ast     : unit -> bool
val print_ext     : unit -> bool
val print_lext    : unit -> bool

val use_unwind    : unit -> bool
val filename      : unit -> string
val channel       : unit -> in_channel
(*e: option.mli *)
(*e: parsing/option.mli *)
