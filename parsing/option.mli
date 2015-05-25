(*s: parsing/option.mli *)
(*s: option.mli *)
val parse_cmdline : unit -> unit

val dump_ast     : bool ref
val dump_ext     : bool ref
val dump_lext    : bool ref

val unwind    : bool ref
val file      : string ref
val inch      : in_channel ref
(*e: option.mli *)
(*e: parsing/option.mli *)
