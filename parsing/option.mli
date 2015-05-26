(*s: parsing/option.mli *)
(*s: option.mli *)
(*s: signature function Option.parse_cmdline *)
val parse_cmdline : unit -> unit
(*e: signature function Option.parse_cmdline *)

val dump_ast     : bool ref
val dump_ext     : bool ref
val dump_lext    : bool ref

val unwind    : bool ref
val file      : string ref
(*s: signature global Option.inch *)
val inch      : in_channel ref
(*e: signature global Option.inch *)
(*e: option.mli *)
(*e: parsing/option.mli *)
