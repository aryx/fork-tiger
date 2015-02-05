(*s: parsing/option.ml *)
(*s: option.ml *)
(*s: command line flags *)
let ast    = ref false
(*x: command line flags *)
let ast    = ref false
let ext    = ref false
let lext   = ref false
let unwind = ref false
let file   = ref ""
let inch   = ref stdin
(*e: command line flags *)
(*s: command line flags wrappers *)
let print_ast()  = !ast
let print_ext()  = !ext
let print_lext() = !lext
let use_unwind() = !unwind
let filename()   = !file
let channel()    = !inch
(*e: command line flags wrappers *)
(*x: option.ml *)
let set_input s =
  try file := s; inch := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("could not open file " ^ err))
(*x: option.ml *)
let rec usage() = Arg.usage options "Usage:";exit 0;
and options = [
  (*s: command line options *)
  "-dump_ast",      Arg.Set ast,    "\tprint Abstract Syntax Tree";
  (*x: command line options *)
  "-dump_ext",      Arg.Set ext,    "\tprint Expression Trees";
  (*x: command line options *)
  "-dump_lext",     Arg.Set lext,   "\tprint Linearized Expression Trees";
  (*x: command line options *)
  "-unwind",   Arg.Set unwind, "\tuse unwind continuations for exceptions";
  "-help",     Arg.Unit usage, "\tprint this message";
  (*e: command line options *)
]
let parse_cmdline() = Arg.parse options set_input "Usage:"
(*e: option.ml *)
(*e: parsing/option.ml *)
