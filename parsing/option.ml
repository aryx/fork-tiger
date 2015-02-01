(*s: parsing/option.ml *)
(*s: option.ml *)
let ast    = ref false
let ext    = ref false
let lext   = ref false
let unwind = ref false
let file   = ref ""
let inch   = ref stdin

let print_ast()  = !ast
let print_ext()  = !ext
let print_lext() = !lext
let use_unwind() = !unwind
let filename()   = !file
let channel()    = !inch
(*x: option.ml *)
let set_input s =
  try file := s; inch := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("could not open file " ^ err))
(*x: option.ml *)
let rec usage() = Arg.usage options "Usage:";exit 0;
and options = [
  "-dump_ast",      Arg.Set ast,    "\tprint Abstract Syntax Tree";
  "-dump_ext",      Arg.Set ext,    "\tprint Expression Trees";
  "-dump_lext",     Arg.Set lext,   "\tprint Linearized Expression Trees";
  "-unwind",   Arg.Set unwind, "\tuse unwind continuations for exceptions";
  "-help",     Arg.Unit usage, "\tprint this message";
]
let parse_cmdline() = Arg.parse options set_input "Usage:"
(*e: option.ml *)
(*e: parsing/option.ml *)
