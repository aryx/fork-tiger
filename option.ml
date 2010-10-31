# 29 "option.nw"
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
# 46 "option.nw"
let set_input s =
  try file := s; inch := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("could not open file " ^ err))
# 56 "option.nw"
let rec usage() = Arg.usage options "Usage:";exit 0;
and options = [
  "-ast",      Arg.Set ast,    "\t\tprint Abstract Syntax Tree";
  "-ext",      Arg.Set ext,    "\t\tprint Expression Trees";
  "-lext",     Arg.Set lext,   "\tprint Linearized Expression Trees";
  "-unwind",   Arg.Set unwind, "\tuse unwind continuations for exceptions";
  "-help",     Arg.Unit usage, "\tprint this message";
]
let parse_cmdline() = Arg.parse options set_input "Usage:"
