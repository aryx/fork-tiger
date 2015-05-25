(*s: parsing/option.ml *)
(*s: option.ml *)
(*s: command line flags *)
let inch   = ref stdin
(*x: command line flags *)
let file   = ref ""
(*x: command line flags *)
let unwind = ref false
(*x: command line flags *)
let dump_ast    = ref false
(*x: command line flags *)
let dump_ext    = ref false
(*x: command line flags *)
let dump_lext   = ref false
(*e: command line flags *)
(*x: option.ml *)
(*s: function Option.set_input *)
let set_input s =
  try 
    file := s; 
    inch := open_in s
  with Sys_error err ->
    raise (Arg.Bad ("could not open file " ^ err))
(*e: function Option.set_input *)
(*x: option.ml *)
(*s: function Option.usage *)
let rec usage() = 
  Arg.usage options "Usage:";
  exit 0;
(*e: function Option.usage *)
(*s: constant Option.options *)
and options = [
  (*s: command line options *)
  "-unwind",   Arg.Set unwind, "\tuse unwind continuations for exceptions";
  (*x: command line options *)
  "-dump_ast",      Arg.Set dump_ast,    "\tprint Abstract Syntax Tree";
  (*x: command line options *)
  "-dump_ext",      Arg.Set dump_ext,    "\tprint Expression Trees";
  (*x: command line options *)
  "-dump_lext",     Arg.Set dump_lext,   "\tprint Linearized Expression Trees";
  (*e: command line options *)
  "-help",     Arg.Unit usage, "\tprint this message";
]
(*e: constant Option.options *)
(*s: function Option.parse_cmdline *)
let parse_cmdline() = 
  Arg.parse options set_input "Usage:"
(*e: function Option.parse_cmdline *)
(*e: option.ml *)
(*e: parsing/option.ml *)
