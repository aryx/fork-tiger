(*s: error.mli *)
(*s: types *)
type error =
    Internal_error of string
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string
  | Syntax_error
  | Type_error of string
  | Undefined_symbol of string
  | Duplicate_symbol of string
  | Illegal_break
type ex = error * int
exception Error of ex
(*e: types *)
val handle_exception : ex -> unit
(*x: error.mli *)
val warning   : int -> string -> unit
val type_err  : int -> string -> 'a
val undefined : int -> string -> 'a
val internal  :        string -> 'a
(*x: error.mli *)
val add_source_mapping : int -> int -> unit
val line_number : int -> int * int
(*e: error.mli *)
