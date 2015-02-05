(*s: parsing/error.mli *)
(*s: error.mli *)
(*s: types(error.nw) *)
(*s: type Error.error *)
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
(*e: type Error.error *)
(*s: type Error.ex *)
type ex = error * int
(*e: type Error.ex *)
(*s: exception Error.Error *)
exception Error of ex
(*e: exception Error.Error *)
(*e: types(error.nw) *)

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
(*e: parsing/error.mli *)
