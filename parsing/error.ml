(*s: parsing/error.ml *)
(*s: type Error.error *)
type error =
    Internal_error of string

  (* lexical errors *)
  | Illegal_character of char
  | Illegal_escape of string
  | Unterminated_comment
  | Unterminated_string

  (* syntaxic errors *)
  | Syntax_error

  (* semantic errors *)
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

(*s: type Error.sm *)
type sm = { mutable sm: (int * int) list }
(*e: type Error.sm *)
(*s: global Error.source_map *)
let source_map = { sm = [(0,0)] }
(*e: global Error.source_map *)
(*s: function Error.add_source_mapping *)
let add_source_mapping pos line =
  source_map.sm <- source_map.sm @ [(pos,line)]
(*e: function Error.add_source_mapping *)

(*s: function Error.line_number *)
let line_number pos =
  let rec line ln last_p = function
      (p,l) :: rest ->
        if p > pos then (l, pos - last_p)
        else line l p rest
    | [] -> (ln + 1, pos - last_p)
  in line 0 0 source_map.sm
(*e: function Error.line_number *)

(*s: function Error.err_msg *)
let err_msg prefix pos msg =
  let (line,col) = line_number pos in
  if line > 0 
  then
    Printf.fprintf stderr
      "%s:%d,%d: %s\n" !Option.file line col msg
  else
    Printf.fprintf stderr "%s: %s\n" prefix msg
(*e: function Error.err_msg *)

(*s: function Error.warning *)
let warning = err_msg "Warning"
(*e: function Error.warning *)

(*s: function Error.handle_exception *)
let handle_exception (ex,pos) =
  let msg = match ex with
    Internal_error s     -> "Compiler bug: " ^ s

  | Illegal_character ch -> Printf.sprintf "illegal character '%c'" ch
  | Illegal_escape str   -> Printf.sprintf "illegal escape %s" str
  | Unterminated_comment -> "unterminated comment"
  | Unterminated_string  -> "unterminated string"

  | Syntax_error         -> "syntax error"

  | Type_error str       -> str
  | Undefined_symbol str -> "undefined symbol: " ^ str
  | Duplicate_symbol str -> "duplcate definition of: " ^ str
  | Illegal_break        -> "Illegal break statement"
  in
  err_msg "Error" pos msg;
  exit 1
(*e: function Error.handle_exception *)

(*s: function Error.type_err *)
let type_err  pos msg = 
  raise(Error(Type_error msg, pos))
(*e: function Error.type_err *)
(*s: function Error.undefined *)
let undefined pos msg = 
  raise(Error(Undefined_symbol msg, pos))
(*e: function Error.undefined *)
(*s: function Error.internal *)
let internal      msg = 
  raise(Error(Internal_error msg, 0))
(*e: function Error.internal *)
(*e: parsing/error.ml *)
