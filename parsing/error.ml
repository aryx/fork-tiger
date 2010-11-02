(*s: error.ml *)
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
(*x: error.ml *)
type sm = { mutable sm: (int * int) list }
let source_map = { sm = [(0,0)] }
let add_source_mapping pos line =
  source_map.sm <- source_map.sm @ [(pos,line)]
(*x: error.ml *)
let line_number pos =
  let rec line ln last_p = function
      (p,l) :: rest ->
        if p > pos then (l, pos - last_p)
        else line l p rest
    | [] -> (ln + 1, pos - last_p)
  in line 0 0 source_map.sm
(*x: error.ml *)
let err_msg prefix pos msg =
  let (line,col) = line_number pos in
  if line > 0 then
    Printf.fprintf stderr
      "%s:%d,%d: %s\n" (Option.filename()) line col msg
  else
    Printf.fprintf stderr "%s: %s\n" prefix msg

let warning = err_msg "Warning"
(*x: error.ml *)
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
(*x: error.ml *)
let type_err  pos msg = raise(Error(Type_error msg, pos))
let undefined pos msg = raise(Error(Undefined_symbol msg, pos))
let internal      msg = raise(Error(Internal_error msg, 0))
(*e: error.ml *)
