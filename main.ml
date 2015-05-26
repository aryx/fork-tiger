(*s: main.ml *)
module S = Symbol
module F = Frame
module V = Environment
module T = Environment
(*s: standard basis *)
(*s: constant Main.base_tenv *)
let base_tenv =
(* name     type *)
[ "int",    T.INT
; "string", T.STRING
]
(*e: constant Main.base_tenv *)
(*x: standard basis *)
(*s: constant Main.base_venv *)
let base_venv = 
(* name        cc        args                    return *)
[ "print",     Some "C", [T.STRING],             T.UNIT
; "printi",    Some "C", [T.INT],                T.UNIT
; "flush",     Some "C", [],                     T.UNIT
; "getchar",   None,     [],                     T.STRING
; "ord",       Some "C", [T.STRING],             T.INT
; "chr",       None,     [T.INT],                T.STRING
; "size",      Some "C", [T.STRING],             T.INT
; "sizea",     Some "C", [T.ARRAY T.ANY],        T.INT
; "substring", None,     [T.STRING;T.INT;T.INT], T.STRING
; "concat",    None,     [T.STRING;T.STRING],    T.STRING
; "not",       Some "C", [T.INT],                T.INT
; "exit",      Some "C", [T.INT],                T.UNIT
]
(*e: constant Main.base_venv *)
(*x: standard basis *)
let imports =
  let internal = [ "alloc"
                 ; "call_gc"
                 ; "compare_str"
                 ; "bounds_check"
                 ; "set_handler"
                 ; "raise"
                 ; "unwind"
                 ; "spawn"
                 ]
  in
  List.map (fun(n,_,_,_) -> n) base_venv @ internal
(*e: standard basis *)
(*s: compiler driver *)
(*s: function Main.emit_function *)
let emit_function (frm,ex) =
  (*s: [[Main.emit_function()]] if dump expression tree *)
  if !Option.dump_ext  
  then Tree.print_exp ex;
  (*e: [[Main.emit_function()]] if dump expression tree *)
  let ltree = Canonical.linearize (Tree.EXP ex) in
  (*s: [[Main.emit_function()]] if dump linearized tree *)
  if !Option.dump_lext 
  then List.iter Tree.print_stm ltree;
  (*e: [[Main.emit_function()]] if dump linearized tree *)
  ltree |> Tree.find_temps |> List.iter (fun (x,p) -> 
    Frame.alloc_temp frm x p |> ignore
  );
  Frame.output_header frm;
  Codegen.emit ltree;
  Frame.output_footer frm
(*e: function Main.emit_function *)

(*s: function Main.compile *)
let compile ch =
  let base_env = Environment.new_env base_tenv base_venv in

  let lexbuf = Lexing.from_channel ch in
  let ast = Parser.program Lexer.token lexbuf in
  (*s: [[Main.compile()]] if dump AST option *)
  if !Option.dump_ast
  then Ast.print_tree ast;
  (*e: [[Main.compile()]] if dump AST option *)

  let exl = Semantics.translate base_env ast in

  Codegen.output_file_header imports;
  Frame.output_strings();

  List.iter emit_function exl
(*e: function Main.compile *)

(*s: function Main.main *)
let main () =
  try
    Option.parse_cmdline();
    compile !Option.inch
  with Error.Error ex ->
    Error.handle_exception ex
(*e: function Main.main *)

(*s: toplevel Main._ *)
let _ = 
  main ()
(*e: toplevel Main._ *)

(*e: compiler driver *)
(*e: main.ml *)
