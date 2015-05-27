(*s: frontend/environment.ml *)
(*s: environment.ml *)
module E = Error
module S = Symbol
module F = Frame
module T = Tree
(*s: type Environment.vartype *)
type vartype =
  (* basic types *)
  | UNIT
  | INT
  | STRING

  (* aggregate types *)
  | ARRAY  of vartype
  | RECORD of (Ast.name * vartype) list

  (* other types *)
  (*s: [[Environment.vartype]] cases *)
  | NIL
  | ANY
  (*x: [[Environment.vartype]] cases *)
  | NAME   of Symbol.symbol
  (*e: [[Environment.vartype]] cases *)
(*e: type Environment.vartype *)
(*s: type Environment.enventry *)
type 'ty enventry =
    VarEntry of (Frame.access * 'ty)
  | FunEntry of (Symbol.symbol * string option * Frame.frame * 'ty list * 'ty)
(*e: type Environment.enventry *)
(*s: type Environment.t *)
type t = {
    (* type definitions *)
    tenv        : vartype Symbol.table;
    (* value definitions *)
    venv        : vartype enventry Symbol.table;
    (*s: [[Environment.t]] other fields *)
    frame       : Frame.frame;
    (*x: [[Environment.t]] other fields *)
    break_label : Tree.label option;
    (*x: [[Environment.t]] other fields *)
    exn_label   : Tree.label option;
    (*x: [[Environment.t]] other fields *)
    xenv        : int Symbol.table;
    (*e: [[Environment.t]] other fields *)
  }
(*e: type Environment.t *)
(*x: environment.ml *)
let new_env types funs =
  let mkfe (n,cc,a,r) = (n, FunEntry(S.symbol n,cc,F.base_frame,a,r))
  in { tenv        = Symbol.create types;
       venv        = Symbol.create (List.map mkfe funs);

       xenv        = Symbol.create [];
       frame       = F.new_frame (S.symbol "tiger_main") F.base_frame;
       break_label = None;
       exn_label   = None 
     }
(*x: environment.ml *)
let new_scope env = { env with
                      tenv = S.new_scope env.tenv;
                      venv = S.new_scope env.venv }
(*x: environment.ml *)
let frame env = env.frame
let new_frame env sym = { env with
                          tenv = S.new_scope env.tenv;
                          venv = S.new_scope env.venv;
                          frame = Frame.new_frame sym env.frame;
                          exn_label = None }
(*x: environment.ml *)
let lookup env sym pos =
  try S.look env sym
  with Not_found ->
    raise(E.Error(E.Undefined_symbol (S.name sym), pos))

let lookup_type  env = lookup env.tenv
let lookup_value env = lookup env.venv
let lookup_exn   env = lookup env.xenv
(*x: environment.ml *)
let enter tbl sym v =
  if S.mem tbl sym
  then raise(E.Error(E.Duplicate_symbol (S.name sym), 0))
  else S.enter tbl sym v

let enter_type env = enter env.tenv
(*x: environment.ml *)
let enter_exn env sym = enter env.xenv sym (S.uid sym)
(*x: environment.ml *)
let enter_fun env sym cc args result =
  let lbl = S.new_symbol (S.name sym) in
  let fenv = new_frame env lbl in
  let fe = FunEntry (lbl, cc, fenv.frame, args, result) in
  enter env.venv sym fe;
  fenv
(*x: environment.ml *)
let enter_param env sym typ ptr =
  let acc = F.alloc_param env.frame sym ptr in
  enter env.venv sym (VarEntry(acc,typ))
(*x: environment.ml *)
let enter_local env sym typ ptr =
  let acc = F.alloc_local env.frame sym ptr in
  enter env.venv sym (VarEntry(acc, typ));
  acc
(*x: environment.ml *)
let break_label env =
  match env.break_label with
    None      -> raise Not_found
  | Some(lbl) -> lbl

let new_break_label env =
  { env with break_label = Some(T.new_label "loop_end") }
(*x: environment.ml *)
let exn_label     env = env.exn_label
let new_exn_label env =
  { env with exn_label = Some(T.new_label "exn") }
(*e: environment.ml *)
(*e: frontend/environment.ml *)
