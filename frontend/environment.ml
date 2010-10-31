# 111 "environment.nw"
module E = Error
module S = Symbol
module F = Frame
module T = Tree
# 36 "environment.nw"
type 'a enventry =
    VarEntry of (Frame.access * 'a)
  | FunEntry of (Symbol.symbol * string option * Frame.frame * 'a list * 'a)
# 16 "environment.nw"
type 'a t = {
    tenv        : 'a Symbol.table;
    venv        : 'a enventry Symbol.table;
    xenv        : int Symbol.table;
    frame       : Frame.frame;
    break_label : Tree.label option;
    exn_label   : Tree.label option
  }
# 121 "environment.nw"
let new_env types funs =
  let mkfe (n,cc,a,r) = (n, FunEntry(S.symbol n,cc,F.base_frame,a,r))
  in { tenv        = Symbol.create types;
       venv        = Symbol.create (List.map mkfe funs);
       xenv        = Symbol.create [];
       frame       = F.new_frame (S.symbol "tiger_main") F.base_frame;
       break_label = None;
       exn_label   = None }
# 132 "environment.nw"
let new_scope env = { env with
                      tenv = S.new_scope env.tenv;
                      venv = S.new_scope env.venv }
# 139 "environment.nw"
let frame env = env.frame
let new_frame env sym = { env with
                          tenv = S.new_scope env.tenv;
                          venv = S.new_scope env.venv;
                          frame = Frame.new_frame sym env.frame;
                          exn_label = None }
# 150 "environment.nw"
let lookup env sym pos =
  try S.look env sym
  with Not_found ->
    raise(E.Error(E.Undefined_symbol (S.name sym), pos))

let lookup_type  env = lookup env.tenv
let lookup_value env = lookup env.venv
let lookup_exn   env = lookup env.xenv
# 164 "environment.nw"
let enter tbl sym v =
  if S.mem tbl sym
  then raise(E.Error(E.Duplicate_symbol (S.name sym), 0))
  else S.enter tbl sym v

let enter_type env = enter env.tenv
# 175 "environment.nw"
let enter_exn env sym = enter env.xenv sym (S.uid sym)
# 182 "environment.nw"
let enter_fun env sym cc args result =
  let lbl = S.new_symbol (S.name sym) in
  let fenv = new_frame env lbl in
  let fe = FunEntry (lbl, cc, fenv.frame, args, result) in
  enter env.venv sym fe;
  fenv
# 192 "environment.nw"
let enter_param env sym typ ptr =
  let acc = F.alloc_param env.frame sym ptr in
  enter env.venv sym (VarEntry(acc,typ))
# 200 "environment.nw"
let enter_local env sym typ ptr =
  let acc = F.alloc_local env.frame sym ptr in
  enter env.venv sym (VarEntry(acc, typ));
  acc
# 210 "environment.nw"
let break_label env =
  match env.break_label with
    None      -> raise Not_found
  | Some(lbl) -> lbl

let new_break_label env =
  { env with break_label = Some(T.new_label "loop_end") }
# 223 "environment.nw"
let exn_label     env = env.exn_label
let new_exn_label env =
  { env with exn_label = Some(T.new_label "exn") }
