# 29 "symbol.nw"
type symbol = string * int

let nextsym = ref 0
let hashtable = Hashtbl.create 128

let name = fst
let uid  = snd

let symbol name =
  try let s = Hashtbl.find hashtable name in (name,s)
  with Not_found ->
    incr nextsym; Hashtbl.add hashtable name !nextsym;
    (name, !nextsym)

let new_symbol s = symbol (Printf.sprintf "%s_%d" s !nextsym)
# 79 "symbol.nw"
type 'a table = {
    level  : int;
    tbl    : (symbol, 'a) Hashtbl.t;
    parent : 'a table option
  }

let enter env s v =
  if Hashtbl.mem env.tbl s
  then failwith "Compiler error: symbol table duplicate entry"
  else Hashtbl.add env.tbl s v
# 94 "symbol.nw"
let rec look env s =
  try Hashtbl.find env.tbl s
  with Not_found -> match env.parent with
    None -> raise Not_found
  | Some e -> look e s
# 103 "symbol.nw"
let mem env = Hashtbl.mem env.tbl
# 109 "symbol.nw"
let create l =
  let env = {
    level = 0;
    tbl = Hashtbl.create 20;
    parent = None
  } in
  List.iter (fun (key, data) -> enter env (symbol key) data) l;
  env

let new_scope env = { level = env.level + 1;
                      tbl = Hashtbl.create 20;
                      parent = Some env }
# 126 "symbol.nw"
let rec iter f env =
  Hashtbl.iter (f env.level) env.tbl;
  match env.parent with
    None -> ()
  | Some e -> iter f e

let rec fold f env init =
  let fold_fun = f env.level in
  let result = Hashtbl.fold (f env.level) env.tbl init in
  match env.parent with
    None -> result
  | Some e -> fold f e result
