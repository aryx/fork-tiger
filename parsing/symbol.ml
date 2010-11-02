(*s: symbol.ml *)
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
(*x: symbol.ml *)
type 'a table = {
    level  : int;
    tbl    : (symbol, 'a) Hashtbl.t;
    parent : 'a table option
  }

let enter env s v =
  if Hashtbl.mem env.tbl s
  then failwith "Compiler error: symbol table duplicate entry"
  else Hashtbl.add env.tbl s v
(*x: symbol.ml *)
let rec look env s =
  try Hashtbl.find env.tbl s
  with Not_found -> match env.parent with
    None -> raise Not_found
  | Some e -> look e s
(*x: symbol.ml *)
let mem env = Hashtbl.mem env.tbl
(*x: symbol.ml *)
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
(*x: symbol.ml *)
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
(*e: symbol.ml *)
