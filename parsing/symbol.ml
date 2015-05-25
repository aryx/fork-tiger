(*s: parsing/symbol.ml *)
(*s: symbol.ml *)
(*s: type Symbol.symbol *)
type symbol = string * int
(*e: type Symbol.symbol *)

(*s: global Symbol.nextsym *)
let nextsym = ref 0
(*e: global Symbol.nextsym *)

(*s: global Symbol.hashtable *)
let (hashtable: (string, int) Hashtbl.t) = Hashtbl.create 128
(*e: global Symbol.hashtable *)

(*s: function Symbol.name *)
let name = fst
(*e: function Symbol.name *)
(*s: function Symbol.uid *)
let uid  = snd
(*e: function Symbol.uid *)

(*s: function Symbol.symbol *)
let symbol name =
  try 
    let uid = Hashtbl.find hashtable name in 
    (name,uid)
  with Not_found ->
    incr nextsym; 
    Hashtbl.add hashtable name !nextsym;
    (name, !nextsym)
(*e: function Symbol.symbol *)

(*s: function Symbol.new_symbol *)
let new_symbol s = 
  symbol (Printf.sprintf "%s_%d" s !nextsym)
(*e: function Symbol.new_symbol *)
(*x: symbol.ml *)
(*s: type Symbol.table *)
type 'a table = {
    level  : int;
    tbl    : (symbol, 'a) Hashtbl.t;
    parent : 'a table option
  }
(*e: type Symbol.table *)

(*s: function Symbol.enter *)
let enter env s v =
  if Hashtbl.mem env.tbl s
  then failwith "Compiler error: symbol table duplicate entry"
  else Hashtbl.add env.tbl s v
(*e: function Symbol.enter *)
(*x: symbol.ml *)
(*s: function Symbol.look *)
let rec look env s =
  try Hashtbl.find env.tbl s
  with Not_found -> 
    (match env.parent with
    | None -> raise Not_found
    | Some e -> look e s
    )
(*e: function Symbol.look *)
(*x: symbol.ml *)
(*s: function Symbol.mem *)
let mem env = Hashtbl.mem env.tbl
(*e: function Symbol.mem *)
(*x: symbol.ml *)
(*s: constructor Symbol.create *)
let create l =
  let env = {
    level = 0;
    tbl = Hashtbl.create 20;
    parent = None
  } in
  l |> List.iter (fun (key, data) -> enter env (symbol key) data);
  env
(*e: constructor Symbol.create *)

(*s: constructor Symbol.new_scope *)
let new_scope env = { level = env.level + 1;
                      tbl = Hashtbl.create 20;
                      parent = Some env }
(*e: constructor Symbol.new_scope *)
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
(*e: parsing/symbol.ml *)
