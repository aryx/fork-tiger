(*s: frame.ml *)
module S = Symbol
module T = Tree
module H = Hashtbl
(*x: frame.ml *)
type frame = { name           : T.label
             ; level          : int
             ; mutable size   : int
             ; mutable params : (T.label * bool) list
             ; mutable vars   : (T.label * bool) list
             ; mutable temps  : (T.label * bool) list
             }
type access =
    Temp  of T.label
  | Stack of frame * int * bool
(*x: frame.ml *)
let fp    frm = T.NAME (S.symbol "fp")
let name  frm = frm.name
let level frm = frm.level
(*x: frame.ml *)
let base_frame = { name   = (S.symbol "frame0")
                 ; level  = 0
                 ; params = [(S.symbol "pfp", true)]
                 ; size   = 1
                 ; vars   = []
                 ; temps  = []
                 }
let new_frame lbl parent = { base_frame with
                             name  = lbl;
                             level = parent.level + 1 }
(*x: frame.ml *)
let stack_alloc frm ptr =
  let v = Stack(frm, frm.size, ptr) in
  frm.size <- frm.size + 1; v

let alloc_param frm name ptr =
  frm.params <- frm.params @ [(name,ptr)];
  stack_alloc frm ptr

let alloc_local frm name ptr =
  frm.vars <- (name,ptr) :: frm.vars;
  stack_alloc frm ptr
(*x: frame.ml *)
let alloc_temp frm name ptr =
  frm.temps <- (name,ptr) :: frm.temps;
  Temp name
(*x: frame.ml *)
let strings = H.create 20
let alloc_string s =
  try H.find strings s
  with Not_found ->
    let lbl = T.new_label "gbl" in
    (H.add strings s lbl; lbl)
(*x: frame.ml *)
let pf           = Printf.printf
let spf          = Printf.sprintf
let join_map f l = String.concat "," (List.map f l)
let iter_ndx f   = let n   = ref(-1) in
                   let g x = incr n; f !n x in
                   List.iter g
(*x: frame.ml *)
let output_header frm =
  let param  (p,_) = spf "bits32 %s" (S.name p)
  and init n (p,_) = pf "  bits32[fp+%d] = %s;\n" (4*n) (S.name p)
  and temp   (t,_) = pf "  bits32 %s;\n" (S.name t)
  and name         = (S.name frm.name) in
  pf "%s(%s) {\n" name (join_map param frm.params);
  pf " span 1 %s_gc_data {\n" name;
  pf "  stackdata { align 4; fp : bits32[%d]; }\n" frm.size;
  iter_ndx  init frm.params;
  List.iter temp frm.temps
(*x: frame.ml *)
let output_footer frm =
  let var_data vl =
    let int_of_var (_,p) = if p then 1 else 0 in
    let data = List.length vl :: List.map int_of_var vl in
    join_map string_of_int data
  in
  pf "}}\n";
  pf "section \"data\" {\n";
  pf " %s_gc_data:\n" (S.name frm.name);
  pf "   bits32[] { %s };\n" (var_data (frm.params @ List.rev frm.vars));
  pf "   bits32[] { %s };\n" (var_data (frm.params @ frm.temps));
  pf "}\n\n"
(*x: frame.ml *)
(* output *)
let output_strings() =
  let print_string str lbl =
    let len = String.length str
    and str = String.escaped str in
    pf " %s: bits32 { %d }; bits8[] \"%s\\000\";\n"
       (S.name lbl) len str
  in
  pf "section \"data\" { align 4;\n";
  H.iter print_string strings;
  pf "}\n\n"
(*e: frame.ml *)
