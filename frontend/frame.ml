(*s: frontend/frame.ml *)
(*s: frame.ml *)
module S = Symbol
module T = Tree
module H = Hashtbl
(*x: frame.ml *)
(*s: type Frame.frame *)
type frame = { 
  mutable params : (Tree.label * Tree.is_ptr) list;
  mutable vars   : (Tree.label * Tree.is_ptr) list;
  mutable temps  : (Tree.label * Tree.is_ptr) list;
  (*s: [[Frame.frame]] other fields *)
  name           : Tree.label;
  (*x: [[Frame.frame]] other fields *)
  mutable size   : int;
  (*x: [[Frame.frame]] other fields *)
  level          : int;
  (*e: [[Frame.frame]] other fields *)
}
(*e: type Frame.frame *)
(*s: type Frame.access *)
type access =
    Stack of frame * int * Tree.is_ptr
(*e: type Frame.access *)
(*x: frame.ml *)
(*s: function Frame.fp *)
let fp    _frm = 
  T.NAME (S.symbol "fp")
(*e: function Frame.fp *)
let name  frm = 
  frm.name
let level frm = 
  frm.level
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
(*s: function [[Frame.stack_alloc]] *)
let stack_alloc frm ptr =
  let v = Stack(frm, frm.size, ptr) in
  frm.size <- frm.size + 1; 
  v
(*e: function [[Frame.stack_alloc]] *)

(*s: function [[Frame.alloc_param]] *)
let alloc_param frm name ptr =
  frm.params <- frm.params @ [(name,ptr)];
  stack_alloc frm ptr
(*e: function [[Frame.alloc_param]] *)

(*s: function [[Frame.alloc_local]] *)
let alloc_local frm name ptr =
  frm.vars <- (name,ptr) :: frm.vars;
  stack_alloc frm ptr
(*e: function [[Frame.alloc_local]] *)
(*x: frame.ml *)
let alloc_temp frm name ptr =
  frm.temps <- (name,ptr) :: frm.temps
(*x: frame.ml *)
(*s: global Frame.strings *)
let strings = H.create 20
(*e: global Frame.strings *)
(*s: function [[Frame.alloc_string]] *)
let alloc_string s =
  try H.find strings s
  with Not_found ->
    let lbl = T.new_label "gbl" in
    (H.add strings s lbl; lbl)
(*e: function [[Frame.alloc_string]] *)
(* claude: cell width/alignment follow -64 (Option.arch64). qc-- ties this
 * to the "target ... wordsize N pointersize N" pragma Codegen emits, which
 * must match the chosen qc-- backend's metrics exactly - verified against
 * the installed qc's -x86 backend, which rejects a wordsize-64 pragma with
 * "metrics of source code don't match the target". So every stack slot,
 * global and memory access width has to track this too, not just the
 * pragma line. *)
let ws()       = if !Option.arch64 then 8 else 4
let bits_str() = if !Option.arch64 then "bits64" else "bits32"
let align()    = if !Option.arch64 then 8 else 4
(*x: frame.ml *)
let pf           = Printf.printf
let spf          = Printf.sprintf
let join_map f l = String.concat "," (List.map f l)
let iter_ndx f   = let n   = ref(-1) in
                   let g x = incr n; f !n x in
                   List.iter g
(*x: frame.ml *)
let output_header frm =
  let bits          = bits_str() in
  let param  (p,_) = spf "%s %s" bits (S.name p)
  and init n (p,_) = pf "  %s[fp+%d] = %s;\n" bits (ws()*n) (S.name p)
  and temp   (t,_) = pf "  %s %s;\n" bits (S.name t)
  and name         = (S.name frm.name) in
  pf "%s(%s) {\n" name (join_map param frm.params);
  pf " span 1 %s_gc_data {\n" name;
  pf "  stackdata { align %d; fp : %s[%d]; }\n" (align()) bits frm.size;
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
  (* claude: these GC descriptor tables are counts and 0/1 pointer flags,
   * not Tiger-value words, and runtime/gc.c reads them as plain 4-byte
   * "unsigned"/"int" regardless of target - so unlike everything else in
   * this file they intentionally stay bits32 even under -64. *)
  pf "   bits32[] { %s };\n" (var_data (frm.params @ List.rev frm.vars));
  pf "   bits32[] { %s };\n" (var_data (frm.params @ frm.temps));
  pf "}\n\n"
(*x: frame.ml *)
(* output *)
let output_strings() =
  let print_string str lbl =
    let len = String.length str
    and str = String.escaped str in
    pf " %s: %s { %d }; bits8[] \"%s\\000\";\n"
       (S.name lbl) (bits_str()) len str
  in
  pf "section \"data\" { align %d;\n" (align());
  H.iter print_string strings;
  pf "}\n\n"
(*e: frame.ml *)
(*e: frontend/frame.ml *)
