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
  (* claude: zero every locally-declared stack slot flagged as a GC
   * pointer, before any call in the procedure body can trigger a
   * collection - params get a real initial value just above, but a local
   * declared via alloc_local sits uninitialised until the program's own
   * first assignment to it, and C-- stackdata itself carries no initial
   * value (see the language spec). Until then, gc.c's root scan reads
   * whatever residual stack garbage an earlier call sequence left there;
   * if that garbage happens to look like a heap pointer, it corrupts the
   * collector - see fork-c--'s docs/claude_notes/todo_colmajor.txt,
   * "Option A", which diagnosed exactly this and recommended this fix.
   * That file's own dated 2026-09-03 follow-up section has the fix's
   * full verification across every qc-- backend and, importantly, a
   * discussion of two more precise (and more expensive) alternative
   * designs that were considered and deliberately NOT built here - read
   * that before reaching for either one. gc.c's is_pointer skips a zero
   * value, so a zeroed slot is inert until it holds a real object.
   * Locals sit right after the params in stack-slot order (both share
   * frm.size's single counter via stack_alloc), and frm.vars is
   * prepended by alloc_local, so List.rev restores allocation order -
   * matching output_footer's own List.rev frm.vars for the gc_data
   * table below. *)
  let nparams = List.length frm.params in
  let zero_var n (_, ptr) =
    if ptr then pf "  %s[fp+%d] = 0;\n" bits (ws()*(nparams+n))
  in
  iter_ndx zero_var (List.rev frm.vars);
  List.iter temp frm.temps
(*x: frame.ml *)
let output_footer frm =
  (* claude: under -64 a bare integer literal's default width tracks the
   * target's native word size (qc-- elab/elabexp.ml: "const
   * metrics.M.wordsize"), i.e. 64, which then fails elaboration against
   * these tables' declared bits32 element type ("type of an initial value
   * does not match declared type bits32", confirmed against the installed
   * qc). So under -64 each literal needs an explicit "::bits32" suffix;
   * leave the 32-bit case untouched (word size already matches) to keep
   * the existing recorded test baseline unchanged. *)
  let suffix = if !Option.arch64 then "::bits32" else "" in
  let var_data vl =
    let int_of_var (_,p) = if p then 1 else 0 in
    let data = List.length vl :: List.map int_of_var vl in
    join_map (fun i -> string_of_int i ^ suffix) data
  in
  pf "}}\n";
  (* claude: was "section \"data\" {\n" with no alignment, unlike
   * output_strings's own "section \"data\" { align %d;\n" two functions
   * below - runtime/gc.c reads every word of this table as a plain 4-byte
   * "unsigned" (see the comment on the bits32[] lines below), but x86/ppc
   * silently tolerate an unaligned word load while SPARC traps (SIGBUS).
   * Landed on a misaligned address purely by data-layout luck (4 of 5
   * spot-checked fork-c-- sparc tiger tests; qsort/arrays/colmajor/merge
   * misaligned, queens happened to land aligned) - same bug class as the
   * already-fixed curr_exn global (fork-c--'s
   * notes_debugging_techniques.txt entry 33), just for this table
   * instead. Confirmed via fork-c--: qsort's SIGBUS was reading
   * spans[1]'s pointed-to descriptor, tiger_main_gc_data, at real linked
   * address 0xb013a (0xb013a & 3 = 2). *)
  pf "section \"data\" { align %d;\n" (align());
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
    (* claude: the section-level "align %d;" below only guarantees the
     * FIRST entry starts aligned - each string's payload
     * (bits8[]"str\000") is a variable length, so without a fresh align
     * here too every entry after the first drifts to whatever offset
     * the previous string's length happened to leave it at. This table
     * is read back as bits32 words (the leading length field) by
     * runtime/gc.c and the tig_* stdlib (tig_size/tig_compare_str/...);
     * x86/ppc silently tolerate an unaligned word load, SPARC traps
     * (SIGBUS) - same bug class fixed for output_footer's own
     * _gc_data table just above, just needing a per-entry fix here
     * instead of a once-per-section one, since that table's every field
     * is already word-sized throughout. *)
    pf " align %d;\n" (align());
    pf " %s: %s { %d }; bits8[] \"%s\\000\";\n"
       (S.name lbl) (bits_str()) len str
  in
  pf "section \"data\" { align %d;\n" (align());
  H.iter print_string strings;
  pf "}\n\n"
(*e: frame.ml *)
(*e: frontend/frame.ml *)
