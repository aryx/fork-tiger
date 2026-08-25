(*s: frontend/translate.ml *)
(*s: translate.ml *)
module E = Error
module A = Ast
module S = Symbol
module T = Tree
module F = Frame

(*s: constant Translate.ws *)
(* claude: word size follows -64 (Frame.ws/Option.arch64), not the host
 * OCaml runtime's word size - qc-- emits i386 by default (32-bit) or,
 * under -64, a 64-bit backend such as alpha, but never the host's own
 * width, so this can't be derived from Sys.word_size either way. Reads as
 * a function call, not a constant, since Option.arch64 is only set once
 * Option.parse_cmdline() runs in Main.main, which is after this module's
 * own top-level bindings would otherwise have been evaluated. *)
let  ws()  = F.ws()
(*e: constant Translate.ws *)

(*s: function Translate.temp *)
let temp ptr = 
  T.TEMP (T.new_temp(), ptr)
(*e: function Translate.temp *)

(*s: function Translate.seq *)
let rec seq = function
    []        -> E.internal "nil passed to seq"
  | x :: []   -> x
  | x :: rest -> T.SEQ(x, seq rest)
(*e: function Translate.seq *)
(*s: function Translate.eseq *)
let eseq exp stmts = 
  T.ESEQ (seq stmts, exp)
(*e: function Translate.eseq *)

(*s: function Translate.simplify *)
let simplify tig_op op = 
 fun x y ->
  match (x,y) with
    (T.CONST x, T.CONST y) -> T.CONST (op x y)
  | _                      -> T.BINOP (tig_op, x, y)
(*e: function Translate.simplify *)
(*s: function [[Translate.angles_xxx]] *)
let (<+>) = simplify T.PLUS  ( + )
let (<->) = simplify T.MINUS ( - )
let (<*>) = simplify T.MUL   ( * )
(*e: function [[Translate.angles_xxx]] *)

(*s: function Translate.getfp *)
let getfp frm parent_frm =
  let diff = F.level frm  - F.level parent_frm in
  let rec deref = function
      0 -> F.fp frm
    | x -> T.MEM (deref (x-1), true)
  in 
  assert (diff >= 0);
  deref diff
(*e: function Translate.getfp *)

(*s: utilities *)
(*s: function Translate.getfp *)
let getfp frm parent_frm =
  let diff = F.level frm  - F.level parent_frm in
  let rec deref = function
      0 -> F.fp frm
    | x -> T.MEM (deref (x-1), true)
  in 
  assert (diff >= 0);
  deref diff
(*e: function Translate.getfp *)
(*x: utilities *)
(*s: constant [[Translate.space_end]] *)
let space_end = T.MEM  (T.NAME (S.symbol "space_end"), true)
(*e: constant [[Translate.space_end]] *)
(*s: constant [[Translate.alloc_ptr]] *)
let alloc_ptr = T.NAME (S.symbol "alloc_ptr")
(*e: constant [[Translate.alloc_ptr]] *)

(*s: function Translate.goto *)
let goto lbl  = 
  T.JUMP lbl
(*e: function Translate.goto *)
(*s: function Translate.arrow *)
let ( =>) e v = T.MOVE (e, v)
(*e: function Translate.arrow *)
(*s: function Translate.simplify *)
let simplify tig_op op = 
 fun x y ->
  match (x,y) with
    (T.CONST x, T.CONST y) -> T.CONST (op x y)
  | _                      -> T.BINOP (tig_op, x, y)
(*e: function Translate.simplify *)
(*s: function [[Translate.angles_xxx]] *)
let (<+>) = simplify T.PLUS  ( + )
let (<->) = simplify T.MINUS ( - )
let (<*>) = simplify T.MUL   ( * )
(*e: function [[Translate.angles_xxx]] *)
(*e: utilities *)


(*s: functions Translate.xxx literals *)
let nil           = T.CONST 0
let int_literal i = T.CONST i
let str_literal s = T.NAME (F.alloc_string s)
(*e: functions Translate.xxx literals *)
(*s: function Translate.call *)
let call caller_frm   lbl cc callee_frm   args k ptr =
  let args =
    if F.level callee_frm == 0 
    then args
    else 
      let pfp =
        if (F.level callee_frm) > (F.level caller_frm) 
        then F.fp callee_frm
        else T.MEM(getfp caller_frm callee_frm, true)
      in 
      pfp  :: args
  in
  match cc with
    None        -> T.CALL((T.NAME lbl), args, cc, k, ptr)
  (*s: [[Translate.call()]] match cc other cases *)
  | Some "gc"   -> T.CALL((T.NAME lbl), args, cc, k, ptr)
  | Some _ ->
      let tmp1      = temp ptr in
      let tmp2      = temp ptr in
      eseq tmp2 [ T.MOVE(alloc_ptr, tmp1);
                  T.MOVE(T.CALL((T.NAME lbl), args, cc, k, ptr), tmp2);
                  T.MOVE(tmp1, alloc_ptr) ]
  (*e: [[Translate.call()]] match cc other cases *)
(*e: function Translate.call *)

(*s: functions [[Translate.ext_xxx_call]] *)
let ext_call cc name args =
  call F.base_frame (S.symbol name) cc
       F.base_frame args None false

let ext_c_call   = ext_call (Some "C")
let ext_gc_call  = ext_call  None (* (Some "gc") *)
let ext_cmm_call = ext_call  None
(*e: functions [[Translate.ext_xxx_call]] *)

(*s: function [[Translate.field_var]] *)
let field_var ex i ptr = T.MEM(ex <+> T.CONST(i * ws()), ptr)
(*e: function [[Translate.field_var]] *)
(*s: function [[Translate.subscript_var]] *)
let subscript_var e1 e2 ptr pos =
  let check = ext_c_call "bounds_check"
                        [e1;e2;T.CONST(fst (Error.line_number pos))] in
  let offset = (e2 <+> T.CONST 1) <*> T.CONST (ws()) in
  eseq (T.MEM(e1 <+> offset, ptr)) [T.EXP check]
(*e: function [[Translate.subscript_var]] *)
(*s: function [[Translate.simple_var]] *)
let simple_var frm = function
  | F.Stack(var_frm, offset, ptr) ->
      T.MEM(getfp frm var_frm <+> T.CONST(offset * ws()), ptr)
(*e: function [[Translate.simple_var]] *)

(*s: function Translate.arithmetic *)
let arithmetic op ex1 ex2 =
  let oper = 
    match op with
      A.PlusOp   -> T.PLUS
    | A.MinusOp  -> T.MINUS
    | A.TimesOp  -> T.MUL
    | A.DivideOp -> T.DIV
    | _          -> E.internal "relop used as binop"
  in 
  T.BINOP(oper, ex1, ex2)
(*e: function Translate.arithmetic *)
(*s: function [[Translate.compare_int]] *)
let compare_int op ex1 ex2 =
  let oper = match op with
    A.EqOp  -> T.EQ
  | A.NeqOp -> T.NE
  | A.LtOp  -> T.LT
  | A.LeOp  -> T.LE
  | A.GtOp  -> T.GT
  | A.GeOp  -> T.GE
  | _       -> E.internal "binop used as relop"
  in T.RELOP(oper, ex1, ex2)
(*e: function [[Translate.compare_int]] *)
(*s: function [[Translate.compare_str]] *)
(* claude: tig_compare_str is a real C function returning a plain 32-bit
 * `int` (stdlib.c) - unlike every other imported runtime helper, which
 * genuinely is native-word-sized (a pointer, or C-- itself). This IR has
 * no notion of a narrower-than-native integer at all (Tree.ml's exp/temp
 * carry no width field - everything is implicitly native-word), which
 * was never a problem in the original 32-bit-only compiler (bits32 ==
 * native word then, by definition) but is exposed now that -64 makes the
 * two diverge: on qc--'s -arm64 backend, a negative result (e.g.
 * strncmp's -1) came back as a huge positive 64-bit number instead,
 * because AArch64 architecturally zero-extends a 32-bit register write
 * into the corresponding 64-bit register - it does not know or care that
 * the value was "supposed to" be sign-extended. (RISC-V64's LP64 calling
 * convention happens to sign-extend automatically, which is why this
 * was never caught there.)
 *
 * Fixed with a plain arithmetic correction instead of threading a real
 * bits32 type through the IR: if the raw result looks like a negative
 * 32-bit value read as an unsigned/zero-extended 64-bit one (i.e. >=
 * 2^31), subtract 2^32 to recover the true signed value. This is a no-op
 * whenever the value is already correctly sign-extended (a genuine
 * negative 64-bit number is never >= 2^31), so the same code is correct
 * on every backend with no Option.arch64 special-casing needed. *)
let compare_str op ex1 ex2 =
  let raw  = ext_c_call "compare_str" [ex1;ex2] in
  let traw = temp false in
  let tmp  = temp false in
  let tru  = T.new_label "negCorrect" in
  let fls  = T.new_label "noCorrect" in
  let end' = T.new_label "correctEnd" in
  let result =
    eseq tmp
      [ raw => traw
      ; T.CJUMP(T.RELOP(T.GE, traw, T.CONST 0x80000000), tru, fls)
      ; T.LABEL tru; (traw <-> T.CONST 0x100000000) => tmp; goto end'
      ; T.LABEL fls; traw => tmp
      ; T.LABEL end'
      ]
  in
  compare_int op result (T.CONST 0)
(*e: function [[Translate.compare_str]] *)

(*s: function Translate.assign *)
let assign v e =
  eseq nil [e => v]
(* claude: a UNIT-typed exp (e.g. a call to a void C runtime function like
 * print) carries no meaningful value - the callee's calling convention makes
 * no promise about what ends up in its "result" register. Reading that
 * value anyway (as ifexp's branch-merge used to unconditionally do) leaks
 * whatever garbage was left over, which became visible as e.g. a Tiger
 * program's exit code being the leftover value of its last print() call
 * instead of 0. Evaluate for effect only, and always yield the same nil. *)
let discard e = eseq nil [T.EXP e]
(*e: function Translate.assign *)
(*s: function Translate.ifexp *)
let ifexp test thn els ptr =
  let tmp  = temp ptr in
  let tru  = T.new_label "ifTrue" in
  let fls  = T.new_label "ifFalse" in
  let end' = T.new_label "ifEnd" in
  eseq tmp [ T.CJUMP(test, tru, fls);
             T.LABEL tru; thn => tmp; goto end';
             T.LABEL fls; els => tmp;
             T.LABEL end']
(*e: function Translate.ifexp *)
(*s: function Translate.loop *)
let loop test body lend = 
  let lbeg = T.new_label "loop_start" in
  let lbdy = T.new_label "loop_body" in
  eseq nil [ T.LABEL lbeg;
             T.CJUMP(test, lbdy, lend);
             T.LABEL lbdy; T.EXP body; goto lbeg;
             T.LABEL lend ]
(*e: function Translate.loop *)
(*s: function Translate.break *)
let break lbl = eseq nil [goto lbl]
(*e: function Translate.break *)

(*s: function Translate.alloc *)
let alloc size =
  let size = (size <+> T.CONST 1) <*> T.CONST (ws()) in
  let test = T.RELOP(T.GT, alloc_ptr <+> size, space_end) in
  let tmp  = temp true in
  let tru  = T.new_label "alc_gc" in
  let fls  = T.new_label "alc" in
  eseq tmp [ T.CJUMP(test, tru, fls);
             T.LABEL tru; T.EXP (ext_gc_call "call_gc" []);
             T.LABEL fls;
             size => T.MEM(alloc_ptr, true);
             (alloc_ptr <+> T.CONST (ws())) => tmp;
             (alloc_ptr <+> size) => alloc_ptr
            ]
(*e: function Translate.alloc *)

(*s: function [[Translate.new_record]] *)
let new_record init =
  let tmp  = temp true in
  let size = T.CONST (List.length init) in
  let rec initialize offset = function
      []             -> []
    | (ex,ptr)::rest -> (ex => field_var tmp offset ptr)
                        :: initialize (offset+1) rest
  in
  eseq tmp ((alloc size => tmp) :: initialize 0 init)
(*e: function [[Translate.new_record]] *)
(*s: function [[Translate.new_array]] *)
let new_array sizeEx initEx ptr = 
  let ary  = temp true in
  let i    = temp false in
  let lbeg = T.new_label "init_start" in
  let lend = T.new_label "init_end" in
  eseq ary
    [ alloc (sizeEx <+> T.CONST 1) => ary;
      sizeEx => T.MEM(ary, false);
      T.CONST 1 => i;
      T.LABEL lbeg;
      initEx => T.MEM (ary <+> (i <*> T.CONST (ws())), ptr);
      i <+> T.CONST 1 => i;
      T.CJUMP(T.RELOP(T.LE, i, sizeEx <+> T.CONST 1), lbeg, lend);
      T.LABEL lend ]
(*e: function [[Translate.new_array]] *)

(*s: function Translate.sequence *)
let rec sequence = function
   []       -> nil
  | e :: [] -> e
  | e :: es -> T.ESEQ((T.EXP e), (sequence es))
(*e: function Translate.sequence *)

(*s: function Translate.func *)
let func body ptr =
  let tmp = temp ptr in
  eseq nil [body => tmp; T.RET tmp]
(*e: function Translate.func *)

(*s: function [[Translate.try_block]] *)
let try_block exp exn_lbl hs =
  let cont l = function
      T.TEMP(t,_) -> T.CONT(l, [t])
    | _           -> E.internal "non temp in continuation node"
  in
  let try_endl         = T.new_label "try_end"
  and tmp              = temp false in
  let handler (uid,ex) =
    let hl = T.new_label "handle"
    and sl = T.new_label "skip" in
    [ T.CJUMP(T.RELOP(T.EQ, tmp, T.CONST uid), hl, sl);
      T.LABEL hl; T.EXP ex; goto try_endl;
      T.LABEL sl ]
  in
  let old            = temp false in
  let set_handler    = ext_cmm_call "set_handler" [T.NAME exn_lbl] => old
  and reset_handler  = T.EXP (ext_cmm_call "set_handler" [old])
  and not_unwind stm = if !Option.unwind then T.EXP nil else stm
  in
  eseq tmp [ T.TRY exn_lbl;
             not_unwind set_handler;
             exp => tmp;
             not_unwind reset_handler;
             T.TRYEND exn_lbl;
             goto try_endl;
             cont exn_lbl tmp;
             not_unwind reset_handler;
             seq (List.flatten (List.map handler hs));
             T.LABEL try_endl ]

(*e: function [[Translate.try_block]] *)
(*s: function [[Translate.raise_exn]] *)
let raise_exn uid =
  let fn = if !Option.unwind then "unwind" else "raise" in
  ext_cmm_call fn [T.CONST uid]
(*e: function [[Translate.raise_exn]] *)

(*s: function Translate.spawn *)
let spawn lbl = ext_cmm_call "spawn" [T.NAME lbl]
(*e: function Translate.spawn *)
(*e: translate.ml *)
(*e: frontend/translate.ml *)
