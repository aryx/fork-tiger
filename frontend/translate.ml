# 47 "translate.nw"
module E = Error
module A = Ast
module S = Symbol
module T = Tree
module F = Frame
type exp   = T.exp
type label = T.label
let  ws    = Sys.word_size / 8
# 73 "translate.nw"
let rec seq = function
    []        -> E.internal "nil passed to seq"
  | x :: []   -> x
  | x :: rest -> T.SEQ(x, seq rest)
# 82 "translate.nw"
let eseq exp stmts = T.ESEQ (seq stmts, exp)
# 87 "translate.nw"
let temp ptr = T.TEMP (T.new_temp(), ptr)
# 97 "translate.nw"
let getfp frm parent_frm =
  let diff = F.level frm  - F.level parent_frm in
  let rec deref = function
      0 -> F.fp frm
    | x -> T.MEM (deref (x-1), true)
  in assert (diff >= 0); deref diff
# 108 "translate.nw"
let alloc_ptr = T.NAME (S.symbol "alloc_ptr")
let space_end = T.MEM  (T.NAME (S.symbol "space_end"), true)
let goto lbl  = T.JUMP (T.NAME lbl)
let ( =>) e v = T.MOVE (e, v)

let simplify tig_op op x y =
  match (x,y) with
    (T.CONST x, T.CONST y) -> T.CONST (op x y)
  | _                      -> T.BINOP (tig_op, x, y)
let (<+>) = simplify T.PLUS  ( + )
let (<->) = simplify T.MINUS ( - )
let (<*>) = simplify T.MUL   ( * )
# 129 "translate.nw"
let nil           = T.CONST 0
let int_literal i = T.CONST i
let str_literal s = T.NAME (F.alloc_string s)
# 145 "translate.nw"
let call myfrm lbl cc frm args k ptr =
  let args =
    if F.level frm == 0 then args
    else let pfp =
      if (F.level frm) > (F.level myfrm) then F.fp frm
      else T.MEM(getfp myfrm frm, true)
    in pfp  :: args
  in
# 159 "translate.nw"
  match cc with
    None        -> T.CALL((T.NAME lbl), args, cc, k, ptr)
  | Some "gc"   -> T.CALL((T.NAME lbl), args, cc, k, ptr)
  | Some _ ->
      let tmp1      = temp ptr
      and tmp2      = temp ptr
      in eseq tmp2 [ T.MOVE(alloc_ptr, tmp1);
                     T.MOVE(T.CALL((T.NAME lbl), args, cc, k, ptr), tmp2);
                     T.MOVE(tmp1, alloc_ptr) ]
# 172 "translate.nw"
let ext_call cc name args =
  call F.base_frame (S.symbol name) cc
       F.base_frame args None false

let ext_c_call   = ext_call (Some "C")
let ext_gc_call  = ext_call  None (* (Some "gc") *)
let ext_cmm_call = ext_call  None
# 187 "translate.nw"
let simple_var frm = function
    F.Temp lbl                    -> T.NAME lbl
  | F.Stack(var_frm, offset, ptr) ->
      T.MEM(getfp frm var_frm <+> T.CONST(offset * ws), ptr)
# 196 "translate.nw"
let field_var ex i ptr = T.MEM(ex <+> T.CONST(i * ws), ptr)
# 204 "translate.nw"
let subscript_var e1 e2 ptr pos =
  let check = ext_c_call "bounds_check"
                        [e1;e2;T.CONST(fst (Error.line_number pos))]
  and offset = (e2 <+> T.CONST 1) <*> T.CONST ws
  in
  eseq (T.MEM(e1 <+> offset, ptr)) [T.EXP check]
# 213 "translate.nw"
let assign v e = eseq nil [e => v]
# 222 "translate.nw"
let arithmetic op ex1 ex2 =
  let oper = match op with
    A.PlusOp   -> T.PLUS
  | A.MinusOp  -> T.MINUS
  | A.TimesOp  -> T.MUL
  | A.DivideOp -> T.DIV
  | _          -> E.internal "relop used as binop"
  in T.BINOP(oper, ex1, ex2)

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
# 247 "translate.nw"
let compare_str op ex1 ex2 =
  let result = ext_c_call "compare_str" [ex1;ex2] in
  compare_int op result (T.CONST 0)
# 256 "translate.nw"
let ifexp test thn els ptr =
  let tmp  = temp ptr
  and tru  = T.new_label "ifTrue"
  and fls  = T.new_label "ifFalse"
  and end' = T.new_label "ifEnd" in
  eseq tmp [ T.CJUMP(test, tru, fls);
             T.LABEL tru; thn => tmp; goto end';
             T.LABEL fls; els => tmp;
             T.LABEL end']
# 272 "translate.nw"
let loop test body lend = 
  let lbeg = T.new_label "loop_start"
  and lbdy = T.new_label "loop_body" in
  eseq nil [ T.LABEL lbeg;
             T.CJUMP(test, lbdy, lend);
             T.LABEL lbdy; T.EXP body; goto lbeg;
             T.LABEL lend ]
# 284 "translate.nw"
let break lbl = eseq nil [goto lbl]
# 293 "translate.nw"
let alloc size =
  let size = (size <+> T.CONST 1) <*> T.CONST ws in
  let test = T.RELOP(T.GT, alloc_ptr <+> size, space_end)
  and tmp  = temp true
  and tru  = T.new_label "alc_gc"
  and fls  = T.new_label "alc"
  in eseq tmp [ T.CJUMP(test, tru, fls);
                T.LABEL tru; T.EXP (ext_gc_call "call_gc" []);
                T.LABEL fls;
                size => T.MEM(alloc_ptr, true);
                (alloc_ptr <+> T.CONST ws) => tmp;
                (alloc_ptr <+> size) => alloc_ptr
                (* ; T.EXP (ext_gc_call "call_gc" []) *)
              ]
# 315 "translate.nw"
let new_record init =
  let tmp  = temp true
  and size = T.CONST (List.length init) in
  let rec initialize offset = function
      []             -> []
    | (ex,ptr)::rest -> (ex => field_var tmp offset ptr)
                        :: initialize (offset+1) rest
  in
  eseq tmp ((alloc size => tmp) :: initialize 0 init)
# 331 "translate.nw"
let new_array sizeEx initEx ptr = 
  let ary  = temp true
  and i    = temp false
  and lbeg = T.new_label "init_start"
  and lend = T.new_label "init_end" in
  eseq ary
    [ alloc (sizeEx <+> T.CONST 1) => ary;
      sizeEx => T.MEM(ary, false);
      T.CONST 1 => i;
      T.LABEL lbeg;
      initEx => T.MEM (ary <+> (i <*> T.CONST ws), ptr);
      i <+> T.CONST 1 => i;
      T.CJUMP(T.RELOP(T.LE, i, sizeEx <+> T.CONST 1), lbeg, lend);
      T.LABEL lend ]
# 352 "translate.nw"
let rec sequence = function
   []       -> nil
  | e :: [] -> e
  | e :: es -> T.ESEQ((T.EXP e), (sequence es))
# 363 "translate.nw"
let func frm ex ptr =
  let tmp = temp ptr in
  eseq nil [ex => tmp; T.RET tmp]
# 377 "translate.nw"
let try_block exp exn_lbl hs =
  let cont l = function
      T.TEMP(t,_) -> T.CONT(l, [t])
    | _           -> E.internal "non temp in continuation node"
  in
# 388 "translate.nw"
  let try_endl         = T.new_label "try_end"
  and tmp              = temp false in
  let handler (uid,ex) =
    let hl = T.new_label "handle"
    and sl = T.new_label "skip" in
    [ T.CJUMP(T.RELOP(T.EQ, tmp, T.CONST uid), hl, sl);
      T.LABEL hl; T.EXP ex; goto try_endl;
      T.LABEL sl ]
  in
# 406 "translate.nw"
  let old            = temp false in
  let set_handler    = ext_cmm_call "set_handler" [T.NAME exn_lbl] => old
  and reset_handler  = T.EXP (ext_cmm_call "set_handler" [old])
  and not_unwind stm = if Option.use_unwind() then T.EXP nil else stm
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
# 425 "translate.nw"
let raise_exn uid =
  let fn = if Option.use_unwind() then "unwind" else "raise" in
  ext_cmm_call fn [T.CONST uid]
# 434 "translate.nw"
let spawn lbl = ext_cmm_call "spawn" [T.NAME lbl]
