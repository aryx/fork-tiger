(*s: semantics.ml *)
module E = Error
module A = Ast
module S = Symbol
module V = Environment
module T = Translate
(*s: variable types *)
type vartype =
    UNIT
  | NIL
  | INT
  | STRING
  | ARRAY  of vartype
  | RECORD of (Symbol.symbol * vartype) list
  | NAME   of Symbol.symbol
  | ANY
(*e: variable types *)
(*s: type system *)
let rec type_name = function
    RECORD l -> (List.fold_left (fun x y -> x ^ (type_name (snd y)))
                "record {" l) ^ "}"
  | NIL      -> "nil"
  | INT      -> "int"
  | STRING   -> "string"
  | ARRAY vt -> "array of " ^ (type_name vt)
  | NAME(s)  -> "named type " ^ (S.name s)
  | UNIT     -> "unit"
  | ANY      -> "any"
(*x: type system *)
let rec base_type env = function
    NAME s ->
      begin
        try base_type env (V.lookup_type env s 0)
        with Not_found -> E.internal "NAME symbol not found"
      end
  | x -> x

let lookup_base_type env sym pos =
  base_type env (V.lookup_type env sym pos)
(*x: type system *)
let is_ptr = function
    INT | UNIT -> false
  | NIL | RECORD _ | STRING | ARRAY _ -> true
  | _ -> E.internal "non-base type for variable"
(*x: type system *)
let check_type_t ty pos msg typ =
  if typ <> ty
  then E.type_err pos (msg ^ " must be of type " ^ type_name ty)
let check_type_int  = check_type_t INT
let check_type_unit = check_type_t UNIT
(*x: type system *)
let check_type_eq pos msg t1 t2 =
  let check = match (t1,t2) with
                (RECORD _,NIL)
              | (NIL,RECORD _)
              | (ARRAY ANY, ARRAY _)
              | (ARRAY _, ARRAY ANY) -> false
              | _                    -> true
  in if check && t1 <> t2 then
    E.type_err pos (Printf.sprintf msg (type_name t1) (type_name t2))
(*e: type system *)
(*s: translators *)
let functions                 = ref []
let get_functions ()          = List.rev !functions
let add_function frm (ex,typ) =
  functions := (frm, T.func frm ex (is_ptr typ)) :: !functions
(*x: translators *)
type ast_node = DEC of Ast.dec | EXP of Ast.exp
let rec trans (env : vartype V.t) (node : ast_node) =
  (*s: declaration translator *)
  let rec trdec = function
    (*s: function declarations *)
      A.FunctionDec functions ->
        let mk_param fenv (name, typ, pos) =
          let t = lookup_base_type env typ pos in
          V.enter_param fenv name t (is_ptr t)
        in
        let mk_func_env (name, params, typ, _, pos) =
          let ret_type = match typ with
            Some x -> lookup_base_type env x pos
          | None   -> UNIT
          and types = List.map (fun(_,t,p)-> lookup_base_type env t p) params in
          let fenv  = V.enter_fun env name None types ret_type in
          List.iter (mk_param fenv) params;
          fenv
        in
    (*x: function declarations *)
        let trans_func fenv (_, _, _, body, _) =
          let b = trans fenv (EXP body) in
          add_function (V.frame fenv) b
        in
    (*x: function declarations *)
        let envs = (List.map mk_func_env functions) in
        List.iter2 trans_func envs functions;
        T.nil
    (*e: function declarations *)
    (*s: variable declarations *)
      | A.VarDec(name, typ, init, pos) ->
          let e,t = trexp init in
          begin match typ with
            Some x -> check_type_eq pos
                "Variable of type %s cannot be initialized with type %s"
                (V.lookup_type env x pos) t
          | None -> ()
          end;
          let acc = V.enter_local env name t (is_ptr t) in
          T.assign (T.simple_var (V.frame env) acc) e
    (*e: variable declarations *)
    (*s: type declarations *)
      | A.TypeDec types ->
          let penv = V.new_scope env in
          let real_type (name, typ, _) = (name, match typ with
            A.NameTy(name, pos) -> V.lookup_type penv name pos
          | A.RecordTy(fields) ->
              let chkfld(name,ty,p) = (name,(V.lookup_type penv ty p))
              in RECORD (List.map chkfld fields)
          | A.ArrayTy(name, pos) -> ARRAY (V.lookup_type penv name pos))
          in
          List.iter (fun(n,_,_) -> V.enter_type penv n (NAME n)) types;
          let real_types = (List.map real_type types) in
          List.iter (fun (n,t) -> V.enter_type env n t) real_types;
          T.nil
    (*e: type declarations *)
    (*s: exception declarations *)
      | A.ExceptionDec(sym,_) -> V.enter_exn env sym; T.nil
    (*e: exception declarations *)
  (*e: declaration translator *)
  (*s: variable translator *)
  and trvar = function
    (*s: simple vars *)
        A.SimpleVar(sym, pos) ->
          begin match V.lookup_value env sym pos with
            V.VarEntry(acc, vt) ->
              (T.simple_var (V.frame env) acc, base_type env vt)
          | V.FunEntry _ ->
              E.type_err pos "function used as value"
          end
    (*e: simple vars *)
    (*s: field vars *)
      | A.FieldVar(var, sym, pos) ->
          let (exp, fields) = match (trvar var) with
            (x, RECORD y) -> (x,y)
          | _ -> E.type_err pos "attempt to dereference non-record type"
          in
          let offset  = ref (-1) in
          let (_,fld) =
            try List.find (fun (s,v) -> incr offset; s = sym) fields
            with Not_found -> E.undefined pos (S.name sym)
          in
          let typ = base_type env fld in
          (T.field_var exp !offset (is_ptr typ), typ)
    (*e: field vars *)
    (*s: subscript vars *)
      | A.SubscriptVar(var, exp, pos) ->
          let e,t = (trexp exp) in
          check_type_int pos "subscript variable" t;
          begin match (trvar var) with
            (exp, ARRAY vt) ->
              let typ = (base_type env vt) in
              (T.subscript_var exp e (is_ptr typ) pos, typ)
          | _ ->
              E.type_err pos "attempt to dereference a non-array type"
          end
    (*e: subscript vars *)
  (*e: variable translator *)
  (*s: expression translator *)
  and trexp = function
    (*s: simple expressions *)
        A.VarExp v       -> trvar v
      | A.NilExp         -> (T.nil,           NIL)
      | A.IntExp i       -> (T.int_literal i, INT)
      | A.StringExp(s,_) -> (T.str_literal s, STRING)
    (*e: simple expressions *)
    (*s: records *)
      | A.RecordExp(var, fields, pos) ->
          let chk_field (s1,e,p) (s2,vt) =
            if (s1 <> s2) then E.type_err p "field names do not match";
            let ex,ty = trexp e in
            check_type_eq p "field type (%s) does not match declaration (%s)"
                            (base_type env vt) ty;
            (ex, is_ptr ty)
          in
          begin match V.lookup_type env var pos with
            RECORD dec_fields ->
              begin try
                let field_vals = (List.map2 chk_field fields dec_fields) in
                (T.new_record field_vals, RECORD dec_fields)
              with Invalid_argument s ->
                E.type_err pos "Record instance does not match declared type"
              end
          | _ ->
              E.type_err pos "Attempt to use non-record type as record"
          end
    (*e: records *)
    (*s: arrays *)
      | A.ArrayExp(name, size, init, pos) ->
          begin match V.lookup_type env name pos with
            ARRAY vt ->
              let size,sizety = trexp size
              and init,initty = trexp init
              and typ         = base_type env vt in
              check_type_int pos "array size" sizety;
              check_type_eq  pos "array type(%s) does not type(%s)" typ initty;
              (T.new_array size init (is_ptr typ), ARRAY vt)
          | _ ->
              E.type_err pos "Attempt to use a non-array type as an array"
          end
    (*e: arrays *)
    (*s: assignment *)
      | A.AssignExp(var, exp, pos) ->
          let exp,ety = trexp exp
          and var,vty = trvar var in
          check_type_eq pos "Cannot assign to type %s from type %s" vty ety;
          (T.assign var exp, UNIT)
    (*e: assignment *)
    (*s: operator expressions *)
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
    (*x: operator expressions *)
    let compare_str op ex1 ex2 =
      let result = ext_c_call "compare_str" [ex1;ex2] in
      compare_int op result (T.CONST 0)
    (*x: operator expressions *)
      | A.OpExp(left, oper, right, pos) ->
          let lexp,lty = trexp left
          and rexp,rty = trexp right in
          check_type_eq pos "Incompatible types %s,%s" lty rty;
          let trans_fn =
            match oper with
              A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp ->
                check_type_int pos "operator argument" lty;    T.arithmetic
            | A.EqOp | A.NeqOp
            | A.LtOp | A.LeOp | A.GtOp | A.GeOp ->
                begin match lty with
                  INT | NIL                                 -> T.compare_int
                | STRING                                    -> T.compare_str
                | ARRAY  _ when oper=A.EqOp or oper=A.NeqOp -> T.compare_int
                | RECORD _ when oper=A.EqOp or oper=A.NeqOp -> T.compare_int
                | _ ->
                    E.type_err pos "Incomparable types"
                end
          in (trans_fn oper lexp rexp, INT)
    (*e: operator expressions *)
    (*s: function calls *)
    let call myfrm lbl cc frm args k ptr =
      let args =
        if F.level frm == 0 then args
        else let pfp =
          if (F.level frm) > (F.level myfrm) then F.fp frm
          else T.MEM(getfp myfrm frm, true)
        in pfp  :: args
      in
    (*x: function calls *)
      match cc with
        None        -> T.CALL((T.NAME lbl), args, cc, k, ptr)
      | Some "gc"   -> T.CALL((T.NAME lbl), args, cc, k, ptr)
      | Some _ ->
          let tmp1      = temp ptr
          and tmp2      = temp ptr
          in eseq tmp2 [ T.MOVE(alloc_ptr, tmp1);
                         T.MOVE(T.CALL((T.NAME lbl), args, cc, k, ptr), tmp2);
                         T.MOVE(tmp1, alloc_ptr) ]
    (*x: function calls *)
    let ext_call cc name args =
      call F.base_frame (S.symbol name) cc
           F.base_frame args None false

    let ext_c_call   = ext_call (Some "C")
    let ext_gc_call  = ext_call  None (* (Some "gc") *)
    let ext_cmm_call = ext_call  None
    (*x: function calls *)
      | A.CallExp(sym, arglist, pos) ->
          let chk_arg = check_type_eq pos
                        "Argument type (%s) does not match declaration (%s)"
          in begin match V.lookup_value env sym pos with
            V.FunEntry(lbl, cc, frm, dec_args, return_type) ->
              let args,tys = List.split (List.map trexp arglist) in
              begin try
                List.iter2 chk_arg tys dec_args;
                let rtyp = base_type env return_type in
                (T.call (V.frame env) lbl cc frm args
                        (V.exn_label env) (is_ptr rtyp), rtyp)
              with Invalid_argument x ->
                E.type_err pos "function arguments do not match declaration"
              end
          | _ ->
              E.type_err pos (S.name sym ^ " is not a function")
          end
    (*e: function calls *)
    (*s: conditionals *)
    let ifexp test thn els ptr =
      let tmp  = temp ptr
      and tru  = T.new_label "ifTrue"
      and fls  = T.new_label "ifFalse"
      and end' = T.new_label "ifEnd" in
      eseq tmp [ T.CJUMP(test, tru, fls);
                 T.LABEL tru; thn => tmp; goto end';
                 T.LABEL fls; els => tmp;
                 T.LABEL end']
    (*x: conditionals *)
      | A.IfExp(if', then', else', pos) ->
          let iex,ity = trexp if'
          and tex,tty = trexp then'
          and eex,ety = match else' with
                          None    -> (T.nil, UNIT)
                        | Some ex -> trexp ex
          in
          check_type_int pos "if condition" ity;
          check_type_eq  pos
            "type of then expression (%s) does not match else (%s)" tty ety;
          let typ = base_type env tty in
          (T.ifexp iex tex eex (is_ptr typ), typ)
    (*e: conditionals *)
    (*s: loops *)
    let loop test body lend = 
      let lbeg = T.new_label "loop_start"
      and lbdy = T.new_label "loop_body" in
      eseq nil [ T.LABEL lbeg;
                 T.CJUMP(test, lbdy, lend);
                 T.LABEL lbdy; T.EXP body; goto lbeg;
                 T.LABEL lend ]
    (*x: loops *)
    let break lbl = eseq nil [goto lbl]
    (*x: loops *)
      | A.WhileExp(test, body, pos) ->
          let body_env = V.new_break_label env in
          let tex,tty = trexp test
          and bex,bty = trans body_env (EXP body) in
          check_type_int pos "while condition" tty;
          check_type_eq  pos "body of while has type %s, must be %s" bty UNIT;
          (T.loop tex bex (V.break_label body_env), UNIT)
    (*x: loops *)
      | A.ForExp(sym, lo, hi, body, pos) ->
          let _,loty = trexp lo
          and _,hity = trexp hi in
          check_type_int pos "for lower bound" loty;
          check_type_int pos "for upper bound" hity;
          let v            = A.SimpleVar(sym, pos) in
          let ve           = A.VarExp v in
          let v_less_eq_hi = A.OpExp(ve, A.LeOp, hi, pos)
          and v_plus_1     = A.OpExp(ve, A.PlusOp, (A.IntExp 1), pos) in
          trexp (A.LetExp(
                 [(A.VarDec(sym,(Some(S.symbol "int")), lo, pos))],
                 (A.WhileExp
                    (v_less_eq_hi,
                     (A.SeqExp([body;
                               (A.AssignExp(v, v_plus_1, pos))], pos)),
                     pos)),
                 pos))
    (*x: loops *)
      | A.BreakExp pos ->
          begin
            try (T.break (V.break_label env), UNIT)
            with Not_found -> raise(E.Error(E.Illegal_break, pos))
          end
    (*e: loops *)
    (*s: sequences *)
    let rec sequence = function
       []       -> nil
      | e :: [] -> e
      | e :: es -> T.ESEQ((T.EXP e), (sequence es))
    (*x: sequences *)
      | A.SeqExp ([],_) -> (T.nil, UNIT)
      | A.SeqExp (el,_) ->
          let exprs = List.rev_map trexp el in
          let _,typ = List.hd exprs in
          let exprs = List.rev_map fst exprs in
          (T.sequence exprs, typ)
    (*e: sequences *)
    (*s: let expressions *)
      | A.LetExp(decls, body, _) ->
          let trns    = trans (V.new_scope env) in
          let decs    = List.map (fun d -> fst (trns (DEC d))) decls
          and bex,bty = trns (EXP body) in
          (T.sequence (decs @ [bex]), bty)
    (*e: let expressions *)
    (*s: exceptions *)
    let try_block exp exn_lbl hs =
      let cont l = function
          T.TEMP(t,_) -> T.CONT(l, [t])
        | _           -> E.internal "non temp in continuation node"
      in
    (*x: exceptions *)
      let try_endl         = T.new_label "try_end"
      and tmp              = temp false in
      let handler (uid,ex) =
        let hl = T.new_label "handle"
        and sl = T.new_label "skip" in
        [ T.CJUMP(T.RELOP(T.EQ, tmp, T.CONST uid), hl, sl);
          T.LABEL hl; T.EXP ex; goto try_endl;
          T.LABEL sl ]
      in
    (*x: exceptions *)
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
    (*x: exceptions *)
    let raise_exn uid =
      let fn = if Option.use_unwind() then "unwind" else "raise" in
      ext_cmm_call fn [T.CONST uid]
    (*x: exceptions *)
      | A.TryExp(expr, handlers, pos) ->
          let new_env         = V.new_exn_label env in
          let tryex, tryty    = trans new_env (EXP expr) in
          let handler (s,h,p) =
            let ex,ty = trexp h in
            check_type_unit p "handler" ty;
            (S.uid s, ex)
          in
          check_type_unit pos "try" tryty;
          begin match V.exn_label new_env with
            None     -> E.internal "no exception label for try block"
          | Some lbl ->
              (T.try_block tryex lbl (List.map handler handlers), tryty)
          end
    (*x: exceptions *)
      | A.RaiseExp(sym, pos) ->
          let exn_id = V.lookup_exn env sym pos in
          (T.raise_exn exn_id, UNIT)
    (*e: exceptions *)
    (*s: threads *)
    let spawn lbl = ext_cmm_call "spawn" [T.NAME lbl]
    (*x: threads *)
      | A.SpawnExp(sym, pos) ->
          begin match V.lookup_value env sym pos with
            V.FunEntry(lbl, cc, frm, dec_args, return_type) ->
              if dec_args <> []
              then E.type_err pos "spawn function must take zero arguments."
              else (T.spawn lbl,INT)
          | _ ->
              E.type_err pos (S.name sym ^ " is not a function")
          end
    (*e: threads *)
  (*e: expression translator *)
in match node with
  DEC d -> (trdec d, NIL)
| EXP e -> trexp e
(*e: translators *)
(*s: entry point *)
let translate env ast =
  begin
    let (mainex,mainty) = trans env (EXP ast) in
    if mainty <> INT && mainty <> UNIT then
      E.type_err 0 "tiger program must return INT or UNIT"
    else ();
    add_function (V.frame env) (mainex,mainty);
    get_functions()
  end
(*e: entry point *)
(*e: semantics.ml *)
