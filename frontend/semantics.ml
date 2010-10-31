# 30 "semantics.nw"
module E = Error
module A = Ast
module S = Symbol
module V = Environment
module T = Translate
# 49 "semantics.nw"
type vartype =
    UNIT
  | NIL
  | INT
  | STRING
  | ARRAY  of vartype
  | RECORD of (Symbol.symbol * vartype) list
  | NAME   of Symbol.symbol
  | ANY
# 62 "semantics.nw"
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
# 84 "semantics.nw"
let rec base_type env = function
    NAME s ->
      begin
        try base_type env (V.lookup_type env s 0)
        with Not_found -> E.internal "NAME symbol not found"
      end
  | x -> x

let lookup_base_type env sym pos =
  base_type env (V.lookup_type env sym pos)
# 106 "semantics.nw"
let is_ptr = function
    INT | UNIT -> false
  | NIL | RECORD _ | STRING | ARRAY _ -> true
  | _ -> E.internal "non-base type for variable"
# 114 "semantics.nw"
let check_type_t ty pos msg typ =
  if typ <> ty
  then E.type_err pos (msg ^ " must be of type " ^ type_name ty)
let check_type_int  = check_type_t INT
let check_type_unit = check_type_t UNIT
# 128 "semantics.nw"
let check_type_eq pos msg t1 t2 =
  let check = match (t1,t2) with
                (RECORD _,NIL)
              | (NIL,RECORD _)
              | (ARRAY ANY, ARRAY _)
              | (ARRAY _, ARRAY ANY) -> false
              | _                    -> true
  in if check && t1 <> t2 then
    E.type_err pos (Printf.sprintf msg (type_name t1) (type_name t2))
# 147 "semantics.nw"
let functions                 = ref []
let get_functions ()          = List.rev !functions
let add_function frm (ex,typ) =
  functions := (frm, T.func frm ex (is_ptr typ)) :: !functions
# 157 "semantics.nw"
type ast_node = DEC of Ast.dec | EXP of Ast.exp
let rec trans (env : vartype V.t) (node : ast_node) =
  
# 185 "semantics.nw"
let rec trdec = function
  
# 202 "semantics.nw"
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
# 222 "semantics.nw"
    let trans_func fenv (_, _, _, body, _) =
      let b = trans fenv (EXP body) in
      add_function (V.frame fenv) b
    in
# 232 "semantics.nw"
    let envs = (List.map mk_func_env functions) in
    List.iter2 trans_func envs functions;
    T.nil
# 187 "semantics.nw"
  
# 244 "semantics.nw"
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
# 188 "semantics.nw"
  
# 266 "semantics.nw"
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
# 189 "semantics.nw"
  
# 285 "semantics.nw"
  | A.ExceptionDec(sym,_) -> V.enter_exn env sym; T.nil
# 160 "semantics.nw"
  
# 294 "semantics.nw"
and trvar = function
  
# 304 "semantics.nw"
    A.SimpleVar(sym, pos) ->
      begin match V.lookup_value env sym pos with
        V.VarEntry(acc, vt) ->
          (T.simple_var (V.frame env) acc, base_type env vt)
      | V.FunEntry _ ->
          E.type_err pos "function used as value"
      end
# 296 "semantics.nw"
  
# 319 "semantics.nw"
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
# 297 "semantics.nw"
  
# 337 "semantics.nw"
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
# 161 "semantics.nw"
  
# 353 "semantics.nw"
and trexp = function
  
# 372 "semantics.nw"
    A.VarExp v       -> trvar v
  | A.NilExp         -> (T.nil,           NIL)
  | A.IntExp i       -> (T.int_literal i, INT)
  | A.StringExp(s,_) -> (T.str_literal s, STRING)
# 355 "semantics.nw"
  
# 387 "semantics.nw"
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
# 356 "semantics.nw"
  
# 411 "semantics.nw"
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
# 357 "semantics.nw"
  
# 430 "semantics.nw"
  | A.AssignExp(var, exp, pos) ->
      let exp,ety = trexp exp
      and var,vty = trvar var in
      check_type_eq pos "Cannot assign to type %s from type %s" vty ety;
      (T.assign var exp, UNIT)
# 358 "semantics.nw"
  
# 449 "semantics.nw"
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
# 359 "semantics.nw"
  
# 476 "semantics.nw"
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
# 360 "semantics.nw"
  
# 501 "semantics.nw"
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
# 361 "semantics.nw"
  
# 520 "semantics.nw"
  | A.WhileExp(test, body, pos) ->
      let body_env = V.new_break_label env in
      let tex,tty = trexp test
      and bex,bty = trans body_env (EXP body) in
      check_type_int pos "while condition" tty;
      check_type_eq  pos "body of while has type %s, must be %s" bty UNIT;
      (T.loop tex bex (V.break_label body_env), UNIT)
# 535 "semantics.nw"
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
# 556 "semantics.nw"
  | A.BreakExp pos ->
      begin
        try (T.break (V.break_label env), UNIT)
        with Not_found -> raise(E.Error(E.Illegal_break, pos))
      end
# 362 "semantics.nw"
  
# 567 "semantics.nw"
  | A.SeqExp ([],_) -> (T.nil, UNIT)
  | A.SeqExp (el,_) ->
      let exprs = List.rev_map trexp el in
      let _,typ = List.hd exprs in
      let exprs = List.rev_map fst exprs in
      (T.sequence exprs, typ)
# 363 "semantics.nw"
  
# 582 "semantics.nw"
  | A.LetExp(decls, body, _) ->
      let trns    = trans (V.new_scope env) in
      let decs    = List.map (fun d -> fst (trns (DEC d))) decls
      and bex,bty = trns (EXP body) in
      (T.sequence (decs @ [bex]), bty)
# 364 "semantics.nw"
  
# 598 "semantics.nw"
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
# 616 "semantics.nw"
  | A.RaiseExp(sym, pos) ->
      let exn_id = V.lookup_exn env sym pos in
      (T.raise_exn exn_id, UNIT)
# 365 "semantics.nw"
  
# 623 "semantics.nw"
  | A.SpawnExp(sym, pos) ->
      begin match V.lookup_value env sym pos with
        V.FunEntry(lbl, cc, frm, dec_args, return_type) ->
          if dec_args <> []
          then E.type_err pos "spawn function must take zero arguments."
          else (T.spawn lbl,INT)
      | _ ->
          E.type_err pos (S.name sym ^ " is not a function")
      end
# 162 "semantics.nw"
in match node with
  DEC d -> (trdec d, NIL)
| EXP e -> trexp e
# 169 "semantics.nw"
let translate env ast =
  begin
    let (mainex,mainty) = trans env (EXP ast) in
    if mainty <> INT && mainty <> UNIT then
      E.type_err 0 "tiger program must return INT or UNIT"
    else ();
    add_function (V.frame env) (mainex,mainty);
    get_functions()
  end
