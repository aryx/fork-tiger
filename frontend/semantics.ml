(*s: frontend/semantics.ml *)
(*s: semantics.ml *)
module E = Error
module A = Ast
module S = Symbol
module Env = Environment
module Trans = Translate

open Environment

(*s: function Semantics.type_name *)
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
(*e: function Semantics.type_name *)
(*s: function Semantics.base_type *)
let rec base_type env = function
    NAME s ->
      (try base_type env (Env.lookup_type env s 0)
       with Not_found -> E.internal "NAME symbol not found"
      )
  | x -> x
(*e: function Semantics.base_type *)
(*s: function Semantics.lookup_base_type *)
let lookup_base_type env sym pos =
  base_type env (Env.lookup_type env sym pos)
(*e: function Semantics.lookup_base_type *)

(*s: function Semantics.is_ptr *)
let is_ptr = function
    INT | UNIT -> false
  | NIL | RECORD _ | STRING | ARRAY _ -> true
  | _ -> E.internal "non-base type for variable"
(*e: function Semantics.is_ptr *)
(*s: functions Semantics.check_type_xxx *)
let check_type_t ty pos msg typ =
  if typ <> ty
  then E.type_err pos (msg ^ " must be of type " ^ type_name ty)

let check_type_int  = check_type_t INT
let check_type_unit = check_type_t UNIT
(*e: functions Semantics.check_type_xxx *)
(*s: function Semantics.check_type_eq *)
let check_type_eq pos msg t1 t2 =
  let are_equivalent = 
    match (t1,t2) with
    | (RECORD _,NIL)
    | (NIL,RECORD _)
    | (ARRAY ANY, ARRAY _)
    | (ARRAY _, ARRAY ANY) -> true
    | _                    -> t1 = t2
  in 
  if not are_equivalent
  then E.type_err pos (Printf.sprintf msg (type_name t1) (type_name t2))
(*e: function Semantics.check_type_eq *)

(*s: translators *)
(*s: global Semantics.functions *)
let functions                 = ref []
(*e: global Semantics.functions *)
(*s: function Semantics.add_function *)
let add_function frm (ex,typ) =
  functions := (frm, Trans.func ex (is_ptr typ)) :: !functions
(*e: function Semantics.add_function *)
(*x: translators *)

(*s: functions Semantics.trxxx *)
(*s: function Semantics.trans.trexp *)
let rec trexp env = function
  (*s: [[Semantics.trans.trexp()]] cases *)
    | A.LetExp(decls, body, _) ->
        let env' = Env.new_scope env in
        let decs = decls |> List.map (fun d -> trdec env' d) in
        let bex,bty = trexp env' body in
        (Trans.sequence (decs @ [bex]), bty)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.NilExp         -> (Trans.nil,           NIL)
    | A.IntExp i       -> (Trans.int_literal i, INT)
    | A.StringExp(s,_) -> (Trans.str_literal s, STRING)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.VarExp v       -> trvar env v
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.RecordExp(var, fields, pos) ->
        (*s: function Semantics.trans.trexp.chk_field *)
        let chk_field (s1,e,p) (s2,vt) =
          if (s1 <> s2) 
          then E.type_err p "field names do not match";
          let ex,ty = trexp env e in
          check_type_eq p "field type (%s) does not match declaration (%s)"
                          (base_type env vt) ty;
          (ex, is_ptr ty)
        in
        (*e: function Semantics.trans.trexp.chk_field *)
        (match Env.lookup_type env var pos with
          RECORD dec_fields ->
            begin try
              let field_vals = (List.map2 chk_field fields dec_fields) in
              (Trans.new_record field_vals, RECORD dec_fields)
            with Invalid_argument s ->
              E.type_err pos "Record instance does not match declared type"
            end
        | _ ->
            E.type_err pos "Attempt to use non-record type as record"
        )
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.ArrayExp(name, size, init, pos) ->
        (match Env.lookup_type env name pos with
          ARRAY vt ->
            let size,sizety = trexp env size in
            let init,initty = trexp env init in
            let typ         = base_type env vt in
            check_type_int pos "array size" sizety;
            check_type_eq  pos "array type(%s) does not type(%s)" typ initty;
            (Trans.new_array size init (is_ptr typ), ARRAY vt)
        | _ ->
            E.type_err pos "Attempt to use a non-array type as an array"
        )
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.OpExp(left, oper, right, pos) ->
        let lexp,lty = trexp env left in
        let rexp,rty = trexp env right in
        check_type_eq pos "Incompatible types %s,%s" lty rty;
        let trans_fn =
          match oper with
            A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp ->
              check_type_int pos "operator argument" lty;    Trans.arithmetic
          | A.EqOp | A.NeqOp
          | A.LtOp | A.LeOp | A.GtOp | A.GeOp ->
              (match lty with
                INT | NIL                                 -> Trans.compare_int
              | STRING                                    -> Trans.compare_str
              | ARRAY  _ when oper=A.EqOp || oper=A.NeqOp -> Trans.compare_int
              | RECORD _ when oper=A.EqOp || oper=A.NeqOp -> Trans.compare_int
              | _ ->
                  E.type_err pos "Incomparable types"
              )
        in (trans_fn oper lexp rexp, INT)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.CallExp(sym, arglist, pos) ->
        let chk_arg = check_type_eq pos
                      "Argument type (%s) does not match declaration (%s)"
        in 
        (match Env.lookup_value env sym pos with
          Env.FunEntry(lbl, cc, frm, dec_args, return_type) ->
            let args,tys = List.split (List.map (trexp env) arglist) in
            begin try
              List.iter2 chk_arg tys dec_args;
              let rtyp = base_type env return_type in
              (Trans.call (Env.frame env) lbl cc frm args
                      (Env.exn_label env) (is_ptr rtyp), rtyp)
            with Invalid_argument x ->
              E.type_err pos "function arguments do not match declaration"
            end
        | _ ->
            E.type_err pos (S.name sym ^ " is not a function")
        )
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.SeqExp ([],_) -> (Trans.nil, UNIT)
    | A.SeqExp (el,_) ->
        let exprs = List.rev_map (trexp env) el in
        let _,typ = List.hd exprs in
        let exprs = List.rev_map fst exprs in
        (Trans.sequence exprs, typ)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.AssignExp(var, exp, pos) ->
        let exp,ety = trexp env exp in
        let var,vty = trvar env var in
        check_type_eq pos "Cannot assign to type %s from type %s" vty ety;
        (Trans.assign var exp, UNIT)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.IfExp(if', then', else', pos) ->
        let iex,ity = trexp env if' in
        let tex,tty = trexp env then' in
        let eex,ety = 
           match else' with
           | None    -> (Trans.nil, UNIT)
           | Some ex -> trexp env ex
        in
        check_type_int pos "if condition" ity;
        check_type_eq  pos
          "type of then expression (%s) does not match else (%s)" tty ety;
        let typ = base_type env tty in
        (Trans.ifexp iex tex eex (is_ptr typ), typ)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.WhileExp(test, body, pos) ->
        let tex,tty = trexp env test in
        let body_env = Env.new_break_label env in
        let bex,bty = trexp body_env body in
        check_type_int pos "while condition" tty;
        check_type_eq  pos "body of while has type %s, must be %s" bty UNIT;
        (Trans.loop tex bex (Env.break_label body_env), UNIT)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.BreakExp pos ->
        begin
          try (Trans.break (Env.break_label env), UNIT)
          with Not_found -> raise(E.Error(E.Illegal_break, pos))
        end
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.ForExp(sym, lo, hi, body, pos) ->
        let _,loty = trexp env lo in
        let _,hity = trexp env hi in
        check_type_int pos "for lower bound" loty;
        check_type_int pos "for upper bound" hity;
       (*s: [[Semantics.trans.trexp()]] ForExp case, unsugaring for in while *)
        let v            = A.SimpleVar(sym, pos) in
        let ve           = A.VarExp v in
        trexp env 
          (A.LetExp(
               [(A.VarDec(sym,(Some(S.symbol "int")), lo, pos))],
               (A.WhileExp
                  (A.OpExp(ve, A.LeOp, hi, pos),
                   (A.SeqExp([body;
                             (A.AssignExp(v, 
                                A.OpExp(ve, A.PlusOp, (A.IntExp 1), pos), pos))
                             ], pos)),
                   pos)),
               pos))
       (*e: [[Semantics.trans.trexp()]] ForExp case, unsugaring for in while *)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.TryExp(expr, handlers, pos) ->
        let new_env         = Env.new_exn_label env in
        let tryex, tryty    = trexp new_env expr in
        let handler (s,h,p) =
          let ex,ty = trexp env h in
          check_type_unit p "handler" ty;
          (S.uid s, ex)
        in
        check_type_unit pos "try" tryty;
        begin match Env.exn_label new_env with
          None     -> E.internal "no exception label for try block"
        | Some lbl ->
            (Trans.try_block tryex lbl (List.map handler handlers), tryty)
        end
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.RaiseExp(sym, pos) ->
        let exn_id = Env.lookup_exn env sym pos in
        (Trans.raise_exn exn_id, UNIT)
  (*x: [[Semantics.trans.trexp()]] cases *)
    | A.SpawnExp(sym, pos) ->
        (match Env.lookup_value env sym pos with
          Env.FunEntry(lbl, cc, frm, dec_args, return_type) ->
            if dec_args <> []
            then E.type_err pos "spawn function must take zero arguments."
            else (Trans.spawn lbl, INT)
        | _ ->
            E.type_err pos (S.name sym ^ " is not a function")
        )
  (*e: [[Semantics.trans.trexp()]] cases *)
(*e: function Semantics.trans.trexp *)
(*s: function Semantics.trans.trdec *)
and trdec env = function
  (*s: [[Semantics.trans.trdec()]] cases *)
  | A.VarDec(name, typ, init, pos) ->
      let e,t = trexp env init in
      (match typ with
        Some x -> check_type_eq pos
            "Variable of type %s cannot be initialized with type %s"
            (Env.lookup_type env x pos) t
      | None -> ()
      );
      let access = Env.enter_local env name t (is_ptr t) in
      Trans.assign (Trans.simple_var (Env.frame env) access) e
  (*x: [[Semantics.trans.trdec()]] cases *)
  | A.TypeDec types ->
      let penv = Env.new_scope env in
      let real_type (name, typ, _) = 
        (name, 
         match typ with
         (* type expansion *)
         | A.NameTy(name, pos) -> Env.lookup_type penv name pos
         | A.RecordTy(fields) ->
            let chkfld(name,ty,p) = (name,(Env.lookup_type penv ty p))
            in RECORD (List.map chkfld fields)
         | A.ArrayTy(name, pos) -> ARRAY (Env.lookup_type penv name pos)
        )
      in
      types |> List.iter (fun(n,_,_) -> Env.enter_type penv n (NAME n));
      let real_types = (List.map real_type types) in
      real_types |> List.iter (fun (n,t) -> Env.enter_type env n t);
      Trans.nil
  (*x: [[Semantics.trans.trdec()]] cases *)
  | A.FunctionDec functions ->
      (*s: local function Semantics.trans.trdec.mk_param (for FunctionDec case) *)
      let mk_param fenv (name, typ, pos) =
        let t = lookup_base_type env typ pos in
        Env.enter_param fenv name t (is_ptr t)
      in
      (*e: local function Semantics.trans.trdec.mk_param (for FunctionDec case) *)
      (*s: local function Semantics.trans.trdec.mk_func_env (for FunctionDec case) *)
      let mk_func_env (name, params, typ, _, pos) =
        let ret_type = 
          match typ with
          | Some x -> lookup_base_type env x pos
          | None   -> UNIT
        in
        let params_types = params|> List.map (fun(_,t,p)->lookup_base_type env t p) in
        let fenv  = Env.enter_fun env name None(*not C func*) params_types ret_type in
        params |> List.iter (fun p -> mk_param fenv p);
        fenv
      in
      (*e: local function Semantics.trans.trdec.mk_func_env (for FunctionDec case) *)
      (*s: local function Semantics.trans.trdec.trans_func (for FunctionDec case) *)
      let trans_func fenv (_, _, _, body, _) =
        let b = trexp fenv body in
        add_function (Env.frame fenv) b
      in
      (*e: local function Semantics.trans.trdec.trans_func (for FunctionDec case) *)
      let envs = (List.map mk_func_env functions) in
      List.iter2 trans_func   envs functions;
      Trans.nil
  (*x: [[Semantics.trans.trdec()]] cases *)
    | A.ExceptionDec(sym,_) -> Env.enter_exn env sym; Trans.nil
  (*e: [[Semantics.trans.trdec()]] cases *)
(*e: function Semantics.trans.trdec *)
(*s: function Semantics.trans.trvar *)
and trvar env = function
  (*s: [[Semantics.trans.trvar()]] cases *)
      A.SimpleVar(sym, pos) ->
        (match Env.lookup_value env sym pos with
          Env.VarEntry(access, vt) ->
            (Trans.simple_var (Env.frame env) access, base_type env vt)
        | Env.FunEntry _ ->
            E.type_err pos "function used as value"
        )
  (*x: [[Semantics.trans.trvar()]] cases *)
    | A.FieldVar(var, sym, pos) ->
        let (exp, fields) = 
          match trvar env var with
          | (x, RECORD y) -> (x,y)
          | _ -> E.type_err pos "attempt to dereference non-record type"
        in
        let offset  = ref (-1) in
        let (_,fld) =
          try List.find (fun (s,v) -> incr offset; s = sym) fields
          with Not_found -> E.undefined pos (S.name sym)
        in
        let typ = base_type env fld in
        (Trans.field_var exp !offset (is_ptr typ), typ)
  (*x: [[Semantics.trans.trvar()]] cases *)
    | A.SubscriptVar(var, exp, pos) ->
        let e,t = trexp env exp in
        check_type_int pos "subscript variable" t;
        (match trvar env var with
          (exp, ARRAY vt) ->
            let typ = (base_type env vt) in
            (Trans.subscript_var exp e (is_ptr typ) pos, typ)
        | _ ->
            E.type_err pos "attempt to dereference a non-array type"
        )
  (*e: [[Semantics.trans.trvar()]] cases *)
(*e: function Semantics.trans.trvar *)
(*e: functions Semantics.trxxx *)
(*e: translators *)
(*s: function Semantics.translate *)
let translate env ast =
  let (mainex, mainty) = trexp env ast in

  if mainty <> INT && mainty <> UNIT 
  then E.type_err 0 "tiger program must return INT or UNIT";

  (*s: [[Semantics.translate()]] return translated functions *)
  add_function (Env.frame env) (mainex, mainty);
  List.rev !functions
  (*e: [[Semantics.translate()]] return translated functions *)
(*e: function Semantics.translate *)
(*e: semantics.ml *)
(*e: frontend/semantics.ml *)
