(*s: parsing/ast.ml *)
(*s: ast.ml *)
(*s: type Ast.pos *)
type pos = int 
(*e: type Ast.pos *)
(*s: type Ast.symbol *)
type symbol = Symbol.symbol
(*e: type Ast.symbol *)

(*s: type Ast.dec *)
type dec =
| VarDec       of symbol * symbol option * exp * pos
| TypeDec      of (symbol * ty * pos) list
| FunctionDec  of (symbol * field list * symbol option * exp * pos) list
(*s: [[Ast.dec]] cases *)
| ExceptionDec of symbol * pos
(*e: [[Ast.dec]] cases *)
(*e: type Ast.dec *)
(*s: type Ast.ty *)
and ty =
| NameTy   of symbol * pos
| RecordTy of field list
| ArrayTy  of symbol * pos
(*e: type Ast.ty *)
(*s: type Ast.field *)
and field = (symbol * symbol * pos)
(*e: type Ast.field *)
(*s: type Ast.var *)
and var =
    SimpleVar    of symbol * pos
  | FieldVar     of var * symbol * pos
  | SubscriptVar of var * exp * pos
(*e: type Ast.var *)
(*s: type Ast.exp *)
and exp =
| NilExp
| IntExp    of int
| StringExp of string * pos

| VarExp    of var

| OpExp     of exp * oper * exp * pos
| CallExp   of symbol * exp list * pos

| RecordExp of symbol * (symbol * exp * pos) list * pos
| ArrayExp  of symbol * exp * exp * pos

| LetExp    of dec list * exp * pos

(*s: [[Ast.exp]] statement cases *)
| AssignExp of var * exp * pos
| SeqExp    of exp list * pos
| IfExp     of exp * exp * exp option * pos
| WhileExp  of exp * exp * pos
| ForExp    of symbol * exp * exp * exp * pos
| BreakExp  of pos
(*e: [[Ast.exp]] statement cases *)
(*s: [[Ast.exp]] cases *)
| TryExp    of exp * (symbol * exp * pos) list * pos
| RaiseExp  of symbol * pos
(*x: [[Ast.exp]] cases *)
| SpawnExp  of symbol * pos
(*e: [[Ast.exp]] cases *)
(*e: type Ast.exp *)
(*s: type Ast.oper *)
and oper =
(*s: [[Ast.oper]] cases *)
| PlusOp | MinusOp | TimesOp | DivideOp
(*x: [[Ast.oper]] cases *)
| EqOp | NeqOp   
| LtOp | LeOp | GtOp | GeOp
(*e: [[Ast.oper]] cases *)
(*e: type Ast.oper *)

(*s: tree printer *)
module S = Symbol

(*s: function iprintf *)
let iprintf d fmt =
  let rec indent = function
      0 -> ()
    | i -> (print_string "  "; indent(i-1))
  in (indent d; Printf.printf fmt)
(*e: function iprintf *)

(*s: function Ast.opname *)
let opname = function
    PlusOp   -> "PlusOp"
  | MinusOp  -> "MinusOp"
  | TimesOp  -> "TimesOp"
  | DivideOp -> "DivideOp"
  | EqOp     -> "EqOp"
  | NeqOp    -> "NeqOp"
  | LtOp     -> "LtOp"
  | LeOp     -> "LeOp"
  | GtOp     -> "GtOp"
  | GeOp     -> "GeOp"
(*e: function Ast.opname *)
(*x: tree printer *)
(*s: function Ast.print_tree *)
let print_tree expression =
  (*s: declaration printer *)
  let rec dec d =
    let print_opt_sym d = function
        None   -> iprintf (d+1) ": NONE\n"
      | Some s -> iprintf (d+1) ": SOME(%s)\n" (S.name s)
    in
    function
      FunctionDec functions ->
        let prfield d (n,t,_) =
          iprintf d "%s:%s\n" (S.name n) (S.name t)
        in
        let prfun d (name, params, type', body, _) =
   iprintf d "%s:\n" (S.name name);
          List.iter (prfield (d+1)) params;
          print_opt_sym (d+1) type';
   exp (d+2) body
        in
        iprintf d "FunctionDec:\n";
        List.iter (prfun (d+1)) functions
    | VarDec(name, type', init,_) ->
        iprintf d "VarDec: %s\n" (S.name name);
        print_opt_sym (d+1) type';
        exp (d+1) init
    | TypeDec types ->
        let prtdec d (name, type',_) =
          iprintf d "%s:\n" (S.name name); ty (d+1) type'
        in
        iprintf d "TypeDec:\n";
        List.iter (prtdec (d+1)) types
    | ExceptionDec(s,_) ->
        iprintf d "ExceptionDec:%s\n" (S.name s)
  (*e: declaration printer *)
  (*s: type printer *)
  and ty d = function
      NameTy(s,_)     -> iprintf d "NameTy : %s\n" (S.name s)
    | ArrayTy(s,_)    -> iprintf d "ArrayTy: %s\n" (S.name s)
    | RecordTy fields ->
        let f d (n,t,_) =
          iprintf d "%s:%s\n" (S.name n) (S.name t)
        in
        iprintf d "RecordTy:\n";
        List.iter (f (d+1)) fields
  (*e: type printer *)
  (*s: variable printer *)
  and var d = function
      SimpleVar(s,_)      -> iprintf d "SimpleVar: %s\n" (S.name s)
    | FieldVar(v,s,_)     -> iprintf d "FieldVar:\n";
                             var (d+1) v;
                             iprintf (d+1) "%s\n" (S.name s)
    | SubscriptVar(v,e,_) -> iprintf d "SubscriptVar:\n";
                             var (d+1) v;
                             exp (d+1) e
  (*e: variable printer *)
  (*s: expression printer *)
  and exp d = function
      VarExp v       -> var d v
    | NilExp         -> iprintf d "NilExp\n"
    | IntExp i       -> iprintf d "IntExp: %d\n" i
    | StringExp(s,_) -> iprintf d "StringExp:%s\n" (String.escaped s)
    | RecordExp(name, fields, _) ->
        let f d (n,e,_) =
          (iprintf d "%s:\n" (S.name n); exp (d+1) e)
        in
        iprintf d "RecordExp: %s\n" (S.name name);
        List.iter (f (d+1)) fields
    | ArrayExp(v, size, init, p) ->
        iprintf d "ArrayExp: %s\n" (S.name v);
        exp (d+1) size;
        exp (d+1) init
    | AssignExp(v, e, _) ->
        iprintf d "AssignExp:\n";
        var (d+1) v;
        exp (d+1) e
    | OpExp(left, oper, right, _) -> 
        iprintf d "OpExp:%s\n" (opname oper);
        exp (d+1) left;
        exp (d+1) right
    | CallExp(name, args, _) ->
        iprintf d "CallExp: %s\n" (S.name name);
        List.iter (exp (d+1)) args
    | IfExp(if', then', else', _) ->
        iprintf d "IfExp:\n";
        exp (d+1) if';
        exp (d+1) then';
        begin match else' with
          None   -> ()
        | Some a -> exp (d+1) a
        end
    | WhileExp(test, body, _) ->
        iprintf d "WhileExp:\n";
        exp (d+1) test;
        exp (d+1) body
    | ForExp(var, lo, hi, body, _) ->
        iprintf d "ForExp: %s\n" (S.name var);
        exp (d+1) lo;
        exp (d+1) hi;
        exp (d+1) body
    | BreakExp _ ->
        iprintf d "BreakExp\n"
    | SeqExp(l, _) ->
        iprintf d "SeqExp:\n"; List.iter (exp (d+1)) l
    | LetExp(decs, body, _) ->
        iprintf d "LetExp:\n";
        List.iter (dec (d+1)) decs;
        iprintf d "IN:\n";
        exp (d+2) body
    | TryExp(expr, handlers, _) ->
        iprintf d "TryExp:\n";
        exp (d+1) expr;
        List.iter
          (fun (n,h,_) -> iprintf (d+2) "%s:\n" (S.name n); exp (d+2) h)
          handlers
    | RaiseExp(name,_) ->
        iprintf d "RaiseExp %s\n" (S.name name)
    | SpawnExp(name,_) ->
        iprintf d "SpawnExp %s\n" (S.name name)
  (*e: expression printer *)
in exp 0 expression
(*e: function Ast.print_tree *)
(*e: tree printer *)
(*e: ast.ml *)
(*e: parsing/ast.ml *)
