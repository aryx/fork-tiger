(*s: parsing/ast.mli *)
(*s: ast.mli *)
(*s: types(ast.nw) *)
(*s: type Ast.pos *)
type pos = int 
(*e: type Ast.pos *)
(*s: type Ast.symbol *)
type symbol = Symbol.symbol
(*e: type Ast.symbol *)
(*x: types(ast.nw) *)
(*s: type Ast.dec *)
type dec =
(*s: type Ast.dec cases *)
| VarDec       of symbol * symbol option * exp * pos
(*x: type Ast.dec cases *)
| TypeDec      of (symbol * ty * pos) list
(*x: type Ast.dec cases *)
| FunctionDec  of (symbol * field list * symbol option * exp * pos) list
(*x: type Ast.dec cases *)
| ExceptionDec of symbol * pos
(*e: type Ast.dec cases *)
(*e: type Ast.dec *)
(*x: types(ast.nw) *)
(*s: type Ast.ty *)
and ty =
(*s: type Ast.ty cases *)
| NameTy   of symbol * pos
| RecordTy of field list
| ArrayTy  of symbol * pos
(*e: type Ast.ty cases *)
(*e: type Ast.ty *)
(*x: types(ast.nw) *)
(*s: type Ast.field *)
and field = (symbol * symbol * pos)
(*e: type Ast.field *)
(*x: types(ast.nw) *)
(*s: type Ast.var *)
and var =
    SimpleVar    of symbol * pos
  | FieldVar     of var * symbol * pos
  | SubscriptVar of var * exp * pos
(*e: type Ast.var *)
(*x: types(ast.nw) *)
(*s: type Ast.exp *)
and exp =
(*s: type Ast.exp cases *)
| LetExp    of dec list * exp * pos
(*x: type Ast.exp cases *)
| VarExp    of var
(*x: type Ast.exp cases *)
| NilExp
| IntExp    of int
| StringExp of string * pos
(*x: type Ast.exp cases *)
| OpExp     of exp * oper * exp * pos
(*x: type Ast.exp cases *)
| CallExp   of symbol * exp list * pos
(*x: type Ast.exp cases *)
| RecordExp of symbol * (symbol * exp * pos) list * pos
| ArrayExp  of symbol * exp * exp * pos
(*x: type Ast.exp cases *)
| SeqExp    of exp list * pos
(*x: type Ast.exp cases *)
| AssignExp of var * exp * pos
(*x: type Ast.exp cases *)
| IfExp     of exp * exp * exp option * pos
(*x: type Ast.exp cases *)
| WhileExp  of exp * exp * pos
| ForExp    of symbol * exp * exp * exp * pos

| BreakExp  of pos
(*x: type Ast.exp cases *)
| TryExp    of exp * (symbol * exp * pos) list * pos
| RaiseExp  of symbol * pos
(*x: type Ast.exp cases *)
| SpawnExp  of symbol * pos
(*e: type Ast.exp cases *)
(*e: type Ast.exp *)
(*x: types(ast.nw) *)
(*s: type Ast.oper *)
and oper =
(*s: type Ast.oper cases *)
| PlusOp | MinusOp | TimesOp | DivideOp
(*x: type Ast.oper cases *)
| EqOp | NeqOp   
| LtOp | LeOp | GtOp | GeOp
(*e: type Ast.oper cases *)
(*e: type Ast.oper *)
(*e: types(ast.nw) *)

val print_tree : exp -> unit
(*e: ast.mli *)
(*e: parsing/ast.mli *)
