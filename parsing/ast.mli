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
| VarDec       of symbol * symbol option * exp * pos
| TypeDec      of (symbol * ty * pos) list
| FunctionDec  of (symbol * field list * symbol option * exp * pos) list
(*s: [[Ast.dec]] cases *)
| ExceptionDec of symbol * pos
(*e: [[Ast.dec]] cases *)
(*e: type Ast.dec *)
(*x: types(ast.nw) *)
(*s: type Ast.ty *)
and ty =
| NameTy   of symbol * pos
| RecordTy of field list
| ArrayTy  of symbol * pos
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
(*x: types(ast.nw) *)
(*s: type Ast.oper *)
and oper =
(*s: [[Ast.oper]] cases *)
| PlusOp | MinusOp | TimesOp | DivideOp
(*x: [[Ast.oper]] cases *)
| EqOp | NeqOp   
| LtOp | LeOp | GtOp | GeOp
(*e: [[Ast.oper]] cases *)
(*e: type Ast.oper *)
(*e: types(ast.nw) *)

val print_tree : exp -> unit
(*e: ast.mli *)
(*e: parsing/ast.mli *)
