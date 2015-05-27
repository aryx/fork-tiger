(*s: parsing/ast.mli *)
(*s: ast.mli *)
(*s: type Ast.pos *)
type pos = int 
(*e: type Ast.pos *)
(*s: type Ast.name *)
type name = Symbol.symbol
(*e: type Ast.name *)
(*s: type Ast.typename *)
type typename = Symbol.symbol
(*e: type Ast.typename *)

(*s: type Ast.dec *)
type dec =
| VarDec       of name * typename option * exp * pos
| TypeDec      of (typename * ty * pos) list
| FunctionDec  of (name * field list * typename option * exp * pos) list
(*s: [[Ast.dec]] cases *)
| ExceptionDec of name * pos
(*e: [[Ast.dec]] cases *)
(*e: type Ast.dec *)
(*s: type Ast.ty *)
and ty =
| NameTy   of typename * pos
| RecordTy of field list
| ArrayTy  of typename * pos
(*e: type Ast.ty *)
(*s: type Ast.field *)
and field = (name * typename * pos)
(*e: type Ast.field *)
(*s: type Ast.var *)
and var =
    SimpleVar    of name * pos
  | FieldVar     of var * name * pos
  | SubscriptVar of var * exp * pos
(*e: type Ast.var *)
(*s: type Ast.exp *)
and exp =
| NilExp
| IntExp    of int
| StringExp of string * pos

| VarExp    of var

| OpExp     of exp * oper * exp * pos
| CallExp   of name * exp list * pos

| RecordExp of name * (name * exp * pos) list * pos
| ArrayExp  of name * exp * exp * pos

| LetExp    of dec list * exp * pos

(*s: [[Ast.exp]] statement cases *)
| AssignExp of var * exp * pos
| SeqExp    of exp list * pos
| IfExp     of exp * exp * exp option * pos
| WhileExp  of exp * exp * pos
| ForExp    of name * exp * exp * exp * pos
| BreakExp  of pos
(*e: [[Ast.exp]] statement cases *)
(*s: [[Ast.exp]] cases *)
| TryExp    of exp * (name * exp * pos) list * pos
| RaiseExp  of name * pos
(*x: [[Ast.exp]] cases *)
| SpawnExp  of name * pos
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

val print_tree : exp -> unit
(*e: ast.mli *)
(*e: parsing/ast.mli *)
