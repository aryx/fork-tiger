/*(*s: parser.mly *)*/
%{
module E = Error
module A = Ast
module S = Symbol

/*(*s: parser helper functions *)*/
let getpos = Parsing.symbol_start
/*(*x: parser helper functions *)*/
let parse_error s =
  let pos = getpos() in
  raise (E.Error(E.Syntax_error, pos))
/*(*e: parser helper functions *)*/
/*(*s: AST mk wrappers *)*/
let mkField n t           = (n, t, getpos())

let mkSimpleVar v         = A.SimpleVar(v, getpos())
let mkFieldVar v t        = A.FieldVar(v, t, getpos())
let mkSubscriptVar v e    = A.SubscriptVar(v, e, getpos())

let mkVarExp v            = A.VarExp(v)
let mkIntExp i            = A.IntExp(i)
let mkStringExp s         = A.StringExp(s, getpos())
let mkCallExp f a         = A.CallExp(f, a, getpos())
let mkOpExp l r op        = A.OpExp(l, op, r, getpos())
let mkRecFld n e          = (n, e, getpos())
let mkRecExp n f          = A.RecordExp(n, f, getpos())
let mkSeqExp el           = A.SeqExp(el, getpos())
let mkAssignExp v e       = A.AssignExp(v, e, getpos())
let mkIfExp tst t e       = A.IfExp(tst, t, e, getpos())
let mkWhileExp tst b      = A.WhileExp(tst, b, getpos())
let mkForExp v lo hi body = A.ForExp(v, lo, hi, body, getpos())
let mkBreakExp            = A.BreakExp(getpos())
let mkLetExp decs body    = A.LetExp(decs, body, getpos())
let mkArrayExp v s init   = A.ArrayExp(v, s, init, getpos())

let mkHandler name exp    = (name, exp, getpos())
let mkTryExp exp handlers = A.TryExp(exp, handlers, getpos())
let mkRaise id            = A.RaiseExp(id, getpos())

let mkSpawn id            = A.SpawnExp(id, getpos())

let mkFunDec n f t b      = (n, f, t, b, getpos())
let mkFunctionDec l       = A.FunctionDec(l)
let mkVarDec n t i        = A.VarDec(n, t, i, getpos())
let mkTyDec n t           = (n, t, getpos())
let mkTypeDec l           = A.TypeDec(l)
let mkNameTy n            = A.NameTy(n, getpos())
let mkRecordTy l          = A.RecordTy(l)
let mkArrayTy n           = A.ArrayTy(n, getpos())
let mkException s         = A.ExceptionDec(s, getpos())
/*(*e: AST mk wrappers *)*/
%}

/* Tokens */
/*(*s: token declarations *)*/
%token <string> ID
%token <int> INT
%token <string> STRING
%token IF THEN ELSE END  WHILE DO FOR TO BREAK
%token PLUS MINUS TIMES DIVIDE
%token AND OR
%token EQ NEQ  LT LE GT GE
%token FUNCTION VAR LET IN
%token TYPE OF ARRAY
%token ASSIGN COLON COMMA DOT SEMICOLON 
%token LPAREN RPAREN  LBRACE RBRACE  LBRACK RBRACK
%token NIL
%token EOF
/*(*x: token declarations *)*/
%token EXCEPTION  TRY RAISE HANDLE
/*(*x: token declarations *)*/
%token SPAWN
/*(*e: token declarations *)*/

/* Precedences and associativities (from low to high) */
/*(*s: token priorities *)*/
%nonassoc ASSIGN
%left AND OR
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS
/*(*e: token priorities *)*/

/* start symbols */
%start program
/*(*s: rule type declarations *)*/
%type <Ast.exp> program
%type <Ast.exp> expr
%type <Ast.var> lvalue
%type <Ast.dec list> decs
/*(*e: rule type declarations *)*/

/* %expect 63 */

%%
/*(*s: grammar *)*/
/*(*s: rule program *)*/
program:
  expr EOF { $1 }
/*(*e: rule program *)*/

/* Expressions */
/*(*s: rule expr *)*/
expr:
  LET decs IN expr_list END { mkLetExp $2 (mkSeqExp $4) }
/*(*x: rule expr *)*/
 | literal                   { $1 }
 | lvalue                    { mkVarExp $1 }
 | function_call             { $1 }
 | arithmetic                { $1 }
 | comparison                { $1 }
 | boolean                   { $1 }
 | construction              { $1 }
/*(*x: rule expr *)*/
 | sequence                  { mkSeqExp $1 }
 | if_statement              { $1 }
 | loop_statement            { $1 }
/*(*x: rule expr *)*/
 | lvalue ASSIGN expr        { mkAssignExp $1 $3 }
/*(*x: rule expr *)*/
 | TRY expr handlers         { mkTryExp $2 (List.rev $3) }
 | RAISE id                  { mkRaise $2 }
/*(*x: rule expr *)*/
 | SPAWN id                  { mkSpawn $2 }
/*(*e: rule expr *)*/
/*(*s: rule lvalue *)*/
/* Variables (L-values) 
   This rule is overly explicit to avoid conflicts with 
   the construction rule below */
lvalue:
 | id                        { mkSimpleVar $1 }
 | id LBRACK expr RBRACK     { mkSubscriptVar (mkSimpleVar $1) $3 }

 | lvalue DOT id             { mkFieldVar $1 $3 }
 | lvalue LBRACK expr RBRACK { mkSubscriptVar $1 $3 }
/*(*e: rule lvalue *)*/
/*(*s: subrules for expr *)*/
/* Literals */
literal:
   NIL    { A.NilExp }
 | INT    { mkIntExp $1 }
 | STRING { mkStringExp $1 }
/*(*x: subrules for expr *)*/
/* Simple Arithmetic */
arithmetic:
   MINUS expr %prec UMINUS { mkOpExp (mkIntExp 0) $2 A.MinusOp }
 | expr PLUS expr          { mkOpExp $1 $3 A.PlusOp    }
 | expr MINUS expr         { mkOpExp $1 $3 A.MinusOp  }
 | expr TIMES expr         { mkOpExp $1 $3 A.TimesOp  }
 | expr DIVIDE expr        { mkOpExp $1 $3 A.DivideOp }
/*(*x: subrules for expr *)*/
/* Comparison */
comparison:
   expr EQ expr  { mkOpExp $1 $3 A.EqOp  }
 | expr NEQ expr { mkOpExp $1 $3 A.NeqOp }
 | expr GT expr  { mkOpExp $1 $3 A.GtOp  }
 | expr LT expr  { mkOpExp $1 $3 A.LtOp  }
 | expr GE expr  { mkOpExp $1 $3 A.GeOp  }
 | expr LE expr  { mkOpExp $1 $3 A.LeOp  }
/*(*x: subrules for expr *)*/
/* Boolean operators */
boolean:
   expr AND expr { mkIfExp $1 $3 (Some(mkIntExp 0)) }
 | expr OR expr  { mkIfExp $1 (mkIntExp 1) (Some $3) }
/*(*x: subrules for expr *)*/
/* function call */
function_call:
   id LPAREN fun_args RPAREN { mkCallExp $1 $3 }

fun_args:
   /* empty */         { [] }
 | expr                { $1 :: [] }
 | expr COMMA fun_args { $1 :: $3 }
/*(*x: subrules for expr *)*/
/* Record and array construction */
construction:
   id LBRACE ctor_list RBRACE    { mkRecExp $1 $3 }
 | id LBRACK expr RBRACK OF expr { mkArrayExp $1 $3 $6 }

ctor_list:
   id EQ expr                 { (mkRecFld $1 $3) :: [] }
 | id EQ expr COMMA ctor_list { (mkRecFld $1 $3) :: $5 }
/*(*e: subrules for expr *)*/
/*(*s: subrules for stmt *)*/
/* Sequence expression */
sequence:
   LPAREN RPAREN           { [] }
 | LPAREN expr_list RPAREN { $2 }
/*(*x: subrules for stmt *)*/
expr_list:
   expr                     { $1 :: [] }
 | expr SEMICOLON expr_list { $1 :: $3 }
/*(*x: subrules for stmt *)*/
/* If statements */
if_statement:
   IF expr THEN expr           { mkIfExp $2 $4 None }
 | IF expr THEN expr ELSE expr { mkIfExp $2 $4 (Some $6) }
/*(*x: subrules for stmt *)*/
/* Loop statements */
loop_statement:
   WHILE expr DO expr                 { mkWhileExp $2 $4 }
 | FOR id ASSIGN expr TO expr DO expr { mkForExp $2 $4 $6 $8 }
 | BREAK                              { mkBreakExp }
/*(*x: subrules for stmt *)*/
/* exception handlers */
handler:
   HANDLE id expr END         { mkHandler $2 $3 }

handlers:
   handler          { $1 :: [] }
 | handler handlers { $1 :: $2 }
/*(*e: subrules for stmt *)*/

/* Declarations */
/*(*s: rule decs *)*/
/* recursive declarations in tiger.
   My interpretation of the tiger language spec is that
   mutally recursive types and functions are valid if they
   are declared together in a sequence. That is:
   type a = {b:int c:d} type d = a
   is valid where as 
   type a = {b:int c:d} var x := 1 type d = a
   is not.
*/
decs:
   dec { $1 :: [] }
 | dec decs { $1 :: $2 }
/*(*e: rule decs *)*/
/*(*s: subrules for decs *)*/
/*(*s: rule dec *)*/
dec:
   var_dec   { $1 }
 | type_decs { mkTypeDec $1 }
 | fun_decs  { mkFunctionDec $1 }
 | exn_dec   { $1 }
/*(*e: rule dec *)*/
/*(*s: rule var_dec *)*/
var_dec:
   VAR id ASSIGN expr          { mkVarDec $2 None $4 }
 | VAR id COLON id ASSIGN expr { mkVarDec $2 (Some $4) $6 }
/*(*e: rule var_dec *)*/
/*(*s: rule type_decs *)*/
type_decs:
   type_dec           { $1 :: [] }
 | type_dec type_decs { $1 :: $2 }
;
type_dec:
   TYPE id EQ ty { mkTyDec $2 $4 }
;
/*(*e: rule type_decs *)*/
/*(*s: rule fun_decs *)*/
fun_decs:
   fun_dec          { $1 :: [] }
 | fun_dec fun_decs { $1 :: $2 }

fun_dec:
   FUNCTION id LPAREN ty_fields RPAREN EQ expr
     { mkFunDec $2 $4 None $7 }
 | FUNCTION id LPAREN ty_fields RPAREN COLON id EQ expr
     { mkFunDec $2 $4 (Some $7) $9 }
/*(*e: rule fun_decs *)*/
/*(*s: rule exn_dec *)*/
exn_dec:
   EXCEPTION id { mkException $2 }
/*(*e: rule exn_dec *)*/
/*(*e: subrules for decs *)*/

/* Types */
/*(*s: rule ty *)*/
ty:
   id                      { mkNameTy $1 }
 | LBRACE ty_fields RBRACE { mkRecordTy $2 }
 | ARRAY OF id             { mkArrayTy $3 }

ty_fields:
   /* empty */                 { [] }
 | id COLON id                 { (mkField $1 $3) :: [] }
 | id COLON id COMMA ty_fields { (mkField $1 $3) :: $5 }
/*(*e: rule ty *)*/

/* Names */
/*(*s: rule id *)*/
/* Symbols are created in this rule only */
id: ID { S.symbol $1 };
/*(*e: rule id *)*/
/*(*e: grammar *)*/
/*(*e: parser.mly *)*/
