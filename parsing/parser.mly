/*(*s: parser.mly *)*/
%{
module E = Error
module A = Ast
module S = Symbol

let getpos = Parsing.symbol_start
let parse_error s =
  let pos = getpos() in
  raise (E.Error(E.Syntax_error, pos))

(* record creation functions *)
let mkField n t           = (n, t, getpos())

let mkSimpleVar v         = A.SimpleVar(v, getpos())
let mkFieldVar v t        = A.FieldVar(v, t, getpos())
let mkSubscriptVar v e    = A.SubscriptVar(v, e, getpos())

let mkNilExp              = A.NilExp
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
%}

/* Tokens */

%token AND ARRAY ASSIGN BREAK COLON COMMA DIVIDE DO DOT
%token ELSE END EOF EQ EXCEPTION FOR FUNCTION GE GT HANDLE IF IN
%token LBRACE LBRACK LE LET LPAREN LT MINUS NEQ NIL
%token OF OR PLUS RAISE RBRACE RBRACK RPAREN SEMICOLON SPAWN
%token TIMES THEN TO TRY TYPE VAR WHILE
%token <int> INT
%token <string> ID STRING

/* Precedences and associativities.
   The precedences must be listed from low to high. */

%nonassoc ASSIGN
%left AND OR
%nonassoc EQ NEQ GT LT GE LE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc UMINUS

/* start symbols */

%start program
%type <Ast.exp> program
%type <Ast.exp> expr
%type <Ast.var> lvalue
%type <Ast.dec list> decs

/* %expect 63 */

%%

program:
  expr EOF { $1 }
;
expr:
   lvalue                    { mkVarExp $1 }
 | sequence                  { mkSeqExp $1 }
 | literal                   { $1 }
 | function_call             { $1 }
 | arithmetic                { $1 }
 | comparison                { $1 }
 | boolean                   { $1 }
 | construction              { $1 }
 | lvalue ASSIGN expr        { mkAssignExp $1 $3 }
 | if_statement              { $1 }
 | loop_statement            { $1 }
 | LET decs IN expr_list END { mkLetExp $2 (mkSeqExp $4) }
 | TRY expr handlers         { mkTryExp $2 (List.rev $3) }
 | RAISE id                  { mkRaise $2 }
 | SPAWN id                  { mkSpawn $2 }
;

/* Symbols are created in this rule only */
id: ID { S.symbol $1 };

/* Variables (L-values) 
   This rule is overly explicit to avoid conflicts with 
   the construction rule below */
lvalue:
 | id                        { mkSimpleVar $1 }
 | id DOT id                 { mkFieldVar (mkSimpleVar $1) $3 }
 | id LBRACK expr RBRACK     { mkSubscriptVar (mkSimpleVar $1) $3 }
 | lvalue DOT id             { mkFieldVar $1 $3 }
 | lvalue LBRACK expr RBRACK { mkSubscriptVar $1 $3 }
;

/* Sequence expression */
sequence:
   LPAREN RPAREN           { [] }
 | LPAREN expr_list RPAREN { $2 }
;
expr_list:
   expr                     { $1 :: [] }
 | expr SEMICOLON expr_list { $1 :: $3 }
;

/* Literals */
literal:
   NIL    { mkNilExp }
 | INT    { mkIntExp $1 }
 | STRING { mkStringExp $1 }
;

/* function call */
function_call:
   id LPAREN fun_args RPAREN { mkCallExp $1 $3 }
;
fun_args:
   /* empty */         { [] }
 | expr                { $1 :: [] }
 | expr COMMA fun_args { $1 :: $3 }
;

/* Simple Arithmetic */
arithmetic:
   MINUS expr %prec UMINUS { mkOpExp (mkIntExp 0) $2 A.MinusOp }
 | expr PLUS expr          { mkOpExp $1 $3 A.PlusOp    }
 | expr MINUS expr         { mkOpExp $1 $3 A.MinusOp  }
 | expr TIMES expr         { mkOpExp $1 $3 A.TimesOp  }
 | expr DIVIDE expr        { mkOpExp $1 $3 A.DivideOp }
;

/* Comparison */
comparison:
   expr EQ expr  { mkOpExp $1 $3 A.EqOp  }
 | expr NEQ expr { mkOpExp $1 $3 A.NeqOp }
 | expr GT expr  { mkOpExp $1 $3 A.GtOp  }
 | expr LT expr  { mkOpExp $1 $3 A.LtOp  }
 | expr GE expr  { mkOpExp $1 $3 A.GeOp  }
 | expr LE expr  { mkOpExp $1 $3 A.LeOp  }
;

/* Boolean operators */
boolean:
   expr AND expr { mkIfExp $1 $3 (Some(mkIntExp 0)) }
 | expr OR expr  { mkIfExp $1 (mkIntExp 1) (Some $3) }
;

/* Record and array construction */
construction:
   id LBRACE ctor_list RBRACE    { mkRecExp $1 $3 }
 | id LBRACK expr RBRACK OF expr { mkArrayExp $1 $3 $6 }
;
ctor_list:
   id EQ expr                 { (mkRecFld $1 $3) :: [] }
 | id EQ expr COMMA ctor_list { (mkRecFld $1 $3) :: $5 }
;

/* If statements */
if_statement:
   IF expr THEN expr           { mkIfExp $2 $4 None }
 | IF expr THEN expr ELSE expr { mkIfExp $2 $4 (Some $6) }
;

/* Loop statements */
loop_statement:
   WHILE expr DO expr                 { mkWhileExp $2 $4 }
 | FOR id ASSIGN expr TO expr DO expr { mkForExp $2 $4 $6 $8 }
 | BREAK                              { mkBreakExp }
;

/* exception handlers */
handler:
   HANDLE id expr END         { mkHandler $2 $3 }
;
handlers:
   handler          { $1 :: [] }
 | handler handlers { $1 :: $2 }
;

/* Declarations */

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
;
dec:
   var_dec   { $1 }
 | type_decs { mkTypeDec $1 }
 | fun_decs  { mkFunctionDec $1 }
 | exn_dec   { $1 }
;
var_dec:
   VAR id ASSIGN expr          { mkVarDec $2 None $4 }
 | VAR id COLON id ASSIGN expr { mkVarDec $2 (Some $4) $6 }
;
type_decs:
   type_dec           { $1 :: [] }
 | type_dec type_decs { $1 :: $2 }
;
type_dec:
   TYPE id EQ ty { mkTyDec $2 $4 }
;
ty:
   id                      { mkNameTy $1 }
 | LBRACE ty_fields RBRACE { mkRecordTy $2 }
 | ARRAY OF id             { mkArrayTy $3 }
;
ty_fields:
   /* empty */                 { [] }
 | id COLON id                 { (mkField $1 $3) :: [] }
 | id COLON id COMMA ty_fields { (mkField $1 $3) :: $5 }
;
fun_decs:
   fun_dec          { $1 :: [] }
 | fun_dec fun_decs { $1 :: $2 }
;
fun_dec:
   FUNCTION id LPAREN ty_fields RPAREN EQ expr
     { mkFunDec $2 $4 None $7 }
 | FUNCTION id LPAREN ty_fields RPAREN COLON id EQ expr
     { mkFunDec $2 $4 (Some $7) $9 }
;
exn_dec:
   EXCEPTION id { mkException $2 }
;
/*(*e: parser.mly *)*/
