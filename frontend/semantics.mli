(*s: frontend/semantics.mli *)
(*s: semantics.mli *)
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
val translate : vartype Environment.t -> Ast.exp ->
               (Frame.frame * Translate.exp) list
(*e: semantics.mli *)
(*e: frontend/semantics.mli *)
