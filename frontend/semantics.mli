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
# 22 "semantics.nw"
val translate : vartype Environment.t -> Ast.exp ->
               (Frame.frame * Translate.exp) list
