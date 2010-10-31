type token =
  | AND
  | ARRAY
  | ASSIGN
  | BREAK
  | COLON
  | COMMA
  | DIVIDE
  | DO
  | DOT
  | ELSE
  | END
  | EOF
  | EQ
  | EXCEPTION
  | FOR
  | FUNCTION
  | GE
  | GT
  | HANDLE
  | IF
  | IN
  | LBRACE
  | LBRACK
  | LE
  | LET
  | LPAREN
  | LT
  | MINUS
  | NEQ
  | NIL
  | OF
  | OR
  | PLUS
  | RAISE
  | RBRACE
  | RBRACK
  | RPAREN
  | SEMICOLON
  | SPAWN
  | TIMES
  | THEN
  | TO
  | TRY
  | TYPE
  | VAR
  | WHILE
  | INT of (int)
  | ID of (string)
  | STRING of (string)

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.exp
