(*s: lexer.mll *)
{
module E = Error
module P = Parser

(*s: global Lexer.keyword_table *)
(* The table of keywords *)
let keyword_table = Hashtbl.create 22;;
List.iter (fun (key, data) -> Hashtbl.add keyword_table key data)
  [
   "if",        P.IF;
   "then",      P.THEN;
   "else",      P.ELSE;
   "while",     P.WHILE;
   "do",        P.DO;
   "for",       P.FOR;
   "to",        P.TO;
   "end",       P.END;
   "break",     P.BREAK;

   "function",  P.FUNCTION;
   "var",       P.VAR;
   "let",       P.LET;
   "in",        P.IN;

   "type",      P.TYPE;
   "of",        P.OF;
   "array",     P.ARRAY;

   "and",       P.AND;
   "or",        P.OR;

   "nil",       P.NIL;

   (*s: [[Lexer.keyword_table]] entries *)
   "exception", P.EXCEPTION;

   "try",       P.TRY;
   "handle",    P.HANDLE;
   "raise",     P.RAISE;
   (*x: [[Lexer.keyword_table]] entries *)
   "spawn",     P.SPAWN;
   (*e: [[Lexer.keyword_table]] entries *)
 ]
(*e: global Lexer.keyword_table *)
(*s: function Lexer.escape *)
(* To buffer string literals *)
let escape c = 
  match c with
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | _ -> c
(*e: function Lexer.escape *)
(*s: Lexer globals *)
let line_num = ref 0
(*x: Lexer globals *)
let comment_pos = Stack.create()
(*x: Lexer globals *)
let string_start_pos = ref 0
let buffer = Buffer.create 30
(*e: Lexer globals *)
}

(*s: Lexer aliases *)
let nl = ['\010' '\013']
let blank = [' ' '\009' '\012']
(*x: Lexer aliases *)
let letter = ['A'-'Z' 'a'-'z']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']
(*x: Lexer aliases *)
let number = ['0'-'9']
(*e: Lexer aliases *)

(*s: rule token *)
rule token = parse
  (*s: [[Lexer.token]] cases *)
  |  nl   { incr line_num;
           E.add_source_mapping (Lexing.lexeme_end lexbuf) !line_num;
           token lexbuf }
  | blank + { token lexbuf }
  (*x: [[Lexer.token]] cases *)
  | "/*" { comment lexbuf; token lexbuf }
  (*x: [[Lexer.token]] cases *)
  | letter identchar *
      { let s = Lexing.lexeme lexbuf in
        try
          Hashtbl.find keyword_table s
        with Not_found ->
          P.ID s 
      }
  (*x: [[Lexer.token]] cases *)
  | "+"  { P.PLUS }
  | "-"  { P.MINUS }
  | "*"  { P.TIMES }
  | "/"  { P.DIVIDE }

  | "&"  { P.AND }
  | "|"  { P.OR }

  | "="  { P.EQ }
  | "<>" { P.NEQ }

  | ">"  { P.GT }
  | "<"  { P.LT }
  | ">=" { P.GE }
  | "<=" { P.LE }
  (*x: [[Lexer.token]] cases *)
  | ":=" { P.ASSIGN }
  | ":"  { P.COLON }
  | ","  { P.COMMA }
  | "."  { P.DOT }
  | ";"  { P.SEMICOLON }

  | "{"  { P.LBRACE } | "}"  { P.RBRACE }
  | "["  { P.LBRACK } | "]"  { P.RBRACK }
  | "("  { P.LPAREN } | ")"  { P.RPAREN }
  (*x: [[Lexer.token]] cases *)
  | number +
      { P.INT (int_of_string(Lexing.lexeme lexbuf)) }
  (*x: [[Lexer.token]] cases *)
  | "\"" 
      { string_start_pos := Lexing.lexeme_start lexbuf;
        Buffer.clear buffer;
        P.STRING (string lexbuf) }
  (*e: [[Lexer.token]] cases *)
  | eof  { P.EOF }
  | _
      { raise (E.Error(E.Illegal_character (Lexing.lexeme_char lexbuf 0),
                       Lexing.lexeme_start lexbuf)) }
(*e: rule token *)
(*s: rule comment *)
and comment = parse
    "/*" { Stack.push (Lexing.lexeme_start lexbuf) comment_pos;
           comment lexbuf; }
  | "*/" { try (ignore(Stack.pop comment_pos); comment lexbuf)
           with Stack.Empty -> () }
  | nl   { incr line_num;
           E.add_source_mapping (Lexing.lexeme_end lexbuf) !line_num;
           comment lexbuf }
  | eof  { let st = Stack.top comment_pos in
           raise (E.Error(E.Unterminated_comment, st)) }
  | _    { comment lexbuf }
(*e: rule comment *)
(*s: rule string *)
and string = parse
    '"'
      { Buffer.contents buffer }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { Buffer.add_char buffer (escape (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | [^ '"' '\\'] +
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        string lexbuf }
  | eof
      { raise (E.Error(E.Unterminated_string, !string_start_pos)) }
(*e: rule string *)
(*e: lexer.mll *)
