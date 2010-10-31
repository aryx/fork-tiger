{
module E = Error
module P = Parser

(* The table of keywords *)
let keyword_table = Hashtbl.create 22;;
List.iter (fun (key, data) -> Hashtbl.add keyword_table key data)
  [
   "and",       P.AND;
   "array",     P.ARRAY;
   "break",     P.BREAK;
   "do",        P.DO;
   "else",      P.ELSE;
   "end",       P.END;
   "exception", P.EXCEPTION;
   "for",       P.FOR;
   "function",  P.FUNCTION;
   "handle",    P.HANDLE;
   "if",        P.IF;
   "in",        P.IN;
   "let",       P.LET;
   "nil",       P.NIL;
   "of",        P.OF;
   "or",        P.OR;
   "raise",     P.RAISE;
   "spawn",     P.SPAWN;
   "then",      P.THEN;
   "to",        P.TO;
   "try",       P.TRY;
   "type",      P.TYPE;
   "var",       P.VAR;
   "while",     P.WHILE;
 ];;
  
(* To buffer string literals *)

let escape c = 
  match c with
  | 'n' -> '\n'
  | 'r' -> '\r'
  | 'b' -> '\b'
  | 't' -> '\t'
  | _ -> c

let line_num = ref 0
let string_start_pos = ref 0
let buffer = Buffer.create 30
let comment_pos = Stack.create()
}

let nl = ['\010' '\013']
let blank = [' ' '\009' '\012']
let letter = ['A'-'Z' 'a'-'z']
let number = ['0'-'9']
let identchar = ['A'-'Z' 'a'-'z' '_' '0'-'9']

rule token = parse
    nl   { incr line_num;
           E.add_source_mapping (Lexing.lexeme_end lexbuf) !line_num;
           token lexbuf }
  | blank + { token lexbuf }
  | letter identchar *
      { let s = Lexing.lexeme lexbuf in
        try
          Hashtbl.find keyword_table s
        with Not_found ->
          P.ID s }
  | number +
      { P.INT (int_of_string(Lexing.lexeme lexbuf)) }
  | "\"" 
      { string_start_pos := Lexing.lexeme_start lexbuf;
        P.STRING (string lexbuf) }
  | "/*" { comment lexbuf; token lexbuf }
  | "&"  { P.AND }
  | ":=" { P.ASSIGN }
  | ":"  { P.COLON }
  | ","  { P.COMMA }
  | "/"  { P.DIVIDE }
  | "."  { P.DOT }
  | "="  { P.EQ }
  | ">=" { P.GE }
  | ">"  { P.GT }
  | "{"  { P.LBRACE }
  | "["  { P.LBRACK }
  | "<=" { P.LE }
  | "("  { P.LPAREN }
  | "<"  { P.LT }
  | "-"  { P.MINUS }
  | "<>" { P.NEQ }
  | "|"  { P.OR }
  | "+"  { P.PLUS }
  | "}"  { P.RBRACE }
  | "]"  { P.RBRACK }
  | ")"  { P.RPAREN }
  | ";"  { P.SEMICOLON }
  | "*"  { P.TIMES }
  | eof  { P.EOF }
  | _
      { raise (E.Error(E.Illegal_character (Lexing.lexeme_char lexbuf 0),
                       Lexing.lexeme_start lexbuf)) }

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

and string = parse
    '"'
      { let s = Buffer.contents buffer in
        (Buffer.clear buffer; s) }
  | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r']
      { Buffer.add_char buffer (escape (Lexing.lexeme_char lexbuf 1));
        string lexbuf }
  | [^ '"' '\\'] +
      { Buffer.add_string buffer (Lexing.lexeme lexbuf);
        string lexbuf }
  | eof
      { raise (E.Error(E.Unterminated_string, !string_start_pos)) }
