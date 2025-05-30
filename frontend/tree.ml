(*s: frontend/tree.ml *)
module S = Symbol

(*s: type Tree.label *)
type label = Symbol.symbol
(*e: type Tree.label *)
(*s: type Tree.temp *)
and  temp  = Symbol.symbol
(*e: type Tree.temp *)

(*s: type Tree.is_ptr *)
type is_ptr = bool
(*e: type Tree.is_ptr *)

(*s: type Tree.stm *)
type stm =
  | EXP    of exp
  | MOVE   of exp * exp

  | LABEL  of label
  | JUMP   of label
  | CJUMP  of exp * label * label
  | RET    of exp
  (*s: [[Tree.stm]] cases *)
  | SEQ    of stm * stm
  (*x: [[Tree.stm]] cases *)
    | CONT   of label * label list
    | TRY    of label
    | TRYEND of label
  (*e: [[Tree.stm]] cases *)
(*e: type Tree.stm *)
(*s: type Tree.exp *)
and exp =
  | CONST of int
  | BINOP of binop * exp * exp
  | RELOP of relop * exp * exp

  | NAME  of label
  | TEMP  of temp * is_ptr
  | MEM   of exp * is_ptr (* dereference? *x ? *)

  | CALL  of exp * exp list * string option * label option * is_ptr
  (*s: [[Tree.exp]] cases *)
  | ESEQ  of stm * exp
  (*e: [[Tree.exp]] cases *)
(*e: type Tree.exp *)

(*s: types Tree.xxxop *)
and binop = PLUS | MINUS | MUL | DIV
and relop = EQ | NE   | LT | GT | LE | GE
(*e: types Tree.xxxop *)

(*s: tree.ml *)
(*s: function Tree.new_label *)
let new_label s = 
  S.new_symbol ("L" ^ s)
(*e: function Tree.new_label *)
(*s: function Tree.new_temp *)
let new_temp () = 
  S.new_symbol "temp"
(*e: function Tree.new_temp *)
(*x: tree.ml *)
let relop_inverse = function
    EQ  -> NE
  | NE  -> EQ
  | LT  -> GE
  | GT  -> LE
  | LE  -> GT
  | GE  -> LT
let cmm_binop = function
    PLUS  -> "add"
  | MINUS -> "sub"
  | MUL   -> "mul"
  | DIV   -> "quot"
let cmm_relop = function
    EQ -> "eq"
  | NE -> "ne"
  | LT -> "lt"
  | GT -> "gt"
  | LE -> "le"
  | GE -> "ge"
(*x: tree.ml *)
let rec is_ptr = function
    BINOP _         -> false
  | RELOP _         -> false
  | MEM(_,p)        -> p
  | TEMP(_,p)       -> p
  | ESEQ(_,e)       -> is_ptr e
  | NAME _          -> false
  | CONST _         -> false
  | CALL(_,_,_,_,p) -> p
(*x: tree.ml *)
module TempSet = Set.Make(
  struct
    type t = Symbol.symbol * bool
    let compare = Stdlib.compare
  end)
(*x: tree.ml *)
let find_temps stmts =
  let rec stm set = function
      SEQ(a,b)     -> stm (stm set a) b
    | LABEL _      -> set
    | CONT _       -> set
    | JUMP _       -> set
    | CJUMP(e,_,_) -> exp set e
    | MOVE(a,b)    -> exp (exp set a) b
    | EXP e        -> exp set e
    | TRY _        -> set
    | TRYEND _     -> set
    | RET e        -> exp set e
  and exp set = function
      BINOP(_,a,b)     -> exp (exp set a) b
    | RELOP(_,a,b)     -> exp (exp set a) b
    | MEM(e,_)         -> exp set e
    | TEMP(t,ptr)      -> TempSet.add (t,ptr) set
    | ESEQ(s,e)        -> exp (stm set s) e
    | NAME _           -> set
    | CONST _          -> set
    | CALL(e,el,_,_,_) -> List.fold_left exp set (e :: el)
  in
  TempSet.elements (List.fold_left stm TempSet.empty stmts)
(*x: tree.ml *)
(*s: function Tree.print_stm *)
let print_stm =
  let rec iprintf = function
      0 -> Printf.printf
    | i -> (print_string "  "; iprintf (i-1))
  in
  let rec prstm d = function
    | LABEL l      -> iprintf d "LABEL:%s\n " (S.name l)
    | CONT(l,_ls)   -> iprintf d "CONT:%s\n "  (S.name l)
    | TRY l        -> iprintf d "TRY:%s\n"    (S.name l)
    | TRYEND l     -> iprintf d "TRYEND:%s\n" (S.name l)
    | SEQ(a,b)     -> iprintf d "SEQ:\n";     prstm(d+1) a; prstm(d+1) b
    | MOVE(a,b)    -> iprintf d "MOVE:\n";    prexp(d+1) a; prexp(d+1) b
    | JUMP l       -> iprintf d "JUMP:%s\n"   (S.name l)
    | EXP e        -> iprintf d "EXP:\n";     prexp(d+1) e
    | RET e        -> iprintf d "RET:\n";     prexp(d+1) e
    | CJUMP(a,t,f) -> iprintf d "CJUMP:\n";   prexp(d+1) a;
                      iprintf (d+1) "true  label: %s\n" (S.name t);
                      iprintf (d+1) "false label: %s\n" (S.name f)
  and prexp d = function
      BINOP(p,a,b)     -> iprintf d "BINOP:%s\n" (cmm_binop p);
                          prexp (d+1) a; prexp (d+1) b
    | RELOP(p,a,b)     -> iprintf d "RELOP:%s\n" (cmm_relop p);
                          prexp (d+1) a; prexp (d+1) b
    | MEM(e,_)         -> iprintf d "MEM:\n"; prexp (d+1) e
    | TEMP(t,_)        -> iprintf d "TEMP %s\n" (S.name t)
    | ESEQ(s,e)        -> iprintf d "ESEQ:\n";
                          prstm (d+1) s; prexp (d+1) e
    | NAME lab         -> iprintf d "NAME %s\n" (S.name lab)
    | CONST i          -> iprintf d "CONST %d\n" i
    | CALL(e,el,_,_,_) -> iprintf d "CALL:\n";
                          prexp (d+1) e; List.iter (prexp (d+2)) el
  in prstm 0
(*e: function Tree.print_stm *)
(*x: tree.ml *)
(*s: function Tree.print_exp *)
let print_exp e = print_stm (EXP e)
(*e: function Tree.print_exp *)
(*e: tree.ml *)
(*e: frontend/tree.ml *)
