(*s: frontend/tree.mli *)
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

(*s: utility functions *)
val new_label : string -> label
val new_temp  : unit   -> temp
(*x: utility functions *)
val relop_inverse  : relop -> relop
val cmm_binop      : binop -> string
val cmm_relop      : relop -> string
(*x: utility functions *)
val is_ptr     : exp -> is_ptr
(*s: signature function Tree.find_temps *)
val find_temps : stm list -> (temp * is_ptr) list
(*e: signature function Tree.find_temps *)
(*x: utility functions *)
val print_stm : stm -> unit
val print_exp : exp -> unit
(*e: utility functions *)
(*e: frontend/tree.mli *)
