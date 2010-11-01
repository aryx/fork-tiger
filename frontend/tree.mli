(*s: tree.mli *)
(*s: types *)
type label = Symbol.symbol
and  temp  = Symbol.symbol
(*x: types *)
type stm =
    SEQ    of stm * stm
  | LABEL  of label
  | CONT   of label * label list
  | JUMP   of exp
  | CJUMP  of exp * label * label
  | MOVE   of exp * exp
  | EXP    of exp
  | TRY    of label
  | TRYEND of label
  | RET    of exp
(*x: types *)
and exp =
    BINOP of binop * exp * exp
  | RELOP of relop * exp * exp
  | MEM   of exp * bool
  | TEMP  of temp * bool
  | ESEQ  of stm * exp
  | NAME  of label
  | CONST of int
  | CALL  of exp * exp list * string option * label option * bool
(*x: types *)
and binop = PLUS | MINUS | MUL | DIV
and relop = EQ | NE | LT | GT | LE | GE
(*e: types *)
(*s: utility functions *)
val new_label : string -> label
val new_temp  : unit   -> temp
(*x: utility functions *)
val relop_inverse  : relop -> relop
val cmm_binop      : binop -> string
val cmm_relop      : relop -> string
(*x: utility functions *)
val is_ptr     : exp -> bool
val find_temps : stm list -> (temp * bool) list
(*x: utility functions *)
val print_stm : stm -> unit
val print_exp : exp -> unit
(*e: utility functions *)
(*e: tree.mli *)
