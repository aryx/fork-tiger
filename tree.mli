# 14 "tree.nw"
type label = Symbol.symbol
and  temp  = Symbol.symbol
# 24 "tree.nw"
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
# 43 "tree.nw"
and exp =
    BINOP of binop * exp * exp
  | RELOP of relop * exp * exp
  | MEM   of exp * bool
  | TEMP  of temp * bool
  | ESEQ  of stm * exp
  | NAME  of label
  | CONST of int
  | CALL  of exp * exp list * string option * label option * bool
# 56 "tree.nw"
and binop = PLUS | MINUS | MUL | DIV
and relop = EQ | NE | LT | GT | LE | GE
# 74 "tree.nw"
val new_label : string -> label
val new_temp  : unit   -> temp
# 82 "tree.nw"
val relop_inverse  : relop -> relop
val cmm_binop      : binop -> string
val cmm_relop      : relop -> string
# 93 "tree.nw"
val is_ptr     : exp -> bool
val find_temps : stm list -> (temp * bool) list
# 99 "tree.nw"
val print_stm : stm -> unit
val print_exp : exp -> unit
