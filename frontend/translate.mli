(*s: translate.mli *)
type exp   = Tree.exp
type label = Tree.label

val nil           : exp
val int_literal   : int -> exp
val str_literal   : string -> exp
val simple_var    : Frame.frame -> Frame.access -> exp
val field_var     : exp -> int -> bool -> exp
val subscript_var : exp -> exp -> bool -> int -> exp
val assign        : exp -> exp -> exp
val call          : Frame.frame -> label -> string option ->
                    Frame.frame -> exp list -> label option -> bool -> exp
val arithmetic    : Ast.oper -> exp -> exp -> exp
val compare_int   : Ast.oper -> exp -> exp -> exp
val compare_str   : Ast.oper -> exp -> exp -> exp
val ifexp         : exp -> exp -> exp -> bool -> exp
val loop          : exp -> exp -> label -> exp
val break         : label -> exp
val new_record    : (exp * bool) list -> exp
val new_array     : exp -> exp -> bool -> exp
val sequence      : exp list -> exp
val func          : Frame.frame -> exp -> bool -> exp

val try_block     : exp -> label -> (int *exp) list -> exp
val raise_exn     : int -> exp
val spawn         : label -> exp
(*e: translate.mli *)
