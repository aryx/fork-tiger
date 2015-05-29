(*s: frontend/translate.mli *)
(*s: translate.mli *)

(*s: signature functions Translate.xxx *)
val func          : Tree.exp -> bool -> Tree.exp
(*x: signature functions Translate.xxx *)
val nil           : Tree.exp
val int_literal   : int -> Tree.exp
val str_literal   : string -> Tree.exp
(*x: signature functions Translate.xxx *)
val field_var     : Tree.exp -> int -> bool -> Tree.exp
(*x: signature functions Translate.xxx *)
val subscript_var : Tree.exp -> Tree.exp -> bool -> int -> Tree.exp
(*x: signature functions Translate.xxx *)
val simple_var    : Frame.frame -> Frame.access -> Tree.exp
(*x: signature functions Translate.xxx *)
val new_record    : (Tree.exp * bool) list -> Tree.exp
val new_array     : Tree.exp -> Tree.exp -> bool -> Tree.exp
(*x: signature functions Translate.xxx *)
val arithmetic    : Ast.oper -> Tree.exp -> Tree.exp -> Tree.exp
val compare_int   : Ast.oper -> Tree.exp -> Tree.exp -> Tree.exp
val compare_str   : Ast.oper -> Tree.exp -> Tree.exp -> Tree.exp
(*x: signature functions Translate.xxx *)
val call          : 
  Frame.frame -> Tree.label -> string option -> Frame.frame -> 
  Tree.exp list -> Tree.label option -> bool -> 
  Tree.exp
(*x: signature functions Translate.xxx *)
val sequence      : Tree.exp list -> Tree.exp
(*x: signature functions Translate.xxx *)
val assign        : Tree.exp -> Tree.exp -> Tree.exp
(*x: signature functions Translate.xxx *)
val ifexp         : Tree.exp -> Tree.exp -> Tree.exp -> bool -> Tree.exp
(*x: signature functions Translate.xxx *)
val loop          : Tree.exp -> Tree.exp -> Tree.label -> Tree.exp
(*x: signature functions Translate.xxx *)
val break         : Tree.label -> Tree.exp
(*x: signature functions Translate.xxx *)
val try_block     : Tree.exp -> Tree.label -> (int *Tree.exp) list -> Tree.exp
val raise_exn     : int -> Tree.exp
(*x: signature functions Translate.xxx *)
val spawn         : Tree.label -> Tree.exp
(*e: signature functions Translate.xxx *)
(*e: translate.mli *)
(*e: frontend/translate.mli *)
