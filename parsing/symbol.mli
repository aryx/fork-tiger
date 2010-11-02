(*s: symbol.mli *)
type symbol

val uid        : symbol -> int
val name       : symbol -> string
val symbol     : string -> symbol
val new_symbol : string -> symbol
(*x: symbol.mli *)
type 'a table

val enter  : 'a table -> symbol -> 'a -> unit
val look   : 'a table -> symbol -> 'a
val mem    : 'a table -> symbol -> bool
val create : (string * 'a) list -> 'a table

val new_scope : 'a table -> 'a table
(*x: symbol.mli *)
val iter : (int -> symbol -> 'a -> unit) -> 'a table -> unit
val fold : (int -> symbol -> 'a -> 'b -> 'b) -> 'a table -> 'b -> 'b
(*e: symbol.mli *)
