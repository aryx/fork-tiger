(*s: frontend/canonical.ml *)
(*s: canonical.ml *)
module T = Tree
module S = Symbol

let nop = T.EXP(T.CONST 0)

let ( % ) x y =
  match (x,y) with
    (T.EXP(T.CONST _), _) -> y
  | (_, T.EXP(T.CONST _)) -> x
  | _ -> T.SEQ(x,y)

let commute = function
    (T.EXP(T.CONST _), _) -> true
  | (_, T.NAME _) -> true
  | (_, T.CONST _) -> true
  | _ -> false


let linearize stm0 =
  let rec reorder = function
      T.CALL(_,_,_,_,ptr) as call :: rest ->
        let t = T.TEMP(T.new_temp(), ptr)
        in reorder(T.ESEQ(T.MOVE(call, t), t) :: rest)
    | a :: rest ->
        let (stms,e) = do_exp a
        and (stms',el) = reorder rest in
        if commute(stms',e)
        then (stms % stms',e::el)
        else let t = T.TEMP(T.new_temp(), T.is_ptr e) in 
        (stms % T.MOVE(e, t) % stms', t :: el)
    | [] -> (nop,[])

  and reorder_exp(el,build) = 
    let (stms,el') = reorder el
    in (stms, build el')

  and reorder_stm(el,build) =
    let (stms,el') = reorder el
    in stms % (build el')

  and do_stm = function
      T.SEQ(a,b) ->
        do_stm a % do_stm b
    | T.JUMP e ->
        let f l = T.JUMP (List.hd l)
        in reorder_stm([e], f)
    | T.CJUMP(e,t,f) ->
        let f l = T.CJUMP(List.hd l, t, f)
        in reorder_stm([e], f)
    | T.MOVE(T.CALL(e,el,ext,k,ptr),T.TEMP(t,_)) ->
        let f l = T.MOVE(T.CALL(List.hd l, List.tl l, ext, k, ptr), T.TEMP(t,ptr))
        in reorder_stm(e :: el, f)
    | T.MOVE(b,T.TEMP(t,ptr)) ->
        let f l = T.MOVE(List.hd l, T.TEMP(t,ptr))
        in reorder_stm([b], f)
    | T.MOVE(b,T.NAME n) ->
        let f l = T.MOVE(List.hd l, T.NAME n)
        in reorder_stm([b], f)
    | T.MOVE(T.ESEQ(s,e), b) ->
        do_stm(T.SEQ(s,T.MOVE(e,b)))
    | T.MOVE(b,T.MEM(e,ptr)) ->
        let f l = T.MOVE(List.hd l, T.MEM(List.nth l 1, ptr))
        in reorder_stm([b ; e], f)
    | T.MOVE(b,T.ESEQ(s,e)) ->
        do_stm(T.SEQ(s,T.MOVE(b,e)))
    | T.EXP(T.CALL(e,el,ext,k,ptr)) ->
        let f l = T.EXP(T.CALL(List.hd l, List.tl l, ext, k, ptr))
        in reorder_stm(e :: el, f)
    | T.EXP e ->
        let f l = T.EXP (List.hd l)
        in reorder_stm([e], f)
    | s ->
        let f l = s
        in reorder_stm([], f)

  and do_exp = function
      T.BINOP(p,a,b) ->
        let f l = T.BINOP(p, List.hd l, List.nth l 1)
        in reorder_exp([a ; b], f)
    | T.RELOP(p,a,b) ->
        let f l = T.RELOP(p, List.hd l, List.nth l 1)
        in reorder_exp([a ; b], f)
    | T.MEM(a,ptr) ->
        let f l = T.MEM (List.hd l,ptr)
        in reorder_exp([a], f)
    | T.ESEQ(s,e) ->
        let stms = do_stm s
        and (stms',e) = do_exp e
        in (stms%stms',e)
    | T.CALL(e,el,ext,k,ptr) ->
        let f l = T.CALL(List.hd l, List.tl l, ext, k, ptr)
        in reorder_exp(e :: el, f)
    | e ->
        let f l = e
        in reorder_exp([], f)

  (* linear gets rid of the top-level SEQ's, producing a list *)
  and linear = function
      (T.SEQ(a,b),l) -> linear(a,linear(b,l))
    | (s,l) -> s :: l

 in (* body of linearize *)
    linear(do_stm stm0, [])
(*e: canonical.ml *)
(*e: frontend/canonical.ml *)
