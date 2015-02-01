(*s: backend/codegen.ml *)
(*s: codegen.ml *)
module E = Error
module S = Symbol
module T = Tree
let pf           = Printf.printf
let spf          = Printf.sprintf
let join_map f l = String.concat "," (List.map f l)
(*x: codegen.ml *)
let output_file_header imports =
  let pr_import x = pf "import bits32 \"tig_%s\" as %s;\n" x x in
  pf "target byteorder little;\n";
  List.iter pr_import imports;
  pf "export tiger_main;\n\n";
  pf "bits32 alloc_ptr;\n";
  pf "import space_end;\n\n"
(*x: codegen.ml *)
let emit exl =
  (*s: statements *)
    let rec stm = function
        T.LABEL l            -> spf "%s:" (S.name l)
      | T.CONT(l,ls)         -> spf "continuation %s(%s):"
                                (S.name l) (join_map S.name ls)
      | T.JUMP e             -> spf "goto %s;" (valexp e)
      | T.CJUMP(ex, l1, l2)  -> spf "if(%s) {goto %s;} else {goto %s;}"
                                (boolexp ex) (S.name l1) (S.name l2)
      | T.MOVE(e1, e2)       -> spf "%s = %s;" (valexp e2) (valexp e1)
      | T.EXP(T.CALL _ as e) -> spf "%s;" (valexp e)
      | T.EXP e              -> spf "/* eliminated: %s */" (valexp e)
      | T.TRY l              -> spf "span 2 1 { /* %s */" (S.name l)
      | T.TRYEND l           -> spf "} /* end %s */" (S.name l)
      | T.RET e              -> spf "return(%s);" (valexp e)
      | T.SEQ _              -> E.internal "SEQ node found in code gen"
  (*e: statements *)
  (*s: value expressions *)
    and valexp = function
        T.BINOP(bop, e1, e2)  -> spf "%%%s(%s, %s)"
                                 (T.cmm_binop bop) (valexp e1) (valexp e2)
      | T.RELOP _ as e        -> spf "%%sx32(%%bit(%s))" (boolexp e)
      | T.MEM(e,ptr)          -> spf "bits32[%s]" (valexp e)
      | T.TEMP(t,ptr)         -> spf "%s" (S.name t)
      | T.NAME l              -> (S.name l)
      | T.CONST i             -> string_of_int i
      | T.CALL(l,el,cc,k,ptr) ->
          let cc = match cc with
                     None   -> ""
                   | Some s -> spf "foreign \"%s\" " s
          and k  = match k with
                     None   -> ""
                   | Some l -> spf "also %s to %s"
                      (if Option.use_unwind() then "unwinds" else "cuts")
                      (S.name l)
          in
          spf "%s %s(%s) also aborts %s"
              cc (valexp l) (join_map valexp el) k
      | T.ESEQ _ ->
          E.internal "ESEQ node found in code gen"
  (*e: value expressions *)
  (*s: boolean expresssions *)
    and boolexp = function
      | T.RELOP(rop, e1, e2) -> spf "%%%s(%s, %s)"
                                (T.cmm_relop rop) (valexp e1) (valexp e2)
      | e                    -> spf "%%ne(%s, 0)" (valexp e)
  (*e: boolean expresssions *)
  in
  let code = List.map stm exl in
  List.iter (fun x -> pf "  %s\n" x) code
(*e: codegen.ml *)
(*e: backend/codegen.ml *)
