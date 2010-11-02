#!/bin/sh
   ocamlc  -c option.mli
   ocamlc  -c option.ml
   ocamlc  -c error.mli
   ocamlc  -c error.ml
   ocamlc  -c symbol.mli
   ocamlc  -c symbol.ml
   ocamlc  -c ast.mli
   ocamlc  -c ast.ml
   ocamlc  -c parser.mli
   ocamlc  -c parser.ml
   ocamlc  -c lexer.ml
   ocamlc  -c tree.mli
   ocamlc  -c tree.ml
   ocamlc  -c frame.mli
   ocamlc  -c frame.ml
   ocamlc  -c environment.mli
   ocamlc  -c environment.ml
   ocamlc  -c translate.mli
   ocamlc  -c translate.ml
   ocamlc  -c semantics.mli
   ocamlc  -c semantics.ml
   ocamlc  -c canonical.mli
   ocamlc  -c canonical.ml
   ocamlc  -c codegen.mli
   ocamlc  -c codegen.ml
   ocamlc  -c driver.ml
   ocamlc   -o tigerc option.cmo error.cmo symbol.cmo ast.cmo parser.cmo lexer.cmo tree.cmo frame.cmo environment.cmo translate.cmo semantics.cmo canonical.cmo codegen.cmo driver.cmo
   gcc -Wall -I /usr/local/bin/../include/qc-- -c stdlib.c -o stdlib.o
qc-- -stop .o -o stdlibcmm.o stdlibcmm.c--
   gcc -Wall -I /usr/local/bin/../include/qc-- -c gc.c -o gc.o
qc-- -stop .o -o alloc.o alloc.c--
   ar cr stdlib.a stdlib.o stdlibcmm.o gc.o alloc.o
   qc-- -stop .o -o runtime.o runtime.c--
