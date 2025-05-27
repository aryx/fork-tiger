##############################################################################
# Literate Programming rules
##############################################################################

TOP=../..

#sync:
#	$(SYNCWEB) -lang ocaml main.nw main.ml
#lpdistclean::
#	rm -f $(LPSRC) .md5sum_*

include $(TOP)/docs/latex/Makefile.common

TEXMAIN=Tiger.nw
TEX=Tiger.tex

SRC_ORIG=Tiger.nw Tiger_extra.nw

SRC_VIEWS= \
 $(TOP)/parsing/error.mli\
 $(TOP)/parsing/error.ml\
 $(TOP)/parsing/option.mli\
 $(TOP)/parsing/option.ml\
 $(TOP)/parsing/symbol.mli\
 $(TOP)/parsing/symbol.ml\
 $(TOP)/parsing/ast.mli\
 $(TOP)/parsing/ast.ml\
 $(TOP)/frontend/environment.mli\
 $(TOP)/frontend/environment.ml\
 $(TOP)/frontend/semantics.mli\
 $(TOP)/frontend/semantics.ml\
 $(TOP)/frontend/tree.mli\
 $(TOP)/frontend/tree.ml\
 $(TOP)/frontend/translate.mli\
 $(TOP)/frontend/translate.ml\
 $(TOP)/frontend/canonical.mli\
 $(TOP)/frontend/canonical.ml\
 $(TOP)/frontend/frame.mli\
 $(TOP)/frontend/frame.ml\
 $(TOP)/backend/codegen.mli\
 $(TOP)/backend/codegen.ml\
 $(TOP)/main/main.ml\
 $(TOP)/parsing/lexer.mll

# -lang C
#alloc.c--
#client.c
#gc.c
#gc.h
#runtime.c--

sync::
	$(MAKE) sync2
sync2:
	$(MAKE) LANG=ocamlyacc sync3
sync3:
	$(SYNCWEB) $(SRC_ORIG) parsing/parser.mly
