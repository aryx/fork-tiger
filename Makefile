#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Variables
##############################################################################
TOP=$(shell pwd)

SRC=main.ml

TARGET=tiger

SYSLIBS=

LIBS= \
 parsing/lib.cma \
 frontend/lib.cma \
 backend/lib.cma \

MAKESUBDIRS=parsing frontend backend \

#TODO
#  stdlib runtime

INCLUDEDIRS=$(MAKESUBDIRS)

##############################################################################
# Generic variables
##############################################################################
-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

all:: 
	$(MAKE) rec 
	$(MAKE) $(TARGET) 

opt:
	$(MAKE) rec.opt 
	$(MAKE) $(TARGET).opt

all.opt: opt
top: $(TARGET).top


rec:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all || exit 1; done 

rec.opt:
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i all.opt || exit 1; done 


$(TARGET): $(LIBS) $(OBJS)
	$(OCAMLC) $(BYTECODE_STATIC) -o $@ $(SYSLIBS) $^

$(TARGET).opt: $(LIBS:.cma=.cmxa) $(OPTOBJS) 
	$(OCAMLOPT) $(STATIC) -o $@ $(SYSLIBS:.cma=.cmxa)  $^


$(TARGET).top: $(LIBS) $(OBJS) 
	$(OCAMLMKTOP) -o $@ $(SYSLIBS) $^

clean::
	rm -f $(TARGET) $(TARGET).opt $(TARGET).top

clean::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i clean; done 

depend::
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i depend; done


# add -custom so dont need add e.g. ocamlbdb/ in LD_LIBRARY_PATH
CUSTOM=-custom

static:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) STATIC="-ccopt -static" $(EXEC).opt
	cp $(EXEC).opt $(EXEC)

purebytecode:
	rm -f $(EXEC).opt $(EXEC)
	$(MAKE) BYTECODE_STATIC="" $(EXEC)


distclean:: clean
	set -e; for i in $(MAKESUBDIRS); do $(MAKE) -C $$i $@; done
	rm -f Makefile.config

##############################################################################
# Build documentation
##############################################################################
.PHONY:: docs

##############################################################################
# Install
##############################################################################

##############################################################################
# Developer rules
##############################################################################

DIRS= $(filter-out commons stdlib runtime, $(MAKESUBDIRS))
dotall:
	ocamldoc $(INCLUDES) $(DIRS:=/*.ml) $(SRC)  -dot -dot-reduce 
	dot -Tps ocamldoc.out > dot.ps
	mv dot.ps Fig_graph_ml.ps
	ps2pdf Fig_graph_ml.ps
	rm -f Fig_graph_ml.ps

##############################################################################
# Literate Programming rules
##############################################################################

#sync:
#	$(SYNCWEB) -lang ocaml main.nw main.ml
#lpdistclean::
#	rm -f $(LPSRC) .md5sum_*

include $(TOP)/docs/latex/Makefile.common

TEXMAIN=Tiger.nw
TEX=Tiger.tex

SRC_ORIG=Tiger.nw Tiger_extra.nw

SRC_VIEWS= \
 parsing/error.mli\
 parsing/error.ml\
 parsing/option.mli\
 parsing/option.ml\
 parsing/symbol.mli\
 parsing/symbol.ml\
 parsing/ast.mli\
 parsing/ast.ml\
 frontend/environment.mli\
 frontend/environment.ml\
 frontend/semantics.mli\
 frontend/semantics.ml\
 frontend/tree.mli\
 frontend/tree.ml\
 frontend/translate.mli\
 frontend/translate.ml\
 frontend/canonical.mli\
 frontend/canonical.ml\
 frontend/frame.mli\
 frontend/frame.ml\
 backend/codegen.mli\
 backend/codegen.ml\
 main.ml\

# -lang C
#alloc.c--
#client.c
#gc.c
#gc.h
#runtime.c--

