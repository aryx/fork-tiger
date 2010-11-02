TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=codegen.ml

SYSLIBS=

LIBS=

INCLUDEDIRS= \
 $(TOP)/parsing \
 $(TOP)/frontend \

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Top rules
##############################################################################
all:: $(TARGET).cma 
all.opt:: $(TARGET).cmxa
opt: all.opt


$(TARGET).cma: $(OBJS) $(LIBS)
	$(OCAMLC) -a -o $(TARGET).cma $(OBJS)

$(TARGET).cmxa: $(OPTOBJS) $(LIBS:.cma=.cmxa)
	$(OCAMLOPT) -a -o $(TARGET).cmxa $(OPTOBJS)


clean::
	rm -f $(TARGET).top

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocaml codegen.nw codegen.ml 
	$(SYNCWEB) -lang ocaml codegen.nw codegen.mli


lpdistclean::
	rm -f *.ml *.mli .md5sum_*
