TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=environment.ml symbol.ml tree.ml \
    canonical.ml translate.ml semantics.ml

SYSLIBS=

LIBS=

INCLUDEDIRS=

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

SRCNW=$(SRC:.ml=.nw)
LPSRC=$(SRC) $(SRC:.ml=.mli)

sync:
	for i in $(LPSRC); do $(SYNCWEB) $(SRCNW) $$i || exit 1; done 

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
