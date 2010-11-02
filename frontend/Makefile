TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=environment.ml tree.ml \
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

SRCBASE=$(SRC:.ml=)

sync:
	for i in $(SRCBASE); do $(SYNCWEB) $$i.nw $$i.ml; $(SYNCWEB) $$i.nw $$i.mli  || exit 1; done 

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
