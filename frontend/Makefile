TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=tree.ml frame.ml environment.ml \
    canonical.ml translate.ml semantics.ml

SYSLIBS=
LIBS=

INCLUDEDIRS= $(TOP)/parsing

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
