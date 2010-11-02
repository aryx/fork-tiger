TOP=..

##############################################################################
# Variables
##############################################################################
TARGET=lib

SRC=symbol.ml ast.ml \
  option.ml error.ml \
  parser.ml lexer.ml 

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

lexer.ml: lexer.mll
	$(OCAMLLEX) $<
clean::
	rm -f lexer.ml
beforedepend:: lexer.ml


parser.ml parser.mli: parser.mly
	$(OCAMLYACC) $<
clean::
	rm -f parser.ml parser.mli parser.output
beforedepend:: parser.ml parser.mli

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang ocamlyacc parser.nw parser.mly
	$(SYNCWEB) -lang ocaml parser.nw lexer.mll
	$(SYNCWEB) -lang ocaml symbol.nw symbol.ml 
	$(SYNCWEB) -lang ocaml symbol.nw symbol.mli
	$(SYNCWEB) -lang ocaml ast.nw ast.ml 
	$(SYNCWEB) -lang ocaml ast.nw ast.mli
	$(SYNCWEB) -lang ocaml error.nw error.ml 
	$(SYNCWEB) -lang ocaml error.nw error.mli
	$(SYNCWEB) -lang ocaml option.nw option.ml
	$(SYNCWEB) -lang ocaml option.nw option.mli


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
