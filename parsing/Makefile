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
	perl -p -i -e 's#/\*\(\*[sex]: .* \*\)\*/##' parser.ml
#pad: this  perl thing is just because ocamlyacc does not handle well
# syncweb special comment at the very end

clean::
	rm -f parser.ml parser.mli parser.output
beforedepend:: parser.ml parser.mli
