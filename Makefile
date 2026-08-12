#############################################################################
# Configuration section
#############################################################################

##############################################################################
# Top rules using dune
##############################################################################

all::
	dune build
clean::
	dune clean
install::
	dune install
uninstall::
	dune uninstall
# The behavioural test suite: compile each Tiger program in tests/tiger.tests
# with tigerc and qc, link it, run it, and check its stdout and exit code.
#
# It needs the libraries, so build them first. It compares against a recorded
# baseline rather than demanding that everything pass - see tests/run-tests.sh
# and the failures noted in tests/expected/tiger.txt.
#
# Requires ./configure to have been run, and an installed qc--.
test::
	$(MAKE) -C stdlib
	$(MAKE) -C runtime
	tests/run-tests.sh

# Same suite, against qc--'s -ppc-elf backend instead of its default x86
# one. Kept as its own target rather than folded into "test" - it needs a
# ppc cross toolchain and qemu-ppc (see ./configure's output), and it is
# not expected to be all-green yet: see tests/expected/tiger-ppc.txt and
# its header for the known qc-- backend gaps this has already turned up.
test-ppc::
	$(MAKE) -C stdlib BACKEND=ppc
	$(MAKE) -C runtime BACKEND=ppc
	BACKEND=ppc tests/run-tests.sh

build-docker:
	docker build -t "tigerc" .

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
# Old top rules
##############################################################################
.PHONY:: all all.opt opt top clean distclean

allold:: 
	$(MAKE) rec 
	$(MAKE) $(TARGET) 

optold:
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
