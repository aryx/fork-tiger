TOP=..

##############################################################################
# Variables
##############################################################################
-include $(TOP)/Makefile.config

##############################################################################
# Generic variables
##############################################################################

-include $(TOP)/Makefile.common

##############################################################################
# Backend selection
##############################################################################
# claude: BACKEND picks qc--'s target; see stdlib/Makefile's header for the
# fuller reasoning. Default (x86) keeps the original flat filenames
# (runtime.o, stdlib.a, qcmm.a) so tests/run-tests.sh's existing paths are
# untouched; ppc's outputs live under build-ppc/ instead, alongside
# stdlib/'s build-ppc/.
BACKEND ?= x86
ifeq ($(BACKEND),x86)
CC        = $(CC32)
AR        = $(AR32)
QCFLAG    =
B         = .
STDLIBDIR = ../stdlib
else ifeq ($(BACKEND),ppc)
CC        = $(CC_PPC)
AR        = $(AR_PPC)
QCFLAG    = -ppc-elf
B         = build-ppc
STDLIBDIR = ../stdlib/build-ppc
else
$(error unknown BACKEND=$(BACKEND) - expected x86 or ppc)
endif

##############################################################################
# Top rules
##############################################################################

# See stdlib/Makefile for why CC32/CC_PPC, -fno-omit-frame-pointer and
# -fcommon.
all:: $(B)/stdlib.a $(B)/runtime.o $(B)/qcmm.a

$(B):
	mkdir -p $(B)

# claude: gc.o's real prerequisites also include qc--'s installed
# qc--runtime.h (gc.c #includes it via -I $(QCINCLUDE)); alloc.o/runtime.o
# are compiled BY $(QC), so a qc-- fix with no local .c--/.h change
# wouldn't otherwise be noticed. Without these, "make" at the top level
# silently reused stale objects after a qc-- rebuild+reinstall.
$(B)/gc.o: gc.c gc.h $(QCINCLUDE)/qc--runtime.h | $(B)
	$(CC) -w -fcommon -fno-omit-frame-pointer -I $(QCINCLUDE) -c gc.c -o $@

# claude: alloc.c--/runtime.c-- declare "target byteorder little"; same
# ppc byteorder flip as stdlib/Makefile's stdlibcmm.o, into a build-local
# copy so the checked-in sources stay backend-neutral.
ifeq ($(BACKEND),ppc)
$(B)/alloc.o: alloc.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' alloc.c-- > $(B)/alloc.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/alloc.c--

$(B)/runtime.o: runtime.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' runtime.c-- > $(B)/runtime.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/runtime.c--
else
$(B)/alloc.o: alloc.c-- $(QC) | $(B)
	$(QC) $(QCFLAG) -stop .o -o $@ alloc.c--

$(B)/runtime.o: runtime.c-- $(QC) | $(B)
	$(QC) $(QCFLAG) -stop .o -o $@ runtime.c--
endif

# claude: also depends on stdlib/'s own objects (copied in below) - the
# top-level Makefile's test-ppc always builds stdlib/ first, but without
# this, a rebuilt stdlib.o/stdlibcmm.o wouldn't trigger re-archiving here.
$(B)/stdlib.a: $(B)/gc.o $(B)/alloc.o $(STDLIBDIR)/stdlib.o $(STDLIBDIR)/stdlibcmm.o
	$(AR) cr $@ $(STDLIBDIR)/stdlib.o $(STDLIBDIR)/stdlibcmm.o $(B)/gc.o $(B)/alloc.o

# The Quick C-- run-time system, built from the sources qc-- installs into
# $(QCLIB). It is not shipped prebuilt because cut.c--, thread.c-- and
# yield.c-- have to be compiled with *our* global-variable declarations and
# *our* target: every C-- unit touching the global area references a
# Cmm.globalsig.<hash> computed from them, and tiger declares
# "bits32 alloc_ptr;". BACKEND is threaded through so the ppc build gets a
# byteorder-flipped, -ppc-elf-compiled qcmm.a of its own rather than
# reusing x86's.
#
# claude: qcmm.a has no real prerequisites of its own - it's a thin cp
# around a recursive $(MAKE) into $(QCLIB), whose *own* Makefile is what
# actually knows whether runtime.o/gcc-linux.o/etc. need rebuilding (see
# that Makefile's per-object header/qc dependencies). So this always
# recurses (FORCE, the standard recursive-make idiom - see the GNU Make
# manual's "Recursive Make" chapter) rather than trying to model qc--'s
# own build graph here; the sub-make no-ops on its own if nothing changed.
.PHONY: FORCE
FORCE:

$(B)/qcmm.a: FORCE | $(B)
	$(MAKE) -C $(QCLIB) B=$(CURDIR)/$(B)/qcmm BACKEND=$(BACKEND) QC=$(QC) CC32="$(CC)" \
	  GLOBALS_DECL='bits32 alloc_ptr;'
	cp $(B)/qcmm/libqcmm.a $@

clean::
	rm -f *.o *.a
	rm -rf qcmm build-ppc

clean::
	rm -f alloc.s runtime.s

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang C runtime.nw runtime.c--
	$(SYNCWEB) -lang C runtime.nw client.c
	$(SYNCWEB) -lang C gc.nw gc.c
	$(SYNCWEB) -lang C gc.nw gc.h
	$(SYNCWEB) -lang C gc.nw alloc.c--


lpdistclean::
	rm -f $(LPSRC) .md5sum_*
