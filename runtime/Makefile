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
# Top rules
##############################################################################

# See stdlib/Makefile for why CC32, -fno-omit-frame-pointer and -fcommon.
all:: stdlib.a runtime.o qcmm.a

gc.o: gc.c gc.h
	$(CC32) -w -fcommon -fno-omit-frame-pointer -I $(QCINCLUDE) -c gc.c -o gc.o

alloc.o: alloc.c--
	$(QC) -stop .o -o alloc.o alloc.c--

runtime.o: runtime.c--
	$(QC) -stop .o -o runtime.o runtime.c--

stdlib.a: gc.o alloc.o
	$(AR32) cr stdlib.a ../stdlib/stdlib.o ../stdlib/stdlibcmm.o gc.o alloc.o

# The Quick C-- run-time system, built from the sources qc-- installs into
# $(QCLIB). It is not shipped prebuilt because cut.c--, thread.c-- and
# yield.c-- have to be compiled with *our* global-variable declarations:
# every C-- unit touching the global area references a Cmm.globalsig.<hash>
# computed from them, and tiger declares "bits32 alloc_ptr;".
qcmm.a:
	$(MAKE) -C $(QCLIB) B=$(CURDIR)/qcmm QC=$(QC) CC32="$(CC32)" \
	  GLOBALS_DECL='bits32 alloc_ptr;'
	cp qcmm/libqcmm.a qcmm.a

clean::
	rm -f *.o *.a
	rm -rf qcmm

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
