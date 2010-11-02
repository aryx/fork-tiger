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

all::
	gcc -Wall -I $(QCINCLUDE)  -o gc.o  -c gc.c
	$(QC) -stop .o -o alloc.o  alloc.c--
	ar cr stdlib.a ../stdlib/stdlib.o ../stdlib/stdlibcmm.o gc.o alloc.o
	$(QC) -stop .o -o runtime.o runtime.c--

clean::
	rm -f *.o *.a

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
