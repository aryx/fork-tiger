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

# CC32, not gcc: qc only emits 32-bit x86, so the host compiler cannot produce
# objects that link with its output unless the host happens to be x86.
#
# -fno-omit-frame-pointer is required, not cosmetic. This library allocates, so
# the stack can run C-- -> C -> C-- -> tig_call_gc, and the collector crosses
# that middle C frame by following its %ebp chain. Modern gcc uses %ebp as an
# ordinary register, which would strand the older C-- frames unscanned and
# collect objects that are still live.
#
# -fcommon because this is pre-GCC-10 C: its tentative definitions would
# otherwise become a definition per translation unit.
all::
	$(CC32) -w -fcommon -fno-omit-frame-pointer -I $(QCINCLUDE) \
	  -I ../runtime -c stdlib.c -o stdlib.o
	$(QC) -stop .o -o stdlibcmm.o stdlibcmm.c--

clean::
	rm -f stdlib.o stdlibcmm.o

clean::
	rm -f o.o

clean::
	rm -f stdlibcmm.s

##############################################################################
# Literate Programming rules
##############################################################################

sync:
	$(SYNCWEB) -lang C stdlib.nw stdlib.h
	$(SYNCWEB) -lang C stdlib.nw stdlib.c
	$(SYNCWEB) -lang C stdlib.nw stdlibcmm.c--

lpdistclean::
	rm -f $(LPSRC) .md5sum_*
