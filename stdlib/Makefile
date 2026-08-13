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
# claude: BACKEND picks qc--'s target, same knob qc--'s own runtime Makefile
# (in $(QCLIB)) and tests/run-native.sh use. Default (x86) keeps producing
# the original flat filenames (stdlib.o, stdlibcmm.o) so the ordinary
# "make test" flow is untouched; ppc's outputs live under build-ppc/
# instead of clobbering the x86 ones.
BACKEND ?= x86
ifeq ($(BACKEND),x86)
CC     = $(CC32)
QCFLAG =
B      = .
else ifeq ($(BACKEND),ppc)
CC     = $(CC_PPC)
QCFLAG = -ppc-elf
B      = build-ppc
else
$(error unknown BACKEND=$(BACKEND) - expected x86 or ppc)
endif

##############################################################################
# Top rules
##############################################################################

# CC32/CC_PPC, not gcc: qc only emits 32-bit x86/ppc, so the host compiler
# cannot produce objects that link with its output unless the host happens
# to match.
#
# -fno-omit-frame-pointer is required, not cosmetic. This library allocates, so
# the stack can run C-- -> C -> C-- -> tig_call_gc, and the collector crosses
# that middle C frame by following its %ebp/r31 chain. Modern gcc uses the
# frame-pointer register as an ordinary register otherwise, which would
# strand the older C-- frames unscanned and collect objects that are still
# live.
#
# -fcommon because this is pre-GCC-10 C: its tentative definitions would
# otherwise become a definition per translation unit.
all:: $(B)/stdlib.o $(B)/stdlibcmm.o

$(B):
	mkdir -p $(B)

# claude: stdlib.o's real prerequisites also include qc--'s installed
# qc--runtime.h (stdlib.c #includes it via -I $(QCINCLUDE)) and, for
# stdlibcmm.o, the qc binary itself ($(QC) does the compiling, so a qc--
# fix with no local source change wouldn't otherwise be noticed). Without
# these, "make" at the top level silently reused stale objects after a
# qc-- rebuild+reinstall - see notes on this in the qc-- repo's ppc memory.
$(B)/stdlib.o: stdlib.c stdlib.h $(QCINCLUDE)/qc--runtime.h | $(B)
	$(CC) -w -fcommon -fno-omit-frame-pointer -I $(QCINCLUDE) \
	  -I ../runtime -c stdlib.c -o $@

# claude: stdlibcmm.c-- declares "target byteorder little"; qc refuses that
# mismatch for -ppc-elf ("metrics of source code don't match the target"),
# so for BACKEND=ppc flip it into a build-local copy first and compile
# that instead - same fix tests/run-native.sh already applies to cmm/*.c--.
ifeq ($(BACKEND),ppc)
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' stdlibcmm.c-- > $(B)/stdlibcmm.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/stdlibcmm.c--
else
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	$(QC) $(QCFLAG) -stop .o -o $@ stdlibcmm.c--
endif

clean::
	rm -f stdlib.o stdlibcmm.o
	rm -rf build-ppc

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
