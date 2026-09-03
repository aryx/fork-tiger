TOP=..

##############################################################################
# Variables
##############################################################################
-include $(TOP)/Makefile.config
-include $(TOP)/Makefile.backends

##############################################################################
# Backend selection
##############################################################################
# claude: BACKEND picks qc--'s target; see stdlib/Makefile's header for the
# fuller reasoning (including what XFORM's four values mean, and how
# CCVAR/XFORMVAR's "$($(...))" double lookup works - same idea here, plus
# AR). Default (x86) keeps the original flat filenames (runtime.o,
# stdlib.a, qcmm.a) so tests/run-tests.sh's existing paths are untouched;
# every other backend's outputs live under their own build-<backend>/
# instead, alongside stdlib/'s build-<backend>/.
#
# GLOBALS_DECL is qcmm.a's own client-globals declaration (see that rule's
# comment below) - bits64 iff XFORM=64, matching Frame.bits_str()'s "-64"
# choice in the frontend and XFORM=64's rewrite of alloc_ptr's own
# declaration in alloc.c--/runtime.c--/stdlibcmm.c--.
BACKEND ?= x86
ifeq ($(strip $(backend.$(BACKEND))),)
$(error unknown BACKEND=$(BACKEND) - expected x86 $(ARCHS))
endif
CCVAR        = $(backend.$(BACKEND))
ARVAR        = $(subst CC,AR,$(CCVAR))
XFORMVAR     = $(subst CC,XFORM,$(CCVAR))
CC           = $($(CCVAR))
AR           = $($(ARVAR))
XFORM        = $($(XFORMVAR))
QCFLAG       = $(if $(filter x86,$(BACKEND)),,-$(BACKEND))
B            = $(if $(filter x86,$(BACKEND)),.,build-$(BACKEND))
STDLIBDIR    = ../stdlib$(if $(filter x86,$(BACKEND)),,/build-$(BACKEND))
GLOBALS_DECL = $(if $(filter 64,$(XFORM)),bits64,bits32) alloc_ptr;

# See stdlib/Makefile's identical block for why this is needed for these
# backends and not x86/ppc.
ifneq ($(filter $(BACKEND),$(filter-out ppc,$(ARCHS))),)
export QC_AS := $(CC)
export QC_LD := $(CC)
endif

##############################################################################
# Top rules
##############################################################################

# See stdlib/Makefile for why CC (CC32/CC_PPC/...), -fno-omit-frame-pointer
# and -fcommon.
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

# claude: alloc.c--/runtime.c-- declare plain "target byteorder little"
# (no explicit wordsize/pointersize - qc--'s default is 32); same four
# XFORM cases as stdlib/Makefile's stdlibcmm.o rule, into a build-local
# copy so the checked-in sources stay backend-neutral. alloc.c-- alone
# also carries the allocator's alignment mask/round-up ("(size + 7) &
# 0xFFFFFFFC"), which XFORM=64 has to rescale from 4-byte to 8-byte
# alignment by hand (the generic bits32->bits64/+4->+8 substitutions don't
# touch "+ 7" or the literal mask) - see stdlib/Makefile's XFORM=64 comment
# and docs/claude_notes/notes_64bits.txt for the full reasoning, verified
# against qc--'s own tests/tiger64/regenerate-riscv64.sh by hand.
ifeq ($(XFORM),big)
$(B)/alloc.o: alloc.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' alloc.c-- > $(B)/alloc.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/alloc.c--

$(B)/runtime.o: runtime.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' runtime.c-- > $(B)/runtime.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/runtime.c--
else ifeq ($(XFORM),arm)
$(B)/alloc.o: alloc.c-- $(QC) | $(B)
	sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' \
	  alloc.c-- > $(B)/alloc.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/alloc.c--

$(B)/runtime.o: runtime.c-- $(QC) | $(B)
	sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' \
	  runtime.c-- > $(B)/runtime.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/runtime.c--
else ifeq ($(XFORM),64)
# claude: "+[ ]*4\([^0-9]\)" -> "+8\1", NOT "+[ ]*4\b" -> "+8" - \b (word
# boundary) is a GNU sed extension that macOS's BSD sed does not support at
# all, so on a Mac (arm64 is the only XFORM=64 backend that ever runs
# there) the old pattern silently matched nothing rather than erroring,
# leaving every "alloc_ptr + 4"/"str_ptr+4" untouched. Found the hard way:
# tig_alloc (below) returns "alloc_ptr + 4" as the pointer past its own
# 8-byte size header on a 64-bit target - unrewritten, every allocated
# object's data overlapped the last 4 bytes of its own header, corrupting
# nearly everything allocated through it (see docs/claude_notes/
# notes_64bits.txt's arm64 postscript for the full diagnosis). The fix
# captures and re-emits whatever single character follows "4" instead of
# asserting a word boundary - functionally identical, portable to both sed
# implementations (verified: neither GNU nor BSD sed treats a bracket
# expression like "\([^0-9]\)" specially, unlike "\b").
$(B)/alloc.o: alloc.c-- $(QC) | $(B)
	sed -e 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
	    -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' \
	    -e 's/(size + 7) & 0xFFFFFFFC/(size + 15) \& 0xFFFFFFFFFFFFFFF8/' \
	  alloc.c-- > $(B)/alloc.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/alloc.c--

$(B)/runtime.o: runtime.c-- $(QC) | $(B)
	sed -e 's/^target byteorder little;/target byteorder little wordsize 64 pointersize 64;/' \
	    -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' \
	  runtime.c-- > $(B)/runtime.c--
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
# "$(GLOBALS_DECL)" (bits64 rather than bits32 for the two 64-bit
# backends - $(QCLIB)'s own Makefile rewrites cut.c--/thread.c--/yield.c--
# to bits64 in that case too, keyed off the same BACKEND). BACKEND is
# threaded through so every non-x86 build gets its own transformed,
# correctly-flagged qcmm.a rather than reusing x86's.
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

# claude: QCLIB's own installed Makefile used to predate qc--'s arm64/amd64
# Linux/ELF rename and know only the plain BACKEND names "arm64"/"amd64" -
# meaning what this repo calls arm64-mach-o/amd64-mach-o - so this used to
# map the two -mach-o names down to those before calling it. QCLIB's
# Makefile has since grown its own native "arm64-mach-o"/"amd64-mach-o"
# BACKEND values (see its own ifeq chain), each picking the correctly
# underscore-prefixed CONTOBJ (arm64machocont.o/amd64machocont.o, not
# arm64cont.o/amd64cont.o - Darwin's C symbol-mangling adds a leading
# underscore that ELF hosts don't use, see arm64cont.s/arm64machocont.s's
# own comments) - so BACKEND is passed through unmapped now. Passing the
# old stripped name still "worked" (no error) but silently built the
# ELF-flavored CONTOBJ, leaving _Cmm_unwindcont_pc undefined at final link.
$(B)/qcmm.a: FORCE | $(B)
	$(MAKE) -C $(QCLIB) B=$(CURDIR)/$(B)/qcmm BACKEND=$(BACKEND) QCFLAG='$(QCFLAG)' \
	  QC=$(QC) CC32="$(CC)" GLOBALS_DECL='$(GLOBALS_DECL)'
	cp $(B)/qcmm/libqcmm.a $@

clean::
	rm -f *.o *.a
	rm -rf qcmm $(addprefix build-,$(ARCHS))

clean::
	rm -f alloc.s runtime.s
