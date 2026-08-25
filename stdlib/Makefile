TOP=..

##############################################################################
# Variables
##############################################################################
-include $(TOP)/Makefile.config

##############################################################################
# Backend selection
##############################################################################
# claude: BACKEND picks qc--'s target, same knob qc--'s own runtime Makefile
# (in $(QCLIB)) and tests/run-tests.sh use. Default (x86) keeps producing
# the original flat filenames (stdlib.o, stdlibcmm.o) so the ordinary
# "make test" flow is untouched; every other backend's outputs live under
# their own build-<backend>/ instead of clobbering the x86 ones.
#
# XFORM picks which transform stdlibcmm.c-- (the one C-- source here) needs
# before qc will accept it for that target - see the $(B)/stdlibcmm.o rule
# below for what each one actually does:
#   none - byteorder/metrics already match qc--'s bits32-little default
#   big  - byteorder little -> big (ppc, sparc: 32-bit big-endian)
#   arm  - splice float "none" (arm has no FPU; every source here otherwise
#          relies on the implicit ieee754 default)
#   64   - the full bits32->bits64 rewrite (alpha, riscv64: 64-bit targets)
BACKEND ?= x86
ifeq ($(BACKEND),x86)
CC     = $(CC32)
QCFLAG =
B      = .
XFORM  = none
else ifeq ($(BACKEND),ppc)
CC     = $(CC_PPC)
QCFLAG = -ppc-elf
B      = build-ppc
XFORM  = big
else ifeq ($(BACKEND),sparc)
CC     = $(CC_SPARC)
QCFLAG = -sparc
B      = build-sparc
XFORM  = big
else ifeq ($(BACKEND),alpha)
CC     = $(CC_ALPHA)
QCFLAG = -alpha
B      = build-alpha
XFORM  = 64
else ifeq ($(BACKEND),mips)
CC     = $(CC_MIPS)
QCFLAG = -mips
B      = build-mips
XFORM  = none
else ifeq ($(BACKEND),arm)
CC     = $(CC_ARM)
QCFLAG = -arm
B      = build-arm
XFORM  = arm
else ifeq ($(BACKEND),riscv32)
CC     = $(CC_RISCV32)
QCFLAG = -riscv32
B      = build-riscv32
XFORM  = none
else ifeq ($(BACKEND),riscv64)
CC     = $(CC_RISCV64)
QCFLAG = -riscv64
B      = build-riscv64
XFORM  = 64
else ifeq ($(BACKEND),arm64)
# claude: also 64-bit, like alpha/riscv64 above - same XFORM=64. Unlike
# every backend above, CC_ARM64 needs no cross toolchain at all on an
# Apple Silicon Mac (see runtime/Makefile's identical arm64 comment).
CC     = $(CC_ARM64)
QCFLAG = -arm64
B      = build-arm64
XFORM  = 64
else
$(error unknown BACKEND=$(BACKEND) - expected x86, ppc, sparc, alpha, mips, arm, riscv32, riscv64 or arm64)
endif

# claude: qc drives an external assembler/linker, defaulting to clang
# (able to target i386 and ppc from any host - see "qc -help"'s -as/-ld
# entry - which is why x86/ppc need no override here). clang has no
# working backend for the rest of these (confirmed by hand for sparc: it
# accepts "-target sparc-unknown-linux-gnu" but then rejects qc--'s own
# emitted assembly, e.g. "call foo, 0" - not a supported syntax), so point
# qc at the real cross toolchain instead, same as qc--'s own tests/
# run-tiger-<arch>.sh scripts do.
ifeq ($(filter $(BACKEND),sparc alpha mips arm riscv32 riscv64 arm64),$(BACKEND))
export QC_AS := $(CC)
export QC_LD := $(CC)
endif

##############################################################################
# Top rules
##############################################################################

# CC (CC32/CC_PPC/CC_SPARC/...), not gcc: qc only emits code for the target
# named by QCFLAG, so the host compiler cannot produce objects that link
# with its output unless the host happens to match.
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

# claude: stdlibcmm.c-- declares "target byteorder little memsize 8 wordsize
# 32 pointersize 32"; qc refuses a metrics/byteorder mismatch against the
# chosen target ("metrics of source code don't match the target"), so for
# XFORM != none this rewrites it into a build-local copy first and compiles
# that instead - same fix runtime/Makefile's alloc.o/runtime.o rules and
# tests/run-tests.sh apply to their own C-- sources. The XFORM=64 case
# (alpha, riscv64) is the biggest of the four: unlike a plain metrics-line
# splice, it also rewrites every bits32 type in the source to bits64 and
# every "+4"/"+ 4" header-skip offset to "+8", plus the allocator-specific
# alignment mask/round-up in ../runtime/alloc.c-- and the EOF sentinel/
# curr_exn alignment here - see docs/claude_notes/notes_64bits.txt and
# qc--'s own tests/tiger64/regenerate-riscv64.sh, which this mirrors
# exactly (verified against it by hand).
ifeq ($(XFORM),big)
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	sed 's/byteorder[ ][ ]*little/byteorder big/' stdlibcmm.c-- > $(B)/stdlibcmm.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/stdlibcmm.c--
else ifeq ($(XFORM),arm)
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' \
	  stdlibcmm.c-- > $(B)/stdlibcmm.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/stdlibcmm.c--
else ifeq ($(XFORM),64)
# claude: "+[ ]*4\b" -> "+8" no longer uses \b - see runtime/Makefile's
# identical XFORM=64 comment for why (macOS's BSD sed doesn't support \b
# at all, so the substitution silently matched nothing there, corrupting
# tig_alloc's own returned pointer).
#
# tig_getchar's EOF check ("if (ch == 0xFFFFFFFF)") is a SEPARATE, subtler
# case: whether that literal needs widening to 0xFFFFFFFFFFFFFFFF at all
# is backend-specific, not just a \b portability bug - it depends on how
# each 64-bit backend's calling convention hands back a 32-bit C `int`
# return value (getchar()'s actual C type) in a 64-bit register.
# Empirically confirmed by a throwaway C-- probe (compare ch against both
# widenings directly, see the arm64 build's own commit message for the
# exact test): on -riscv64 (and, unverified but assumed from the same
# LP64 calling-convention family, -alpha) a 32-bit int return gets SIGN-
# extended, so ch really is 0xFFFFFFFFFFFFFFFF and needs the rewrite; on
# -arm64, AArch64's architectural rule that writing a 32-bit (W) register
# always ZERO-extends the corresponding 64-bit (X) register means ch is
# actually 0x00000000FFFFFFFF - rewriting to all-Fs there made the EOF
# check never fire, which silently broke every test reading from an empty
# stdin (e.g. tests/merge.tig's readlist() never terminating its list,
# printing a stray extra line). So: rewrite for alpha/riscv64, leave
# untouched for arm64.
ifeq ($(BACKEND),arm64)
EOF_SENTINEL_SED =
else
EOF_SENTINEL_SED = -e 's/0xFFFFFFFF\([^0-9A-Fa-f]\)/0xFFFFFFFFFFFFFFFF\1/g'
endif
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	sed -e 's/wordsize 32 pointersize 32/wordsize 64 pointersize 64/' \
	    -e 's/bits32/bits64/g' -e 's/+[ ]*4\([^0-9]\)/+8\1/g' \
	    -e 's/align 4;/align 8;/' $(EOF_SENTINEL_SED) \
	  stdlibcmm.c-- > $(B)/stdlibcmm.c--
	$(QC) $(QCFLAG) -stop .o -o $@ $(B)/stdlibcmm.c--
else
$(B)/stdlibcmm.o: stdlibcmm.c-- $(QC) | $(B)
	$(QC) $(QCFLAG) -stop .o -o $@ stdlibcmm.c--
endif

clean::
	rm -f stdlib.o stdlibcmm.o
	rm -rf build-ppc build-sparc build-alpha build-mips build-arm build-riscv32 build-riscv64 build-arm64

clean::
	rm -f o.o

clean::
	rm -f stdlibcmm.s

