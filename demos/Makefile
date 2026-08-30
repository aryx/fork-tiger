# claude: build and run the Tiger demo programs in this directory
# (hello.tig, forloop.tig, queens.tig) end to end, for every backend qc--
# supports (see "qc --help"): its default x86, plus -ppc-elf, -sparc,
# -alpha, -mips, -arm, -riscv32 and -riscv64. Modeled on
# ~/c--/demos/Makefile, adapted for the extra front-end stage and the
# runtime/GC system a Tiger program needs that a bare C-- one (like that
# Makefile's hello.c--) does not.
#
# This is the "poke at one demo by hand" version of the pipeline
# tests/run-tests.sh already automates over the whole test suite; see that
# script if what you want is the full behavioural suite with a recorded
# baseline instead of a couple of demos, and for the fuller version of the
# per-backend XFORM/TIGERFLAG reasoning skimmed here.
#
# ---------------------------------------------------------------------------
# REQUIREMENTS
# ---------------------------------------------------------------------------
#
# 1. tigerc, this compiler, built and reachable at ../bin/tigerc:
#
#      cd .. && dune build
#
# 2. qc-- installed and ../configure run, which writes ../Makefile.config
#    (QC, CC32, CC_PPC, CC_SPARC, CC_ALPHA, CC_MIPS, CC_ARM, CC_RISCV32,
#    CC_RISCV64, RUN32, RUN_PPC, ...).
#
# 3. A cross toolchain per target you want - NOT gcc-multilib/g++-multilib,
#    which add secondary ABIs of the *host's own* architecture family (so
#    on aarch64 they offer 32-bit ARM, never i386 or powerpc). See
#    ../configure's own per-backend comments for the exact apt package per
#    target (its warnings when a toolchain is missing repeat them too);
#    riscv32 alone is a bare-metal/picolibc toolchain rather than a glibc
#    one - see the riscv32 section below.
#
# 4. To *run* the results on a host that isn't natively the target
#    architecture, qemu's user-mode emulators:
#
#      sudo apt install qemu-user
#
#    RUN32/RUN_PPC/RUN_SPARC/... (from ../Makefile.config) name the qemu
#    binary explicitly rather than relying on binfmt_misc, which is a
#    host-wide registration a container inherits only by accident - the
#    kernel resolves the interpreter path in the container's own mount
#    namespace, so the same image runs on one machine and not another. On
#    a host genuinely native to a given target, ../configure already
#    leaves that RUN_<ARCH> empty, so the binary just runs directly.
#
#    We link -static, so no sysroot/-L argument is required to run any of
#    these binaries (riscv32's freestanding link is its own case - see
#    below).
#
# ---------------------------------------------------------------------------

TOP = ..

ifeq (,$(wildcard $(TOP)/Makefile.config))
$(error demos/Makefile: no $(TOP)/Makefile.config; run $(TOP)/configure first)
endif
-include $(TOP)/Makefile.config

TIGERC ?= $(TOP)/bin/tigerc
DEMOS   = hello forloop queens

BX  = build
BP  = build-ppc
BS  = build-sparc
BA  = build-alpha
BM  = build-mips
BAR = build-arm
BR32 = build-riscv32
BR64 = build-riscv64
BAR64 = build-arm64
BAMD64 = build-amd64
BAR64MO = build-arm64-mach-o
BAMD64MO = build-amd64-mach-o

.PHONY: all x86 ppc sparc alpha mips arm riscv32 riscv64 arm64 amd64 arm64-mach-o amd64-mach-o \
        run run-x86 run-ppc run-sparc run-alpha run-mips run-arm run-riscv32 run-riscv64 run-arm64 \
        run-amd64 run-arm64-mach-o run-amd64-mach-o \
        rt rt-ppc rt-sparc rt-alpha rt-mips rt-arm rt-riscv32 rt-riscv64 rt-arm64 \
        rt-amd64 rt-arm64-mach-o rt-amd64-mach-o clean

# claude: without this, make treats the .c--/.o files below as disposable
# intermediates of the pattern-rule chain (.tig -> .c-- -> .o -> binary)
# and silently deletes them after each build - annoying since the .c-- is
# useful to eyeball and deleting the .o forces a full qc rerun even when
# only the link step needs redoing.
.SECONDARY:

# claude: amd64 (Linux/ELF) joins the default "all" - the two -mach-o
# variants don't, same Darwin-only reasoning as the "run" aggregate below.
all: x86 ppc sparc alpha mips arm riscv32 riscv64 arm64 amd64

$(BX) $(BP) $(BS) $(BA) $(BM) $(BAR) $(BR32) $(BR64) $(BAR64) $(BAMD64) $(BAR64MO) $(BAMD64MO):
	mkdir -p $@

# The runtime/stdlib system these demos link against (GC, alloc, print,
# ...) - see ../stdlib/Makefile and ../runtime/Makefile. Not file-based
# prerequisites of the demo binaries below: those Makefiles already track
# their own staleness, so it is cheap to always ask and let them no-op.
rt:
	$(MAKE) -C $(TOP)/stdlib
	$(MAKE) -C $(TOP)/runtime

rt-ppc:
	$(MAKE) -C $(TOP)/stdlib BACKEND=ppc
	$(MAKE) -C $(TOP)/runtime BACKEND=ppc

rt-sparc:
	$(MAKE) -C $(TOP)/stdlib BACKEND=sparc
	$(MAKE) -C $(TOP)/runtime BACKEND=sparc

rt-alpha:
	$(MAKE) -C $(TOP)/stdlib BACKEND=alpha
	$(MAKE) -C $(TOP)/runtime BACKEND=alpha

rt-mips:
	$(MAKE) -C $(TOP)/stdlib BACKEND=mips
	$(MAKE) -C $(TOP)/runtime BACKEND=mips

rt-arm:
	$(MAKE) -C $(TOP)/stdlib BACKEND=arm
	$(MAKE) -C $(TOP)/runtime BACKEND=arm

rt-riscv32:
	$(MAKE) -C $(TOP)/stdlib BACKEND=riscv32
	$(MAKE) -C $(TOP)/runtime BACKEND=riscv32

rt-riscv64:
	$(MAKE) -C $(TOP)/stdlib BACKEND=riscv64
	$(MAKE) -C $(TOP)/runtime BACKEND=riscv64

rt-arm64:
	$(MAKE) -C $(TOP)/stdlib BACKEND=arm64
	$(MAKE) -C $(TOP)/runtime BACKEND=arm64

rt-amd64:
	$(MAKE) -C $(TOP)/stdlib BACKEND=amd64
	$(MAKE) -C $(TOP)/runtime BACKEND=amd64

rt-arm64-mach-o:
	$(MAKE) -C $(TOP)/stdlib BACKEND=arm64-mach-o
	$(MAKE) -C $(TOP)/runtime BACKEND=arm64-mach-o

rt-amd64-mach-o:
	$(MAKE) -C $(TOP)/stdlib BACKEND=amd64-mach-o
	$(MAKE) -C $(TOP)/runtime BACKEND=amd64-mach-o

# ---------------------------------------------------------------------------
# x86
# ---------------------------------------------------------------------------
# tigerc is the front end under test: .tig -> generic C--.
$(BX)/%.c--: %.tig | $(BX)
	$(TIGERC) $< > $@

# .c-- -> .o. -globals goes here and nowhere else: the global-variable area
# is one object per program, and runtime.o/stdlib.a/qcmm.a were built
# without it - see tests/run-tests.sh for the fuller version of this note.
$(BX)/%.o: $(BX)/%.c-- | $(BX)
	$(QC) -globals -stop .o -o $@ $<

# runtime.o first: it supplies main, and by qc--(1)'s convention the unit
# holding main comes first so the C-- globals work out.
$(BX)/%: $(BX)/%.o
	$(CC32) -static $(TOP)/runtime/runtime.o $< \
	  $(TOP)/runtime/stdlib.a $(TOP)/runtime/qcmm.a $(QCPCMAP) -o $@

x86: rt $(foreach d,$(DEMOS),$(BX)/$(d))

run-x86: x86
	@for d in $(DEMOS); do \
	  echo "=== $$d (x86) ==="; \
	  $(RUN32) ./$(BX)/$$d; echo "[exit status $$?]"; \
	done

# ---------------------------------------------------------------------------
# ppc (byteorder big, the -ppc-elf backend)
# ---------------------------------------------------------------------------
# claude: tigerc always emits "target byteorder little" (see
# backend/codegen.ml); qc refuses that mismatch for -ppc-elf ("metrics of
# source code don't match the target"), so flip it here the same way
# ../runtime/Makefile, ../stdlib/Makefile and tests/run-tests.sh already do
# for their own C-- sources/output.
$(BP)/%.c--: %.tig | $(BP)
	$(TIGERC) $< | sed 's/byteorder[ ][ ]*little/byteorder big/' > $@

$(BP)/%.o: $(BP)/%.c-- | $(BP)
	$(QC) -ppc-elf -globals -stop .o -o $@ $<

$(BP)/%: $(BP)/%.o
	$(CC_PPC) -static $(TOP)/runtime/build-ppc/runtime.o $< \
	  $(TOP)/runtime/build-ppc/stdlib.a $(TOP)/runtime/build-ppc/qcmm.a $(QCPCMAP) -o $@

# claude: "$(MAKE) -k ... || true" rather than a plain prerequisite list -
# every non-x86 backend below uses the same shape. ppc itself is all-green
# now (see tests/expected/tiger-ppc.txt), but several others are not (see
# CLAUDE.md's Architectures section and the other expected/tiger-<arch>.txt
# baselines) - "-k" keeps building the remaining demos past the first
# failure instead of the whole "make <arch>" bailing out, and the trailing
# "|| true" stops that sub-make's own failure from failing this target.
ppc: rt-ppc
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BP)/$(d)) || true

run-ppc: ppc
	@for d in $(DEMOS); do \
	  if [ -x $(BP)/$$d ]; then \
	    echo "=== $$d (ppc) ==="; \
	    $(RUN_PPC) ./$(BP)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (ppc): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# sparc (byteorder big, the -sparc backend, SPARC V8)
# ---------------------------------------------------------------------------
# claude: same byteorder flip as ppc above. QC_AS/QC_LD point qc at the
# real cross assembler/linker instead of its clang default - clang accepts
# "-target sparc-unknown-linux-gnu" but then rejects qc--'s own emitted
# assembly (e.g. "call foo, 0" - not syntax clang's sparc backend
# understands), confirmed by hand; ppc/x86 need no such override because
# clang's own backend for them is what "qc -help"'s -as/-ld default
# describes. See ../runtime/Makefile's identical comment.
$(BS)/%.c--: %.tig | $(BS)
	$(TIGERC) $< | sed 's/byteorder[ ][ ]*little/byteorder big/' > $@

$(BS)/%.o: $(BS)/%.c-- | $(BS)
	QC_AS='$(CC_SPARC)' QC_LD='$(CC_SPARC)' $(QC) -sparc -globals -stop .o -o $@ $<

$(BS)/%: $(BS)/%.o
	$(CC_SPARC) -static $(TOP)/runtime/build-sparc/runtime.o $< \
	  $(TOP)/runtime/build-sparc/stdlib.a $(TOP)/runtime/build-sparc/qcmm.a $(QCPCMAP) -o $@

sparc: rt-sparc
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BS)/$(d)) || true

run-sparc: sparc
	@for d in $(DEMOS); do \
	  if [ -x $(BS)/$$d ]; then \
	    echo "=== $$d (sparc) ==="; \
	    $(RUN_SPARC) ./$(BS)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (sparc): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# alpha (byteorder little, wordsize/pointersize 64, the -alpha backend)
# ---------------------------------------------------------------------------
# claude: alpha is little-endian, so no byteorder flip - but it is 64-bit,
# so tigerc itself needs "-64" to emit bits64 C-- (wordsize/pointersize
# 64) in the first place; that flag also fixes up the two codegen bugs
# described in docs/claude_notes/notes_64bits.txt. See ../runtime/Makefile
# and ../stdlib/Makefile's XFORM=64 for why this repo's own hand-written
# runtime/stdlib .c-- sources need a heavier bits32->bits64 rewrite that
# tigerc's own "-64" output does not.
$(BA)/%.c--: %.tig | $(BA)
	$(TIGERC) -64 $< > $@

$(BA)/%.o: $(BA)/%.c-- | $(BA)
	QC_AS='$(CC_ALPHA)' QC_LD='$(CC_ALPHA)' $(QC) -alpha -globals -stop .o -o $@ $<

$(BA)/%: $(BA)/%.o
	$(CC_ALPHA) -static $(TOP)/runtime/build-alpha/runtime.o $< \
	  $(TOP)/runtime/build-alpha/stdlib.a $(TOP)/runtime/build-alpha/qcmm.a $(QCPCMAP) -o $@

alpha: rt-alpha
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BA)/$(d)) || true

run-alpha: alpha
	@for d in $(DEMOS); do \
	  if [ -x $(BA)/$$d ]; then \
	    echo "=== $$d (alpha) ==="; \
	    $(RUN_ALPHA) ./$(BA)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (alpha): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# mips (byteorder little, mipsel, the -mips backend)
# ---------------------------------------------------------------------------
# claude: mips is 32-bit little-endian, matching tigerc's own default
# output exactly - no transform needed at all, like x86.
$(BM)/%.c--: %.tig | $(BM)
	$(TIGERC) $< > $@

$(BM)/%.o: $(BM)/%.c-- | $(BM)
	QC_AS='$(CC_MIPS)' QC_LD='$(CC_MIPS)' $(QC) -mips -globals -stop .o -o $@ $<

$(BM)/%: $(BM)/%.o
	$(CC_MIPS) -static $(TOP)/runtime/build-mips/runtime.o $< \
	  $(TOP)/runtime/build-mips/stdlib.a $(TOP)/runtime/build-mips/qcmm.a $(QCPCMAP) -o $@

mips: rt-mips
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BM)/$(d)) || true

run-mips: mips
	@for d in $(DEMOS); do \
	  if [ -x $(BM)/$$d ]; then \
	    echo "=== $$d (mips) ==="; \
	    $(RUN_MIPS) ./$(BM)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (mips): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# arm (byteorder little, no FPU, the -arm backend)
# ---------------------------------------------------------------------------
# claude: arm is 32-bit little-endian, matching tigerc's own default
# output's byteorder, but arm.ml's T.float = Float.none (no FPU) while
# every C-- source here relies on the implicit ieee754 default, so a
# float "none" pragma has to be spliced in instead of a byteorder flip -
# same mismatch ../runtime/Makefile's XFORM=arm comment and qc--'s own
# tests/run-tiger-arm.sh describe.
$(BAR)/%.c--: %.tig | $(BAR)
	$(TIGERC) $< | sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' > $@

$(BAR)/%.o: $(BAR)/%.c-- | $(BAR)
	QC_AS='$(CC_ARM)' QC_LD='$(CC_ARM)' $(QC) -arm -globals -stop .o -o $@ $<

$(BAR)/%: $(BAR)/%.o
	$(CC_ARM) -static $(TOP)/runtime/build-arm/runtime.o $< \
	  $(TOP)/runtime/build-arm/stdlib.a $(TOP)/runtime/build-arm/qcmm.a $(QCPCMAP) -o $@

arm: rt-arm
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BAR)/$(d)) || true

run-arm: arm
	@for d in $(DEMOS); do \
	  if [ -x $(BAR)/$$d ]; then \
	    echo "=== $$d (arm) ==="; \
	    $(RUN_ARM) ./$(BAR)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (arm): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# riscv32 (byteorder little, the -riscv32 backend, RV32IMAC/ilp32)
# ---------------------------------------------------------------------------
# claude: riscv32 is 32-bit little-endian and has an FPU per arch/
# riscv32/riscv32.ml (ieee754, matching the implicit default) - no
# transform needed, like mips. What makes riscv32 different is its
# toolchain and link step: Ubuntu ships no riscv32-linux-gnu glibc at
# all, so CC_RISCV32 is a bare-metal (gcc-riscv64-unknown-elf) compiler
# against picolibc, and the final link cannot go through $(CC_RISCV32)
# (gcc + --specs=picolibc.specs) - see the link recipe below for the two
# independent reasons, both found (and fixed) the same way in qc--'s own
# tests/run-tiger-riscv32.sh, which this mirrors.
$(BR32)/%.c--: %.tig | $(BR32)
	$(TIGERC) $< > $@

$(BR32)/%.o: $(BR32)/%.c-- | $(BR32)
	QC_AS='$(CC_RISCV32)' QC_LD='$(CC_RISCV32)' $(QC) -riscv32 -globals -stop .o -o $@ $<

# claude: the freestanding entry point - riscv32_crt0.o must never be
# archived into a .a (a linker only pulls an archive member in for a
# referenced undefined symbol, and nothing ever references "_start" by
# name - it is found via entry-point lookup, a different mechanism), so
# it is built once here and passed as a plain object on the final link
# line instead, exactly like qc--'s own tests/run-tiger-riscv32.sh.
$(BR32)/riscv32_crt0.o: $(QCINCLUDE)/riscv32_crt0.s | $(BR32)
	$(CC_RISCV32) -c $< -o $@

# claude: plain `ld`, NOT $(CC_RISCV32) (gcc + --specs=picolibc.specs) -
# two independent reasons, both found the hard way (see qc--'s own
# tests/run-tiger-riscv32.sh header comment for the full story): (1)
# picolibc.specs' link spec injects "-Tpicolibc.ld" unless -T is given
# explicitly, a bare-metal-style memory map at a load address plain
# qemu-riscv32 user-mode emulation cannot run; (2) it also adds
# "--gc-sections" unconditionally, which silently discards individual
# .pcmap entries even though pcmap.ld's own Cmm_pc_map/Cmm_pc_map_limit
# symbols still bound a correctly-sized region - --gc-sections's liveness
# analysis doesn't understand that a linker-script address-range
# reference (not an ordinary symbol reference) is what keeps a .pcmap
# entry meaningful. Plain ld sidesteps both; -lc/-lgcc and their search
# paths (normally supplied by the specs file) are derived and added
# explicitly instead, so this survives a toolchain version bump unchanged.
RISCV32_MARCH = -march=rv32imac -mabi=ilp32
RISCV32_GCCLIBDIR = $(shell dirname "$$(riscv64-unknown-elf-gcc $(RISCV32_MARCH) -print-libgcc-file-name)")
RISCV32_MULTIDIR  = $(shell riscv64-unknown-elf-gcc $(RISCV32_MARCH) -print-multi-directory)
RISCV32_PICOLIBDIR = /usr/lib/picolibc/riscv64-unknown-elf/lib/$(RISCV32_MULTIDIR)

$(BR32)/%: $(BR32)/%.o $(BR32)/riscv32_crt0.o
	riscv64-unknown-elf-ld -m elf32lriscv -static -e _start \
	  -L$(RISCV32_PICOLIBDIR) -L$(RISCV32_GCCLIBDIR) \
	  $(BR32)/riscv32_crt0.o $(TOP)/runtime/build-riscv32/runtime.o $< \
	  $(TOP)/runtime/build-riscv32/stdlib.a $(TOP)/runtime/build-riscv32/qcmm.a $(QCPCMAP) \
	  --start-group -lc -lgcc --end-group \
	  -o $@

riscv32: rt-riscv32
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BR32)/$(d)) || true

run-riscv32: riscv32
	@for d in $(DEMOS); do \
	  if [ -x $(BR32)/$$d ]; then \
	    echo "=== $$d (riscv32) ==="; \
	    $(RUN_RISCV32) ./$(BR32)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (riscv32): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# riscv64 (byteorder little, wordsize/pointersize 64, the -riscv64 backend)
# ---------------------------------------------------------------------------
# claude: same "-64" reasoning as alpha above - little-endian, so no
# byteorder flip, but 64-bit, so tigerc needs "-64" to emit bits64 C--.
# Unlike riscv32, Ubuntu ships a real riscv64-linux-gnu glibc cross
# toolchain, so this links the ordinary way ($(CC_RISCV64) -static), not
# picolibc/freestanding.
$(BR64)/%.c--: %.tig | $(BR64)
	$(TIGERC) -64 $< > $@

$(BR64)/%.o: $(BR64)/%.c-- | $(BR64)
	QC_AS='$(CC_RISCV64)' QC_LD='$(CC_RISCV64)' $(QC) -riscv64 -globals -stop .o -o $@ $<

$(BR64)/%: $(BR64)/%.o
	$(CC_RISCV64) -static $(TOP)/runtime/build-riscv64/runtime.o $< \
	  $(TOP)/runtime/build-riscv64/stdlib.a $(TOP)/runtime/build-riscv64/qcmm.a $(QCPCMAP) -o $@

riscv64: rt-riscv64
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BR64)/$(d)) || true

run-riscv64: riscv64
	@for d in $(DEMOS); do \
	  if [ -x $(BR64)/$$d ]; then \
	    echo "=== $$d (riscv64) ==="; \
	    $(RUN_RISCV64) ./$(BR64)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (riscv64): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# arm64 (byteorder little, wordsize/pointersize 64, the -arm64 backend,
# Linux/ELF - the bare/default flag of qc--'s arm64 pair)
# ---------------------------------------------------------------------------
# claude: same "-64" reasoning as alpha/riscv64 above - little-endian, so
# no byteorder flip, but 64-bit, so tigerc needs "-64" to emit bits64 C--.
# Native on an aarch64-linux host (this dev box included) - no cross
# toolchain, no qemu; via CC_ARM64/RUN_ARM64 otherwise. Ordinary
# Linux-hosted link ($(CC) -static ... $(QCPCMAP)), exactly like riscv64
# above - unlike the OLD arm64 (see arm64-mach-o below), this one runs on
# real glibc/ELF, no Apple-specific link exceptions needed.
$(BAR64)/%.c--: %.tig | $(BAR64)
	$(TIGERC) -64 $< > $@

$(BAR64)/%.o: $(BAR64)/%.c-- | $(BAR64)
	QC_AS='$(CC_ARM64)' QC_LD='$(CC_ARM64)' $(QC) -arm64 -globals -stop .o -o $@ $<

$(BAR64)/%: $(BAR64)/%.o
	$(CC_ARM64) -static $(TOP)/runtime/build-arm64/runtime.o $< \
	  $(TOP)/runtime/build-arm64/stdlib.a $(TOP)/runtime/build-arm64/qcmm.a $(QCPCMAP) -o $@

arm64: rt-arm64
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BAR64)/$(d)) || true

run-arm64: arm64
	@for d in $(DEMOS); do \
	  if [ -x $(BAR64)/$$d ]; then \
	    echo "=== $$d (arm64) ==="; \
	    $(RUN_ARM64) ./$(BAR64)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (arm64): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# amd64 (byteorder little, wordsize/pointersize 64, the -amd64 backend,
# Linux/ELF - the bare/default flag of qc--'s amd64 pair)
# ---------------------------------------------------------------------------
# claude: same shape as arm64 above - real glibc/ELF, ordinary
# $(CC) -static ... $(QCPCMAP) link. Needs qemu-x86_64 to run on a non-x86_64
# host (this dev box included); native on an x86-64 Linux host.
$(BAMD64)/%.c--: %.tig | $(BAMD64)
	$(TIGERC) -64 $< > $@

$(BAMD64)/%.o: $(BAMD64)/%.c-- | $(BAMD64)
	QC_AS='$(CC_AMD64)' QC_LD='$(CC_AMD64)' $(QC) -amd64 -globals -stop .o -o $@ $<

$(BAMD64)/%: $(BAMD64)/%.o
	$(CC_AMD64) -static $(TOP)/runtime/build-amd64/runtime.o $< \
	  $(TOP)/runtime/build-amd64/stdlib.a $(TOP)/runtime/build-amd64/qcmm.a $(QCPCMAP) -o $@

amd64: rt-amd64
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BAMD64)/$(d)) || true

run-amd64: amd64
	@for d in $(DEMOS); do \
	  if [ -x $(BAMD64)/$$d ]; then \
	    echo "=== $$d (amd64) ==="; \
	    $(RUN_AMD64) ./$(BAMD64)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (amd64): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# arm64-mach-o (byteorder little, wordsize/pointersize 64, the
# -arm64-mach-o backend) - the OLD arm64 behaviour, before qc-- grew a
# Linux/ELF -arm64 (see qc--'s docs/claude_notes/notes_arm64.txt)
# ---------------------------------------------------------------------------
# claude: this is this host's own native architecture whenever it's an
# Apple Silicon Mac: no cross toolchain (CC_ARM64_MACHO is plain "clang"),
# no QC_AS/QC_LD override needed (qc--'s own -arm64-mach-o default is
# already "clang" - set anyway for uniformity with every backend above), no
# "-static" (Apple does not support static-linking libSystem at all,
# unlike every Linux-hosted backend here), and no $(QCPCMAP) on the link
# line: that linker-script fragment is GNU-ld-specific (Mach-O's ld64 has
# no "-T"/linker-script mechanism at all) - qc--'s own -arm64-mach-o
# backend doesn't need one, see its docs/claude_notes/notes_arm64.txt for
# why (ld64 auto-synthesizes the section-bounds symbols pcmap.ld would
# otherwise define). Only ever buildable on Darwin - CC_ARM64_MACHO is
# empty everywhere else, so this target's own sub-make will fail there,
# same as every other missing optional toolchain.
$(BAR64MO)/%.c--: %.tig | $(BAR64MO)
	$(TIGERC) -64 $< > $@

$(BAR64MO)/%.o: $(BAR64MO)/%.c-- | $(BAR64MO)
	QC_AS='$(CC_ARM64_MACHO)' QC_LD='$(CC_ARM64_MACHO)' $(QC) -arm64-mach-o -globals -stop .o -o $@ $<

$(BAR64MO)/%: $(BAR64MO)/%.o
	$(CC_ARM64_MACHO) $(TOP)/runtime/build-arm64-mach-o/runtime.o $< \
	  $(TOP)/runtime/build-arm64-mach-o/stdlib.a $(TOP)/runtime/build-arm64-mach-o/qcmm.a -o $@

arm64-mach-o: rt-arm64-mach-o
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BAR64MO)/$(d)) || true

run-arm64-mach-o: arm64-mach-o
	@for d in $(DEMOS); do \
	  if [ -x $(BAR64MO)/$$d ]; then \
	    echo "=== $$d (arm64-mach-o) ==="; \
	    ./$(BAR64MO)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (arm64-mach-o): build failed, skipping ==="; \
	  fi; \
	done

# ---------------------------------------------------------------------------
# amd64-mach-o (byteorder little, wordsize/pointersize 64, the
# -amd64-mach-o backend) - the OLD amd64 behaviour
# ---------------------------------------------------------------------------
# claude: same Apple-specific link exceptions as arm64-mach-o above
# (no -static, no $(QCPCMAP)), but cross-assembled via "clang -arch
# x86_64" and run under Rosetta 2 rather than natively - only ever
# buildable/runnable on Darwin.
$(BAMD64MO)/%.c--: %.tig | $(BAMD64MO)
	$(TIGERC) -64 $< > $@

$(BAMD64MO)/%.o: $(BAMD64MO)/%.c-- | $(BAMD64MO)
	QC_AS='$(CC_AMD64_MACHO)' QC_LD='$(CC_AMD64_MACHO)' $(QC) -amd64-mach-o -globals -stop .o -o $@ $<

$(BAMD64MO)/%: $(BAMD64MO)/%.o
	$(CC_AMD64_MACHO) $(TOP)/runtime/build-amd64-mach-o/runtime.o $< \
	  $(TOP)/runtime/build-amd64-mach-o/stdlib.a $(TOP)/runtime/build-amd64-mach-o/qcmm.a -o $@

amd64-mach-o: rt-amd64-mach-o
	@$(MAKE) -k $(foreach d,$(DEMOS),$(BAMD64MO)/$(d)) || true

run-amd64-mach-o: amd64-mach-o
	@for d in $(DEMOS); do \
	  if [ -x $(BAMD64MO)/$$d ]; then \
	    echo "=== $$d (amd64-mach-o) ==="; \
	    ./$(BAMD64MO)/$$d; echo "[exit status $$?]"; \
	  else \
	    echo "=== $$d (amd64-mach-o): build failed, skipping ==="; \
	  fi; \
	done

# claude: amd64 (Linux/ELF) is added to the default all/run aggregates,
# same status as every other Linux-hosted backend above - the two -mach-o
# variants are NOT (Darwin-only, would just fail/skip on every other host);
# invoke them explicitly ("make arm64-mach-o", "make run-amd64-mach-o").
run: run-x86 run-ppc run-sparc run-alpha run-mips run-arm run-riscv32 run-riscv64 run-arm64 run-amd64

clean:
	rm -rf $(BX) $(BP) $(BS) $(BA) $(BM) $(BAR) $(BR32) $(BR64) $(BAR64) $(BAMD64) $(BAR64MO) $(BAMD64MO)
