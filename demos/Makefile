# claude: build and run the Tiger demo programs in this directory
# (hello.tig, forloop.tig, queens.tig) end to end, for every backend qc--
# supports (see "qc --help", or ../Makefile.backends' ARCHS). Modeled on
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
# claude: x86 (below) is hand-written, since it's the one backend with no
# per-backend transform/override of any kind and no query_backend-derived
# names to look up. Every other backend is synthesized by
# DEMO_ARCH_template, one $(eval $(call ...)) per ../Makefile.backends'
# ARCHS entry, rather than the 12 copy-pasted per-backend rule blocks (one
# per backend, ~40 lines each) this file used to carry - see
# Makefile.backends' own header for why only the backend list and each
# backend's CC_<ARCH> variable name are still hand-maintained at all.
# riscv32's final link step is still special-cased by hand after the
# loop (freestanding, via plain `ld` - see its own section below), same
# as before.
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
-include $(TOP)/Makefile.backends

TIGERC ?= $(TOP)/bin/tigerc
DEMOS   = hello forloop queens

BX = build

.PHONY: all x86 run run-x86 rt clean

# claude: without this, make treats the .c--/.o files below as disposable
# intermediates of the pattern-rule chain (.tig -> .c-- -> .o -> binary)
# and silently deletes them after each build - annoying since the .c-- is
# useful to eyeball and deleting the .o forces a full qc rerun even when
# only the link step needs redoing.
.SECONDARY:

# claude: DARWIN_ARCHS (arm64-mach-o, amd64-mach-o) excluded from both
# aggregates below - only ever buildable/runnable on Darwin, same
# reasoning as every other backend whose cross toolchain isn't assumed to
# be installed, just unconditional here instead of best-effort.
all: x86 $(filter-out $(DARWIN_ARCHS),$(ARCHS))
run: run-x86 $(addprefix run-,$(filter-out $(DARWIN_ARCHS),$(ARCHS)))

$(BX):
	mkdir -p $@

# The runtime/stdlib system these demos link against (GC, alloc, print,
# ...) - see ../stdlib/Makefile and ../runtime/Makefile. Not file-based
# prerequisites of the demo binaries below: those Makefiles already track
# their own staleness, so it is cheap to always ask and let them no-op.
rt:
	$(MAKE) -C $(TOP)/stdlib
	$(MAKE) -C $(TOP)/runtime

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
# Every other backend (ppc, sparc, alpha, mips, arm, riscv32, riscv64,
# arm64, amd64, arm64-mach-o, amd64-mach-o) - synthesized from
# ../Makefile.backends' ARCHS, one $(eval $(call ...)) per entry. See this
# file's own header and ../stdlib/Makefile's header for the fuller
# per-backend reasoning ($(1)'s XFORM class, why sparc/alpha/etc need
# QC_AS/QC_LD pointed at the real cross toolchain instead of qc--'s own
# clang default, why the two mach-o backends drop -static/$(QCPCMAP) from
# their link line).
#
# The four resolved-once-per-backend variables below (CC_$(1), XFORM_$(1),
# RUN_$(1), TIGERFLAG_$(1)) are plain immediate (":=") assignments, not
# variable *names* to look up again - Makefile.backends' backend.$(1) is
# itself only a variable name (Makefile.config's "CC_PPC" etc), so this
# resolves it exactly once here rather than re-deriving it at every use
# site below.
define DEMO_ARCH_template

CC_$(1)        := $$($$(backend.$(1)))
XFORM_$(1)     := $$($$(subst CC,XFORM,$$(backend.$(1))))
RUN_$(1)       := $$($$(subst CC,RUN,$$(backend.$(1))))
TIGERFLAG_$(1) := $$(if $$(filter 64,$$(XFORM_$(1))),-64,)

.PHONY: $(1) run-$(1) rt-$(1)

build-$(1):
	mkdir -p $$@

rt-$(1):
	$$(MAKE) -C $$(TOP)/stdlib BACKEND=$(1)
	$$(MAKE) -C $$(TOP)/runtime BACKEND=$(1)

# claude: tigerc always emits "target byteorder little" with 32-bit
# metrics (see backend/codegen.ml); qc refuses that mismatch for a
# big-endian or floatless target ("metrics of source code don't match
# the target"), so XFORM=big/arm splice the pragma line tigerc's own
# output needs, the same fix ../runtime/Makefile and ../stdlib/Makefile
# apply to their own hand-written C-- sources. XFORM=64 needs no splice
# here - passing tigerc "-64" makes it emit bits64 C-- (wordsize/
# pointersize 64) directly, unlike those hand-written sources.
ifeq ($$(XFORM_$(1)),big)
build-$(1)/%.c--: %.tig | build-$(1)
	$$(TIGERC) $$< | sed 's/byteorder[ ][ ]*little/byteorder big/' > $$@
else ifeq ($$(XFORM_$(1)),arm)
build-$(1)/%.c--: %.tig | build-$(1)
	$$(TIGERC) $$< | sed 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' > $$@
else
build-$(1)/%.c--: %.tig | build-$(1)
	$$(TIGERC) $$(TIGERFLAG_$(1)) $$< > $$@
endif

# claude: qc drives an external assembler/linker, defaulting to clang
# (able to target i386/ppc from any host - see "qc -help"'s -as/-ld
# entry, which is why x86/ppc need no override). clang has no working
# backend for the rest of these (confirmed by hand for sparc: it accepts
# "-target sparc-unknown-linux-gnu" but then rejects qc--'s own emitted
# assembly) - same fix ../runtime/Makefile and ../stdlib/Makefile apply
# for the same reason.
ifeq ($(1),ppc)
build-$(1)/%.o: build-$(1)/%.c-- | build-$(1)
	$$(QC) -$(1) -globals -stop .o -o $$@ $$<
else
build-$(1)/%.o: build-$(1)/%.c-- | build-$(1)
	QC_AS='$$(CC_$(1))' QC_LD='$$(CC_$(1))' $$(QC) -$(1) -globals -stop .o -o $$@ $$<
endif

# runtime.o first: it supplies main, and by qc--(1)'s convention the unit
# holding main comes first so the C-- globals work out. riscv32's own
# final link is freestanding (plain `ld`, no glibc) and defined by hand
# after this template instead - see its own section below for why.
# arm64-mach-o/amd64-mach-o drop -static (Apple does not support
# static-linking libSystem at all) and $(QCPCMAP) (that linker-script
# fragment is GNU-ld-specific; Mach-O's ld64 has no -T mechanism at all
# and needs no equivalent - see qc--'s own notes_arm64.txt/notes_amd64.txt).
ifeq ($(1),riscv32)
else ifneq ($$(filter $(1),$$(DARWIN_ARCHS)),)
build-$(1)/%: build-$(1)/%.o
	$$(CC_$(1)) $$(TOP)/runtime/build-$(1)/runtime.o $$< \
	  $$(TOP)/runtime/build-$(1)/stdlib.a $$(TOP)/runtime/build-$(1)/qcmm.a -o $$@
else
build-$(1)/%: build-$(1)/%.o
	$$(CC_$(1)) -static $$(TOP)/runtime/build-$(1)/runtime.o $$< \
	  $$(TOP)/runtime/build-$(1)/stdlib.a $$(TOP)/runtime/build-$(1)/qcmm.a $$(QCPCMAP) -o $$@
endif

# claude: "$(MAKE) -k ... || true", unlike x86's own direct prerequisite
# list above - not every backend is all-green (see tests/expected/
# tiger-<arch>.txt) - "-k" keeps building the remaining demos past the
# first failure instead of the whole "make <arch>" bailing out, and the
# trailing "|| true" stops that sub-make's own failure from failing this
# target.
$(1): rt-$(1)
	@$$(MAKE) -k $$(foreach d,$$(DEMOS),build-$(1)/$$(d)) || true

run-$(1): $(1)
	@for d in $$(DEMOS); do \
	  if [ -x build-$(1)/$$$$d ]; then \
	    echo "=== $$$$d ($(1)) ==="; \
	    $$(RUN_$(1)) ./build-$(1)/$$$$d; echo "[exit status $$$$?]"; \
	  else \
	    echo "=== $$$$d ($(1)): build failed, skipping ==="; \
	  fi; \
	done
endef

$(foreach a,$(ARCHS),$(eval $(call DEMO_ARCH_template,$(a))))

# ---------------------------------------------------------------------------
# riscv32's final link (freestanding, byte-identical shape to
# tests/run-tests.sh's own riscv32 branch - see there for the fuller
# story): plain `ld`, NOT $(CC_riscv32) (gcc + --specs=picolibc.specs) -
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
#
# riscv32_crt0.o must never be archived into a .a (a linker only pulls an
# archive member in for a referenced undefined symbol, and nothing ever
# references "_start" by name - it is found via entry-point lookup, a
# different mechanism), so it is built once here and passed as a plain
# object on the final link line instead, exactly like qc--'s own
# tests/run-tiger-riscv32.sh.
RISCV32_MARCH = -march=rv32imac -mabi=ilp32
RISCV32_GCCLIBDIR = $(shell dirname "$$(riscv64-unknown-elf-gcc $(RISCV32_MARCH) -print-libgcc-file-name)")
RISCV32_MULTIDIR  = $(shell riscv64-unknown-elf-gcc $(RISCV32_MARCH) -print-multi-directory)
RISCV32_PICOLIBDIR = /usr/lib/picolibc/riscv64-unknown-elf/lib/$(RISCV32_MULTIDIR)

build-riscv32/riscv32_crt0.o: $(QCINCLUDE)/riscv32_crt0.s | build-riscv32
	$(CC_RISCV32) -c $< -o $@

build-riscv32/%: build-riscv32/%.o build-riscv32/riscv32_crt0.o
	riscv64-unknown-elf-ld -m elf32lriscv -static -e _start \
	  -L$(RISCV32_PICOLIBDIR) -L$(RISCV32_GCCLIBDIR) \
	  build-riscv32/riscv32_crt0.o $(TOP)/runtime/build-riscv32/runtime.o $< \
	  $(TOP)/runtime/build-riscv32/stdlib.a $(TOP)/runtime/build-riscv32/qcmm.a $(QCPCMAP) \
	  --start-group -lc -lgcc --end-group \
	  -o $@

clean:
	rm -rf $(BX) $(addprefix build-,$(ARCHS))
