# claude: build and run the Tiger demo programs in this directory
# (hello.tig, forloop.tig, queens.tig) end to end, for both qc--'s
# default x86 backend and its -ppc-elf one. Modeled on ~/c--/demos/Makefile,
# adapted for the extra front-end stage and the runtime/GC system a Tiger
# program needs that a bare C-- one (like that Makefile's hello.c--) does
# not.
#
# This is the "poke at one demo by hand" version of the pipeline
# tests/run-tests.sh already automates over the whole test suite; see that
# script if what you want is the full behavioural suite with a recorded
# baseline instead of a couple of demos.
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
#    (QC, CC32, CC_PPC, RUN32, RUN_PPC, ...).
#
# 3. A cross toolchain per target you want - NOT gcc-multilib/g++-multilib,
#    which add secondary ABIs of the *host's own* architecture family (so
#    on aarch64 they offer 32-bit ARM, never i386 or powerpc):
#
#      sudo apt install gcc-i686-linux-gnu libc6-dev-i386-cross       # x86
#      sudo apt install gcc-powerpc-linux-gnu libc6-dev-powerpc-cross # ppc
#
# 4. To *run* the results on a host that isn't natively x86/ppc, qemu's
#    user-mode emulators:
#
#      sudo apt install qemu-user
#
#    RUN32/RUN_PPC (from ../Makefile.config) name qemu-i386/qemu-ppc
#    explicitly rather than relying on binfmt_misc, which is a host-wide
#    registration a container inherits only by accident - the kernel
#    resolves the interpreter path in the container's own mount namespace,
#    so the same image runs on one machine and not another. On a genuine
#    x86 (or ppc) host, ../configure already leaves RUN32 (or RUN_PPC)
#    empty, so the binary just runs directly.
#
#    We link -static, so no sysroot/-L argument is required to run either
#    binary.
#
# ---------------------------------------------------------------------------

TOP = ..

ifeq (,$(wildcard $(TOP)/Makefile.config))
$(error demos/Makefile: no $(TOP)/Makefile.config; run $(TOP)/configure first)
endif
-include $(TOP)/Makefile.config

TIGERC ?= $(TOP)/bin/tigerc
DEMOS   = hello forloop queens

BX = build
BP = build-ppc

.PHONY: all x86 ppc run run-x86 run-ppc rt rt-ppc clean

# claude: without this, make treats the .c--/.o files below as disposable
# intermediates of the pattern-rule chain (.tig -> .c-- -> .o -> binary)
# and silently deletes them after each build - annoying since the .c-- is
# useful to eyeball and deleting the .o forces a full qc rerun even when
# only the link step needs redoing.
.SECONDARY:

all: x86 ppc

$(BX) $(BP):
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

# claude: unlike x86, ppc is not expected to be all-green yet - see
# tests/expected/tiger-ppc.txt and CLAUDE.md's PPC section, currently
# queens fails to even compile ("0-key type scheme", a qc-- backend gap).
# A plain prerequisite list would make the whole "make ppc" bail out at
# the first such failure and never build the demos after it; "$(MAKE) -k"
# instead keeps going, and the trailing "|| true" stops that sub-make's
# own failure from failing this target in turn.
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

run: run-x86 run-ppc

clean:
	rm -rf $(BX) $(BP)
