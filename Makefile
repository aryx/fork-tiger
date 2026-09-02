##############################################################################
# Top rules using dune
##############################################################################

-include Makefile.config
# claude: what plain "make"/"make test" below build when no BACKEND is
# given - ./configure picks this (x86 normally; arm64-mach-o on a Mac with
# no i686 cross toolchain - see configure's own "Pick the default backend"
# comment) and writes it into Makefile.config. The "?=" here is only a
# fallback for a stale Makefile.config from before this existed (or no
# configure run at all yet) - re-run ./configure to get the real answer
# for this host.
DEFAULT_BACKEND ?= x86

all::
	dune build
# claude: also build the C/C-- runtime support for DEFAULT_BACKEND (see
# above) so a plain "make" leaves everything needed to link a tiger
# program, not just tigerc itself. Every other backend's own stdlib/
# runtime is unaffected - "make -C stdlib BACKEND=<x>" etc. still work
# exactly as before, this only changes what the bare "all"/"test" here
# reach for.
all::
	$(MAKE) -C stdlib BACKEND=$(DEFAULT_BACKEND)
	$(MAKE) -C runtime BACKEND=$(DEFAULT_BACKEND)
clean::
	dune clean
	$(MAKE) -C stdlib BACKEND=$(DEFAULT_BACKEND) clean
	$(MAKE) -C runtime BACKEND=$(DEFAULT_BACKEND) clean
	$(MAKE) -C demos clean
# claude: tests/run-tests.sh leaves its own tests/build-<arch>/ dirs behind
# (see its "B=build-$BACKEND" - one per backend, same idea as demos/'s own
# build-<arch>/ that "$(MAKE) -C demos clean" above already handles), but
# there's no tests/Makefile to delegate to, so remove them directly here.
clean::
	rm -rf tests/build tests/build-ppc tests/build-sparc tests/build-alpha tests/build-mips tests/build-arm tests/build-riscv32 tests/build-riscv64 tests/build-arm64 tests/build-amd64 tests/build-arm64-mach-o tests/build-amd64-mach-o
install::
	dune install
uninstall::
	dune uninstall
# The behavioural test suite: compile each Tiger program in tests/tiger.tests
# with tigerc and qc, link it, run it, and check its stdout and exit code.
#
# It needs the libraries, so build them first. It compares against a recorded
# baseline rather than demanding that everything pass - see tests/run-tests.sh
# and the failures noted in tests/expected/tiger-$(DEFAULT_BACKEND).txt (or
# tests/expected/tiger.txt for the plain x86 case).
#
# Requires ./configure to have been run, and an installed qc--.
test::
	$(MAKE) -C stdlib BACKEND=$(DEFAULT_BACKEND)
	$(MAKE) -C runtime BACKEND=$(DEFAULT_BACKEND)
	BACKEND=$(DEFAULT_BACKEND) tests/run-tests.sh

# Same suite, against qc--'s other backends instead of its default x86
# one (see "qc --help" for the full list). Kept as their own targets
# rather than folded into "test" - each needs its own cross toolchain (and
# usually a qemu-user emulator, see ./configure's output), and none is
# expected to be all-green: see tests/expected/tiger-<arch>.txt and
# tests/run-tests.sh's header for the known qc-- backend gaps each has
# already turned up (ppc alone is currently all-green).
test-ppc::
	$(MAKE) -C stdlib BACKEND=ppc
	$(MAKE) -C runtime BACKEND=ppc
	BACKEND=ppc tests/run-tests.sh

test-sparc::
	$(MAKE) -C stdlib BACKEND=sparc
	$(MAKE) -C runtime BACKEND=sparc
	BACKEND=sparc tests/run-tests.sh

test-alpha::
	$(MAKE) -C stdlib BACKEND=alpha
	$(MAKE) -C runtime BACKEND=alpha
	BACKEND=alpha tests/run-tests.sh

test-mips::
	$(MAKE) -C stdlib BACKEND=mips
	$(MAKE) -C runtime BACKEND=mips
	BACKEND=mips tests/run-tests.sh

test-arm::
	$(MAKE) -C stdlib BACKEND=arm
	$(MAKE) -C runtime BACKEND=arm
	BACKEND=arm tests/run-tests.sh

test-riscv32::
	$(MAKE) -C stdlib BACKEND=riscv32
	$(MAKE) -C runtime BACKEND=riscv32
	BACKEND=riscv32 tests/run-tests.sh

test-riscv64::
	$(MAKE) -C stdlib BACKEND=riscv64
	$(MAKE) -C runtime BACKEND=riscv64
	BACKEND=riscv64 tests/run-tests.sh

# claude: Linux/ELF (qc--'s -arm64 bare flag) - needs no cross toolchain at
# all on an aarch64-linux host (this dev box included), this being that
# host's own native architecture, same as x86 on an x86 host (see
# ./configure's own arm64 comment). The OLD macOS-only Mach-O behaviour is
# test-arm64-mach-o below instead.
test-arm64::
	$(MAKE) -C stdlib BACKEND=arm64
	$(MAKE) -C runtime BACKEND=arm64
	BACKEND=arm64 tests/run-tests.sh

# claude: x86-64, Linux/ELF (qc--'s -amd64 bare flag) - same shape as
# arm64 above.
test-amd64::
	$(MAKE) -C stdlib BACKEND=amd64
	$(MAKE) -C runtime BACKEND=amd64
	BACKEND=amd64 tests/run-tests.sh

# claude: the OLD arm64/amd64 behaviour (macOS/Mach-O only - see
# configure's own arm64-mach-o/amd64-mach-o comment) - only buildable on
# Darwin, kept out of test-all below for the same reason every other
# optional cross toolchain isn't required to be present.
test-arm64-mach-o::
	$(MAKE) -C stdlib BACKEND=arm64-mach-o
	$(MAKE) -C runtime BACKEND=arm64-mach-o
	BACKEND=arm64-mach-o tests/run-tests.sh

test-amd64-mach-o::
	$(MAKE) -C stdlib BACKEND=amd64-mach-o
	$(MAKE) -C runtime BACKEND=amd64-mach-o
	BACKEND=amd64-mach-o tests/run-tests.sh

# Runs every backend's test tier in one go - useful before a commit that
# touches shared code (frontend/, backend/codegen.ml, runtime/, stdlib/)
# to see the full cross-architecture blast radius at once. Each target
# above already reports its own pass/fail count and diffs against its own
# baseline; this adds nothing beyond running them all back to back.
# claude: test-arm64-mach-o/test-amd64-mach-o are Darwin-only (see their
# own comment above) - deliberately excluded here, same reasoning as every
# other backend whose cross toolchain isn't assumed to be installed.
test-all:: test test-ppc test-sparc test-alpha test-mips test-arm test-riscv32 test-riscv64 test-arm64 test-amd64

build-docker:
	docker build -t "tigerc" .

##############################################################################
# Pad's rules
##############################################################################
