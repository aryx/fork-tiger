##############################################################################
# Top rules using dune
##############################################################################

all::
	dune build
# claude: also build the C/C-- runtime support (x86 by default, matching
# "test"'s default) so a plain "make" leaves everything needed to link a
# tiger program, not just tigerc itself
all::
	$(MAKE) -C stdlib
	$(MAKE) -C runtime
clean::
	dune clean
install::
	dune install
uninstall::
	dune uninstall
# The behavioural test suite: compile each Tiger program in tests/tiger.tests
# with tigerc and qc, link it, run it, and check its stdout and exit code.
#
# It needs the libraries, so build them first. It compares against a recorded
# baseline rather than demanding that everything pass - see tests/run-tests.sh
# and the failures noted in tests/expected/tiger.txt.
#
# Requires ./configure to have been run, and an installed qc--.
test::
	$(MAKE) -C stdlib
	$(MAKE) -C runtime
	tests/run-tests.sh

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

# Runs every backend's test tier in one go - useful before a commit that
# touches shared code (frontend/, backend/codegen.ml, runtime/, stdlib/)
# to see the full cross-architecture blast radius at once. Each target
# above already reports its own pass/fail count and diffs against its own
# baseline; this adds nothing beyond running them all back to back.
test-all:: test test-ppc test-sparc test-alpha test-mips test-arm test-riscv32 test-riscv64

build-docker:
	docker build -t "tigerc" .

##############################################################################
# Pad's rules
##############################################################################
