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

# Same suite, against qc--'s -ppc-elf backend instead of its default x86
# one. Kept as its own target rather than folded into "test" - it needs a
# ppc cross toolchain and qemu-ppc (see ./configure's output), and it is
# not expected to be all-green yet: see tests/expected/tiger-ppc.txt and
# its header for the known qc-- backend gaps this has already turned up.
test-ppc::
	$(MAKE) -C stdlib BACKEND=ppc
	$(MAKE) -C runtime BACKEND=ppc
	BACKEND=ppc tests/run-tests.sh

build-docker:
	docker build -t "tigerc" .

##############################################################################
# Pad's rules
##############################################################################
