#!/bin/sh
# Written by Claude Code.
#
# Run the Tiger test suite: compile each program in tiger.tests with tigerc
# and qc, link it, run it, and check its stdout, stderr, and exit code.
#
# This is the tier that says the whole toolchain works, front end and back end
# together, rather than merely that tigerc does not crash. It replaces
# tiger.tst, which was Lua for qc--'s testdrv.lua; qc-- no longer embeds Lua.
#
# Results are checked against a recorded baseline (expected/tiger.txt, or
# expected/tiger-<backend>.txt for BACKEND=<backend>) rather than
# "everything must pass", so that a suite with known failures still reports
# *changes*, which is what regression testing is for. Individual failures
# stay visible in the output.
#
#   ./run-tests.sh                  run them all (x86), check against the baseline
#   ./run-tests.sh --update         re-record the baseline (review the diff!)
#   ./run-tests.sh hello wf         run only those, report but do not compare
#   BACKEND=ppc ./run-tests.sh      same, but for qc--'s -ppc-elf backend
#   BACKEND=<x> ./run-tests.sh      x in ppc, sparc, alpha, mips, arm, riscv32, riscv64, arm64
#
# Needs ../Makefile.config, i.e. ./configure must have been run, and the
# libraries built for the chosen backend:
#   make -C ../stdlib BACKEND=<x> && make -C ../runtime BACKEND=<x>
#
# claude: tigerc itself is backend-agnostic by default - it always emits
# "target byteorder little" with 32-bit metrics (see backend/codegen.ml) -
# so non-x86 backends need their own transform applied to tigerc's output
# before qc will accept it, mirroring exactly what ../runtime/Makefile and
# ../stdlib/Makefile already apply to the runtime/stdlib .c-- sources (see
# those Makefiles' XFORM comments for the fuller reasoning):
#   none - mips, riscv32: already 32-bit little-endian ieee754, no change
#   big  - ppc, sparc: byteorder little -> big
#   arm  - splice float "none" (arm has no FPU)
#   64   - alpha, riscv64, arm64: also pass tigerc "-64" so it emits bits64
#          C-- (wordsize/pointersize 64) in the first place - see
#          docs/claude_notes/notes_64bits.txt
#
# arm64 is also the one exception (besides riscv32) to "link with $CC
# -static": Apple does not support static-linking libSystem at all, and
# there is no $QCPCMAP equivalent to pass either (Mach-O's ld64 has no
# linker-script mechanism) - see the link step below and qc--'s own
# docs/claude_notes/notes_arm64.txt.
#
# Expected stdout/stderr (x86/<name>.1, x86/<name>.2) are reused as-is for
# every backend: Tiger's observable behaviour (what a program prints and
# returns) is not meant to depend on the target, so there is nothing
# backend-specific to record separately - if a real target-dependent
# divergence ever turns up, follow qc--'s cmm/output-ppc/ precedent (an
# override directory consulted first) rather than duplicating the lot.
#
# riscv32 is the one exception to "link with $CC -static": Ubuntu ships no
# riscv32-linux-gnu glibc at all, so its toolchain is bare-metal
# (gcc-riscv64-unknown-elf) against picolibc, and the final link has to be
# done with plain `ld` rather than gcc+--specs=picolibc.specs - see the
# link step below for the two independent reasons (picolibc.ld's load
# address, and --gc-sections silently dropping .pcmap entries), verified
# against qc--'s own tests/run-tiger-riscv32.sh.

set -e

here=$(dirname "$0")
cd "$here"
TOP=..

BACKEND=${BACKEND:-x86}
case "$BACKEND" in
  x86|ppc|sparc|alpha|mips|arm|riscv32|riscv64|arm64) ;;
  *) echo "run-tests.sh: unknown BACKEND=$BACKEND (expected x86, ppc, sparc, alpha, mips, arm, riscv32, riscv64 or arm64)" >&2; exit 2 ;;
esac

if [ ! -f "$TOP/Makefile.config" ]; then
  echo "run-tests.sh: no ../Makefile.config; run ./configure first" >&2
  exit 2
fi

# Read the generated config without involving make.
QC=$(sed -n 's/^QC=//p' "$TOP/Makefile.config")
QCINCLUDE=$(sed -n 's/^QCINCLUDE=//p' "$TOP/Makefile.config")
QCPCMAP=$(sed -n 's/^QCPCMAP=//p' "$TOP/Makefile.config")

TIGERFLAG=
XFORM=none
case "$BACKEND" in
  x86)
    CC=$(sed -n 's/^CC32=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN32=//p' "$TOP/Makefile.config")
    QCFLAG=
    ;;
  ppc)
    CC=$(sed -n 's/^CC_PPC=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_PPC=//p' "$TOP/Makefile.config")
    QCFLAG=-ppc-elf
    XFORM=big
    ;;
  sparc)
    CC=$(sed -n 's/^CC_SPARC=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_SPARC=//p' "$TOP/Makefile.config")
    QCFLAG=-sparc
    XFORM=big
    ;;
  alpha)
    CC=$(sed -n 's/^CC_ALPHA=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_ALPHA=//p' "$TOP/Makefile.config")
    QCFLAG=-alpha
    TIGERFLAG=-64
    XFORM=64
    ;;
  mips)
    CC=$(sed -n 's/^CC_MIPS=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_MIPS=//p' "$TOP/Makefile.config")
    QCFLAG=-mips
    ;;
  arm)
    CC=$(sed -n 's/^CC_ARM=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_ARM=//p' "$TOP/Makefile.config")
    QCFLAG=-arm
    XFORM=arm
    ;;
  riscv32)
    CC=$(sed -n 's/^CC_RISCV32=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_RISCV32=//p' "$TOP/Makefile.config")
    QCFLAG=-riscv32
    ;;
  riscv64)
    CC=$(sed -n 's/^CC_RISCV64=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_RISCV64=//p' "$TOP/Makefile.config")
    QCFLAG=-riscv64
    TIGERFLAG=-64
    XFORM=64
    ;;
  arm64)
    # claude: also 64-bit, like alpha/riscv64 above - TIGERFLAG=-64/XFORM=64
    # need nothing arm64-specific (see the XFORM=64 comment further down).
    # Unlike every backend above, this is this host's own native
    # architecture whenever it's an Apple Silicon Mac - CC_ARM64 is plain
    # "clang", no cross toolchain.
    CC=$(sed -n 's/^CC_ARM64=//p' "$TOP/Makefile.config")
    RUN=$(sed -n 's/^RUN_ARM64=//p' "$TOP/Makefile.config")
    QCFLAG=-arm64
    TIGERFLAG=-64
    XFORM=64
    ;;
esac
RTDIR=$TOP/runtime
B=build
baseline=expected/tiger.txt
if [ "$BACKEND" != x86 ]; then
  RTDIR=$TOP/runtime/build-$BACKEND
  B=build-$BACKEND
  baseline=expected/tiger-$BACKEND.txt
fi

# claude: qc drives an external assembler/linker, defaulting to clang (able
# to target i386/ppc from any host - see "qc -help"'s -as/-ld entry, which
# is why x86/ppc need no override here). clang has no working backend for
# the rest of these - same fix ../runtime/Makefile and ../stdlib/Makefile
# apply for the same reason.
case "$BACKEND" in
  sparc|alpha|mips|arm|riscv32|riscv64|arm64)
    QC_AS=$CC
    QC_LD=$CC
    export QC_AS QC_LD
    ;;
esac

TIGERC=${TIGERC:-$TOP/bin/tigerc}

for f in "$TIGERC" "$QC"; do
  if [ ! -x "$f" ]; then
    echo "run-tests.sh: missing $f" >&2
    echo "  build tigerc with 'dune build' and install qc-- with 'make install'" >&2
    exit 2
  fi
done
for f in "$RTDIR/runtime.o" "$RTDIR/stdlib.a" "$RTDIR/qcmm.a"; do
  if [ ! -f "$f" ]; then
    echo "run-tests.sh: missing $f" >&2
    echo "  make -C $TOP/stdlib BACKEND=$BACKEND && make -C $TOP/runtime BACKEND=$BACKEND" >&2
    exit 2
  fi
done

# riscv32's freestanding final link needs a few extra pieces up front - see
# this script's header and the link step below for why plain `ld` rather
# than $CC drives it.
if [ "$BACKEND" = riscv32 ]; then
  LDRISCV32=riscv64-unknown-elf-ld
  RISCV32_MARCH="-march=rv32imac -mabi=ilp32"
  GCCLIBDIR=$(dirname "$(riscv64-unknown-elf-gcc $RISCV32_MARCH -print-libgcc-file-name)")
  MULTIDIR=$(riscv64-unknown-elf-gcc $RISCV32_MARCH -print-multi-directory)
  PICOLIBDIR=/usr/lib/picolibc/riscv64-unknown-elf/lib/$MULTIDIR
  if [ ! -f "$PICOLIBDIR/libc.a" ]; then
    echo "run-tests.sh: no libc.a under $PICOLIBDIR - picolibc-riscv64-unknown-elf layout changed?" >&2
    exit 2
  fi
fi

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*

mkdir -p "$B" expected
: > "$B/actual.txt"

# claude: the freestanding entry point, built once here rather than via
# runtime/Makefile - riscv32_crt0.o must never be archived into a .a (a
# linker only pulls an archive member in for a referenced undefined
# symbol, and nothing ever references "_start" by name - it is found via
# entry-point lookup, a different mechanism), so, like qc--'s own
# tests/run-tiger-riscv32.sh, it is passed as a plain object on the final
# link command line instead.
if [ "$BACKEND" = riscv32 ] && [ ! -f "$B/riscv32_crt0.o" ]; then
  $CC -c "$QCINCLUDE/riscv32_crt0.s" -o "$B/riscv32_crt0.o"
fi

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  # .tig -> .c--, the front end under test
  if ! "$TIGERC" $TIGERFLAG "$src" > "$B/$name.c--" 2>"$B/$name.tigerr"; then
    echo "FAIL $name (tigerc)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  # claude: apply this backend's XFORM to tigerc's output - see this
  # script's header for what each value does. XFORM=64 needs nothing
  # here: TIGERFLAG=-64 already made tigerc itself emit bits64 C-- with
  # "wordsize 64 pointersize 64" and "+8" offsets (Frame.bits_str()/
  # codegen.ml, unlike ../runtime/Makefile's hand-written sources, which
  # have no -64 flag of their own to lean on) - so only "big"/"arm" need a
  # post-hoc rewrite of the pragma line.
  case "$XFORM" in
    big)
      sed -i 's/byteorder[ ][ ]*little/byteorder big/' "$B/$name.c--"
      ;;
    arm)
      sed -i 's/^target byteorder little\(.*\);/target byteorder little float "none"\1;/' "$B/$name.c--"
      ;;
  esac

  # .c-- -> .o. -globals goes here and nowhere else: the global-variable area
  # is one object per program, and runtime.o and the libraries were built
  # without it.
  if ! "$QC" $QCFLAG -globals -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (qc)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  # runtime.o first: it supplies main, and by qc--(1)'s convention the unit
  # holding main comes first so the C-- globals work out.
  if [ "$BACKEND" = riscv32 ]; then
    # claude: plain `ld`, NOT $CC (gcc + --specs=picolibc.specs) - two
    # independent reasons, both found the hard way by qc--'s own
    # tests/run-tiger-riscv32.sh (see its header comment for the full
    # story): (1) picolibc.specs' link spec injects "-Tpicolibc.ld", a
    # bare-metal memory map at a load address plain qemu-riscv32
    # user-mode emulation cannot run; (2) it also adds "--gc-sections"
    # unconditionally, which silently discards individual .pcmap entries
    # even though pcmap.ld's Cmm_pc_map/Cmm_pc_map_limit symbols still
    # bound a correctly-sized region - --gc-sections's liveness analysis
    # doesn't understand a linker-script address-range reference. Plain
    # ld sidesteps both; -lc/-lgcc and their search paths (normally
    # supplied by the specs file) are added explicitly instead.
    if ! $LDRISCV32 -m elf32lriscv -static -e _start \
         -L"$PICOLIBDIR" -L"$GCCLIBDIR" \
         "$B/riscv32_crt0.o" "$RTDIR/runtime.o" "$B/$name.o" \
         "$RTDIR/stdlib.a" "$RTDIR/qcmm.a" "$QCPCMAP" \
         --start-group -lc -lgcc --end-group \
         -o "$B/$name" 2>"$B/$name.lderr"; then
      echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
    fi
  elif [ "$BACKEND" = arm64 ]; then
    # claude: no "-static" (Apple does not support static-linking
    # libSystem at all, unlike every Linux-hosted backend above) and no
    # "$QCPCMAP" (that linker-script fragment is GNU-ld-specific - Mach-O's
    # ld64 has no "-T"/linker-script mechanism at all; qc--'s own -arm64
    # backend doesn't need one, see its docs/claude_notes/notes_arm64.txt).
    if ! $CC "$RTDIR/runtime.o" "$B/$name.o" \
         "$RTDIR/stdlib.a" "$RTDIR/qcmm.a" \
         -o "$B/$name" 2>"$B/$name.lderr"; then
      echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
    fi
  else
    if ! $CC -static "$RTDIR/runtime.o" "$B/$name.o" \
         "$RTDIR/stdlib.a" "$RTDIR/qcmm.a" "$QCPCMAP" \
         -o "$B/$name" 2>"$B/$name.lderr"; then
      echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
    fi
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$stdin_file; fi
  # "|| got=$?" rather than a bare call: these programs exit non-zero by
  # design - a Tiger program returns the value of its body - and set -e would
  # otherwise abort the whole run on the first one.
  got=0
  timeout 60 $RUN "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err" \
    || got=$?

  # x86/<name>.2 is expected stderr; tests without one (there shouldn't be
  # any, but just in case) are expected to print nothing on stderr.
  stderr_expected="x86/$name.2"
  [ -f "$stderr_expected" ] || stderr_expected=/dev/null

  if ! diff "$B/$name.out" "x86/$name.1" > "$B/$name.diff" 2>&1; then
    echo "FAIL $name (stdout differs; see $B/$name.diff)"
    if [ -s "$B/$name.err" ]; then
      echo "     stderr: $(head -1 "$B/$name.err")"
    fi
    echo "$name FAIL" >> "$B/actual.txt"
  elif [ "$got" != "$rc" ]; then
    echo "FAIL $name (exit $got, expected $rc)"
    echo "$name FAIL" >> "$B/actual.txt"
  elif ! diff "$B/$name.err" "$stderr_expected" > "$B/$name.errdiff" 2>&1; then
    echo "FAIL $name (stderr differs; see $B/$name.errdiff)"
    echo "$name FAIL" >> "$B/actual.txt"
  else
    echo "PASS $name"
    echo "$name PASS" >> "$B/actual.txt"
  fi
done < "$B/manifest.txt"

pass=$(grep -c " PASS$" "$B/actual.txt" || true)
fail=$(grep -c " FAIL$" "$B/actual.txt" || true)
echo
echo "tiger-$BACKEND: $pass passed, $fail failed"

# Running a subset says nothing about the whole, so do not compare then.
if [ -n "$want" ]; then exit 0; fi

if [ "$update" = yes ]; then
  cp "$B/actual.txt" "$baseline"
  echo "recorded baseline ($baseline)"
  exit 0
fi

if [ ! -f "$baseline" ]; then
  echo "no baseline at $baseline; run with --update" >&2
  exit 2
fi

if diff "$baseline" "$B/actual.txt" > "$B/baseline.diff" 2>&1; then
  echo "matches the baseline"
  exit 0
fi

echo
echo "CHANGED against the baseline ('<' expected, '>' got):"
grep '^[<>]' "$B/baseline.diff"
echo
echo "If intended, re-record with: $0 --update"
exit 1
