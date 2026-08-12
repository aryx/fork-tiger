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
# expected/tiger-ppc.txt for BACKEND=ppc) rather than "everything must
# pass", so that a suite with known failures still reports *changes*, which
# is what regression testing is for. Individual failures stay visible in
# the output.
#
#   ./run-tests.sh              run them all (x86), check against the baseline
#   ./run-tests.sh --update     re-record the baseline (review the diff!)
#   ./run-tests.sh hello wf     run only those, report but do not compare
#   BACKEND=ppc ./run-tests.sh  same, but for qc--'s -ppc-elf backend
#
# Needs ../Makefile.config, i.e. ./configure must have been run, and the
# libraries built for the chosen backend:
#   make -C ../stdlib [BACKEND=ppc] && make -C ../runtime [BACKEND=ppc]
#
# claude: tigerc itself is backend-agnostic - it always emits
# "target byteorder little" (see backend/codegen.ml) - so for BACKEND=ppc
# this flips that to big in its output before handing it to qc, the same
# fix ../runtime/Makefile and ../stdlib/Makefile already apply to the
# runtime/stdlib .c-- sources. Expected stdout/stderr (x86/<name>.1,
# x86/<name>.2) are reused as-is for ppc: Tiger's observable behaviour
# (what a program prints and returns) is not meant to depend on the
# target, so there is nothing ppc-specific to record separately - if a
# real target-dependent divergence ever turns up, follow qc--'s
# cmm/output-ppc/ precedent (an override directory consulted first) rather
# than duplicating the lot.

set -e

here=$(dirname "$0")
cd "$here"
TOP=..

BACKEND=${BACKEND:-x86}
case "$BACKEND" in
  x86) ;;
  ppc) ;;
  *) echo "run-tests.sh: unknown BACKEND=$BACKEND (expected x86 or ppc)" >&2; exit 2 ;;
esac

if [ ! -f "$TOP/Makefile.config" ]; then
  echo "run-tests.sh: no ../Makefile.config; run ./configure first" >&2
  exit 2
fi

# Read the generated config without involving make.
QC=$(sed -n 's/^QC=//p' "$TOP/Makefile.config")
QCPCMAP=$(sed -n 's/^QCPCMAP=//p' "$TOP/Makefile.config")
if [ "$BACKEND" = ppc ]; then
  CC=$(sed -n 's/^CC_PPC=//p' "$TOP/Makefile.config")
  RUN=$(sed -n 's/^RUN_PPC=//p' "$TOP/Makefile.config")
  QCFLAG=-ppc-elf
  RTDIR=$TOP/runtime/build-ppc
  B=build-ppc
  baseline=expected/tiger-ppc.txt
else
  CC=$(sed -n 's/^CC32=//p' "$TOP/Makefile.config")
  RUN=$(sed -n 's/^RUN32=//p' "$TOP/Makefile.config")
  QCFLAG=
  RTDIR=$TOP/runtime
  B=build
  baseline=expected/tiger.txt
fi
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

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*

mkdir -p "$B" expected
: > "$B/actual.txt"

grep -v '^#' tiger.tests | grep -v '^[ 	]*$' > "$B/manifest.txt"

while read -r name src rc stdin_file; do
  if [ -n "$want" ]; then
    case " $want " in *" $name "*) ;; *) continue ;; esac
  fi

  # .tig -> .c--, the front end under test
  if ! "$TIGERC" "$src" > "$B/$name.c--" 2>"$B/$name.tigerr"; then
    echo "FAIL $name (tigerc)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  # claude: tigerc always emits "target byteorder little"; qc refuses that
  # mismatch for -ppc-elf, so flip it in place before compiling - same fix
  # as ../runtime/Makefile and ../stdlib/Makefile apply to their own
  # sources (see this script's header).
  if [ "$BACKEND" = ppc ]; then
    sed -i 's/byteorder[ ][ ]*little/byteorder big/' "$B/$name.c--"
  fi

  # .c-- -> .o. -globals goes here and nowhere else: the global-variable area
  # is one object per program, and runtime.o and the libraries were built
  # without it.
  if ! "$QC" $QCFLAG -globals -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (qc)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  # runtime.o first: it supplies main, and by qc--(1)'s convention the unit
  # holding main comes first so the C-- globals work out.
  if ! $CC -static "$RTDIR/runtime.o" "$B/$name.o" \
       "$RTDIR/stdlib.a" "$RTDIR/qcmm.a" "$QCPCMAP" \
       -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
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
