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
# Results are checked against a recorded baseline (expected/tiger.txt) rather
# than "everything must pass", so that a suite with known failures still
# reports *changes*, which is what regression testing is for. Individual
# failures stay visible in the output.
#
#   ./run-tests.sh              run them all, check against the baseline
#   ./run-tests.sh --update     re-record the baseline (review the diff!)
#   ./run-tests.sh hello wf     run only those, report but do not compare
#
# Needs ../Makefile.config, i.e. ./configure must have been run, and the
# libraries built:  make -C ../stdlib && make -C ../runtime

set -e

here=$(dirname "$0")
cd "$here"
TOP=..

if [ ! -f "$TOP/Makefile.config" ]; then
  echo "run-tests.sh: no ../Makefile.config; run ./configure first" >&2
  exit 2
fi

# Read the generated config without involving make.
QC=$(sed -n 's/^QC=//p' "$TOP/Makefile.config")
QCPCMAP=$(sed -n 's/^QCPCMAP=//p' "$TOP/Makefile.config")
CC32=$(sed -n 's/^CC32=//p' "$TOP/Makefile.config")
RUN32=$(sed -n 's/^RUN32=//p' "$TOP/Makefile.config")
TIGERC=${TIGERC:-$TOP/bin/tigerc}

B=build

for f in "$TIGERC" "$QC"; do
  if [ ! -x "$f" ]; then
    echo "run-tests.sh: missing $f" >&2
    echo "  build tigerc with 'dune build' and install qc-- with 'make install'" >&2
    exit 2
  fi
done
for f in "$TOP/runtime/runtime.o" "$TOP/runtime/stdlib.a" "$TOP/runtime/qcmm.a"; do
  if [ ! -f "$f" ]; then
    echo "run-tests.sh: missing $f" >&2
    echo "  make -C $TOP/stdlib && make -C $TOP/runtime" >&2
    exit 2
  fi
done

update=no
if [ "$1" = "--update" ]; then update=yes; shift; fi
want=$*
baseline=expected/tiger.txt

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

  # .c-- -> .o. -globals goes here and nowhere else: the global-variable area
  # is one object per program, and runtime.o and the libraries were built
  # without it.
  if ! "$QC" -globals -stop .o -o "$B/$name.o" "$B/$name.c--" \
       >"$B/$name.qcerr" 2>&1; then
    echo "FAIL $name (qc)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  # runtime.o first: it supplies main, and by qc--(1)'s convention the unit
  # holding main comes first so the C-- globals work out.
  if ! $CC32 -static "$TOP/runtime/runtime.o" "$B/$name.o" \
       "$TOP/runtime/stdlib.a" "$TOP/runtime/qcmm.a" "$QCPCMAP" \
       -o "$B/$name" 2>"$B/$name.lderr"; then
    echo "FAIL $name (link)"; echo "$name FAIL" >> "$B/actual.txt"; continue
  fi

  if [ "$stdin_file" = "-" ]; then input=/dev/null; else input=$stdin_file; fi
  # "|| got=$?" rather than a bare call: these programs exit non-zero by
  # design - a Tiger program returns the value of its body - and set -e would
  # otherwise abort the whole run on the first one.
  got=0
  timeout 60 $RUN32 "./$B/$name" < "$input" > "$B/$name.out" 2> "$B/$name.err" \
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
echo "tiger: $pass passed, $fail failed"

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
