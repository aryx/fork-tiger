# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A Tiger language compiler (Tiger is the language from Appel's "Modern Compiler
Implementation in ML") written in OCaml. It is a **front end only**: it
lexes/parses/typechecks/translates Tiger source down to an internal IR, then
emits textual **C--** for [Quick C--](http://www.cminusminus.org) (`qc--`),
which does the real x86 code generation. This repo does not itself contain a
register allocator or assembler.

## Build

Requires OCaml, GCC, an i686 cross toolchain, and a built/installed `qc--`.

```sh
./configure          # locates qc--, writes Makefile.config (generated, gitignored)
dune build            # or: make
```

`./configure` looks for `qc` on PATH or in the current opam switch, and
derives `QCLIB` (its runtime dir with `qc--runtime.h`/`pcmap.ld`) from it. It
also picks the 32-bit x86 cross toolchain (`i686-linux-gnu-gcc`/`-ar`, default
`CC32`/`AR32`) since qc-- only ever emits i386, and `qemu-i386` for `RUN32`
when the host isn't x86. Options: `--qc=PATH`, `--cc32=CMD`, or set
`QC`/`CC32`/`RUN32`/`AR32` in the environment. `Makefile.config` is generated
by this script — never edit it, re-run `./configure` instead.
`configure.old` is the original Perl script, kept for reference but unused.

The compiler binary ends up at `bin/tigerc` (symlink to
`_build/install/default/bin`).

```sh
./tigerc demos/hello.tig > demos/hello.c--
qc -globals -o hello runtime/runtime.o stdlib/stdlib.a demos/hello.c--
```

## Tests

```sh
make test              # builds stdlib + runtime, then runs tests/run-tests.sh
tests/run-tests.sh                # all tests, diffed against the recorded baseline
tests/run-tests.sh hello wf       # just these tests, report only (no baseline diff)
tests/run-tests.sh --update       # re-record the baseline (review the diff before committing!)
```

This is an **end-to-end behavioural** suite, not OCaml unit tests: for each
`.tig` program listed in `tests/tiger.tests` it runs `tigerc` → `qc` → link
(`runtime.o` + `stdlib.a` + `qcmm.a`) → execute (under `qemu-i386` on non-x86
hosts) → compares stdout/exit code. It requires `./configure` to have
succeeded and `make -C stdlib && make -C runtime` to have run first (`make
test` does this for you). Results are checked against
`tests/expected/tiger.txt` rather than requiring 100% pass, because some
tests are known-failing; the point is to catch *changes* in pass/fail status.
Assembly-output comparisons (`tests/x86/*.s.gz`) from the original suite were
dropped since qc--'s register allocator differs from the one that produced
them.

### PPC (qc--'s `-ppc-elf` backend)

```sh
make test-ppc                     # builds stdlib+runtime for BACKEND=ppc, then runs the suite
BACKEND=ppc tests/run-tests.sh    # same suite, driven directly
```

Same manifest, same `tests/x86/*.1`/`*.2` expected output (Tiger's observable
behaviour isn't meant to depend on the target), but linked against qc--'s
`-ppc-elf` backend and run under `qemu-ppc`. `stdlib/Makefile` and
`runtime/Makefile` take a `BACKEND=ppc` argument (default `x86`) and build
into `build-ppc/` instead of clobbering the x86 objects; `./configure`
detects the ppc cross toolchain (`CC_PPC`, `AR_PPC`, `RUN_PPC` in
`Makefile.config`, best-effort — a missing one only warns). tigerc itself
always emits `target byteorder little`; both the Makefiles and
`run-tests.sh` flip that to `big` in their own output on the fly, they don't
touch the checked-in `.c--` sources.

Currently 6/14 pass (`tests/expected/tiger-ppc.txt`); the rest fail at `qc`
with `Impossible("instantiated 0-key type scheme with 1 widths")`, a real
qc--/target-metrics gap, not a regression in this suite.

There is no OCaml-level unit test framework in this repo (unlike the Testo
convention used elsewhere) — correctness is verified through this behavioural
suite.

## Architecture

Pipeline, roughly one stage per directory:

```
parsing/   lexer.mll, parser.mly  -->  Ast.exp   (parsing/ast.ml)
frontend/  Semantics.translate : Environment.t -> Ast.exp -> (Frame.frame * Tree.exp) list
              - Environment.ml   scopes/types (vartype, VarEntry/FunEntry), one env per nesting level
              - Frame.ml         abstract stack frame / calling-convention (params, locals, temps, static links)
              - Translate.ml     builds Tree IR fragments (records, arrays, calls, control flow) from typed AST
              - Tree.ml          the IR: stm (EXP/MOVE/JUMP/CJUMP/SEQ/...) and exp (CONST/BINOP/MEM/CALL/...)
              - Canonical.ml     linearize: turns a Tree.exp/stm nest into a flat stm list (canonical trees)
backend/   Codegen.ml   -->  emits textual C-- (not machine code) for each linearized function
main/      main.ml      -->  CLI driver: wires base_tenv/base_venv (builtins), runs the pipeline, prints C--
```

`main/main.ml` defines the Tiger standard library's *type signatures*
(`base_tenv`, `base_venv`) and the runtime import list — the actual
implementations live in `stdlib/` (C and C--) and `runtime/` (GC, allocator,
startup, all C/C--). Those directories are compiled with the 32-bit cross
toolchain, separately from the OCaml compiler, via their own Makefiles
(`stdlib/Makefile`, `runtime/Makefile`) — see the comments there for why
`-fno-omit-frame-pointer` and `-fcommon` are required (the GC walks `%ebp`
frame chains and pre-C99-style tentative definitions are relied on).

Exceptions have two supported codegen strategies: C-- `cut to` (default) and
stack unwinding (`tigerc -unwind`).

### Literate-programming (syncweb) markers

Source files under `parsing/`, `frontend/`, `backend/` are laced with
`(*s: ... *)` / `(*e: ... *)` / `(*x: ... *)` syncweb chunk markers (see the
global CLAUDE.md rule: never edit these by hand). Each directory's `Makefile`
has a `sync` target that regenerates the `.ml`/`.mli` from a corresponding
`.nw` literate doc via `~/github/syncweb`. Note: those `.nw` sources
(`parser.nw`, `ast.nw`, `codegen.nw`, etc.) are not currently present in this
checkout, so `make sync` in these directories isn't currently usable — the
markers are present in anticipation of that workflow. `stdlib/` and
`runtime/` are the same story for their C/C-- files. `docs/principia/Tiger.nw`
is a separate literate book about this compiler.

## Directory map

- `parsing/`, `frontend/`, `backend/`, `main/` — the OCaml compiler (dune libraries `tiger_parsing`, `tiger_frontend`, `tiger_backend`, executable `main`)
- `stdlib/`, `runtime/` — C/C-- standard library and runtime (GC, allocator), built with `CC32`/`qc` per `Makefile.config`, independent of `dune build`
- `tests/` — behavioural test suite (`tiger.tests` manifest, `expected/` baseline, `misc/` stdin fixtures, `x86/*.1` expected stdout)
- `demos/` — small example `.tig` programs
- `docs/` — manual, slides, `docs/original-sml-project/` (the SML compiler this was ported from), `docs/principia/` (literate book)
- `old/`, `todo/` — legacy/unfinished, not part of the active build
