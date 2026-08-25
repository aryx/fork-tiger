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

Requires OCaml, GCC (or, on macOS, clang), an i686 cross toolchain (unless
building for `-arm64` — see below), and a built/installed `qc--`.

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

`./configure` also picks `DEFAULT_BACKEND`, what the plain `make`/`make test`
above actually build: normally `x86`, but on a genuine Apple Silicon Mac (no
`i686-linux-gnu-gcc` on PATH, since that's a Linux-only cross-compiler with
no macOS equivalent) it defaults to `arm64` instead — the one backend
guaranteed to build there, since it's the host's own native architecture.
Every other Makefile/script here (`stdlib/Makefile`, `runtime/Makefile`,
`demos/Makefile`, `tests/run-tests.sh`) still defaults to `x86` on its own
regardless — only the top-level `Makefile`'s `all`/`test` read
`DEFAULT_BACKEND`. See the "Other qc-- backends" section below for how to
target any backend explicitly (`make test-arm64`, `BACKEND=arm64 ...`).

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

### Other qc-- backends

qc-- targets more than x86 (see `qc --help`): `-ppc-elf`, `-sparc`, `-alpha`,
`-mips`, `-arm`, `-riscv32`, `-riscv64`, `-arm64`, alongside the default
`-x86`. `stdlib/Makefile` and `runtime/Makefile` take a `BACKEND=<arch>`
argument (default `x86`; `<arch>` one of
`ppc sparc alpha mips arm riscv32 riscv64 arm64`) and build into their own
`build-<arch>/` instead of clobbering the x86 objects. `demos/Makefile` and
`tests/run-tests.sh` follow the same `BACKEND=<arch>` convention
(`run-tests.sh` reads it from the environment rather than as a `make`
argument). `arm64` is the odd one out: it is this host's own native
architecture on an Apple Silicon Mac (Mach-O, not ELF), needing no cross
toolchain and no `qemu-user` at all — see `docs/claude_notes/notes_64bits.txt`'s
own arm64 section.

```sh
make test-ppc                       # builds stdlib+runtime for BACKEND=ppc, then runs the suite
make test-sparc test-alpha ...      # ditto for the other backends; make test-all runs every one
BACKEND=ppc tests/run-tests.sh      # same suite, driven directly
```

Same manifest, same `tests/x86/*.1`/`*.2` expected output for every backend
(Tiger's observable behaviour isn't meant to depend on the target). `./configure`
detects each backend's optional cross toolchain (`CC_<ARCH>`/`AR_<ARCH>`/
`RUN_<ARCH>` in `Makefile.config`, best-effort — a missing one only warns)
and, where relevant, its `qemu-user` emulator.

tigerc itself always emits 32-bit little-endian C-- (`target byteorder
little`, `bits32` types) by default; each backend needs one of four
transforms before qc-- will accept the result, applied on the fly by the
Makefiles/`run-tests.sh` rather than by editing the checked-in `.c--`
sources:

| transform | backends | what happens |
|---|---|---|
| none | mips, riscv32 | already matches (32-bit little-endian, has an FPU) |
| byteorder flip | ppc, sparc | `little` → `big` (both are 32-bit big-endian) |
| float "none" splice | arm | arm has no FPU; every source here otherwise relies on the implicit ieee754 default |
| `-64` / bits64 rewrite | alpha, riscv64, arm64 | all three are 64-bit; `tigerc -64` emits bits64 C-- directly (see `docs/claude_notes/notes_64bits.txt`), but `runtime/alloc.c--`, `runtime/runtime.c--` and `stdlib/stdlibcmm.c--` are hand-written and need every `bits32`→`bits64`, `+4`→`+8`, and the allocator's alignment mask rewritten by hand (`XFORM=64` in `runtime/Makefile`/`stdlib/Makefile`) |

riscv32 is also the only backend with no glibc cross toolchain on Ubuntu; its
`CC_RISCV32` is a bare-metal `gcc-riscv64-unknown-elf` compiled against
picolibc, and its final link (in `demos/Makefile` and `run-tests.sh`) goes
through plain `ld` with an explicit `riscv32_crt0.o`, not `$(CC_RISCV32)` —
see either file's riscv32 section for why (picolibc.specs' default link
script and `--gc-sections` are both wrong for this target).

`arm64` is the other exception to "link with `$(CC) -static ... $(QCPCMAP)`":
Apple does not support static-linking libSystem at all, so its final link
in `demos/Makefile`/`run-tests.sh` drops `-static`; and Mach-O's linker
(`ld64`) has no `-T`/linker-script mechanism at all, so `$(QCPCMAP)` (a
GNU-ld script fragment) is dropped from the link line too — qc--'s own
`-arm64` backend needs no equivalent (see qc--'s own
`docs/claude_notes/notes_arm64.txt`).

None of the non-x86 backends is expected to be all-green — see each
`tests/expected/tiger-<arch>.txt` for the current pass/fail split. As of the
last recorded baselines: ppc and arm and riscv32 are fully green (14/14);
riscv64 13/14 and alpha 11/14 fail on real qc-- instruction-selection gaps
(`%quot`/`%sx` at 64-bit widths on alpha; a `malloc` corruption on riscv64's
`colmajor`); sparc and mips are the least complete backends (2/14 and 1/14),
mostly around exception/GC codegen — consistent with qc--'s own (much
simpler) `tests/run-tiger-<arch>.sh` baselines in the qc-- checkout, so this
reflects real backend completeness, not a gap in this suite's own
infrastructure. `arm64` is newer and lands at 6/14 — mostly a shared
GC-allocation assertion also seen on qc--'s own `tests/tiger64/` suite for
`-arm64`, not yet root-caused (see `docs/claude_notes/notes_64bits.txt`'s
arm64 section and qc--'s own `notes_arm64.txt`).

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
