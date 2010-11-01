
                   Tiger front-end for C--

This directory contains source code for a Tiger language
compiler. This compiler is a front end for the Quick C-- compiler
primarily used for testing Quick C--. The Tiger language is described
in "Modern Compiler Implementation in ML" by Andrew W. Appel. To quote
from Appendix A of this book:

  The Tiger language is a small language with nested functions,
  record values with implicit pointers, arrays, integer and
  string variables, and a few simple structured control constructs.

BUILDING
----------------------------------------------------------------------

This version of the tiger compiler requires OCaml, GCC, and a binary
version of Quick C-- to build. To build the sources, use the build.sh
script.

  ./build.sh

The following files will be produced:

   tigerc     - the tiger compiler
   stdlib.a   - the tiger standard library
   runtime.o  - the stand-alone startup code

USING THE COMPILER
----------------------------------------------------------------------
The tigerc program converts a tiger program into an equivalent c--
program which can then be compiled with qc--. Since this compiler is
intended to be used for testing qc--, it does not attempt to call qc--
or the system linker to produce an executable program. In it's most
basic usage:

   tigerc source.tig > source.c--

The tiger compiler supports two implementations of exceptions. The
default implementation uses a C-- "cut to". Alternatively, you can
generate code for the stack unwinding implementation:

  tigerc -unwind source.tig > source.c--

For additional command line options see:

  tigerc -help

A simple standard library and startup code are included with the
compiler. In order to create an executable, these will have to be
linked with the final result. The qc-- compiler can handle this for
us: 

   qc-- -globals -o source runtime.o stdlib.a source.c--
