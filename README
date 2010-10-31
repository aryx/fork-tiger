
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
In order to build the Tiger compiler, you will first need a Quick C--
source tree that has been successfully built. Some components of the
Tiger compiler depend on intermediate build results from the Quick C--
source tree.

Edit the mkfile and set the location of your qc-- source tree. Also,
if the noweb tools or ocaml are not in your PATH, you will need to
specify the locations of each of these.

Build the dependencies file:

   mk depend

Build the compiler, libraries, and tools:

   mk all

Build a native binary "tigerc.opt": (optional)

   mk all.opt

Build source documentation: (optional)

   mk doc

The following files are produced:

   tigerc     - the tiger compiler
   tigerc.opt - the tiger compiler native binary (optional)
   stdlib.a   - the tiger standard library
   runtime.o  - the stand-alone startup code
   client     - a qc-- interpreter client for tiger
   tigerc.ps  - source code documentation (optional)
   tigerc.pdf - source code documentation (optional)


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

The Quick C-- interpreter client can be used to run code compiled for
the interpreter. The client registers the tiger standard library
functions and sets up the garbage collector before running the source
program:

   qc-- -interp source.c--
   client source.qs


ADDITIONAL NOTES
----------------------------------------------------------------------
