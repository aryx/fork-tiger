# ------------------------------------------------------------------
# mkfile for tigerc compiler
# $Id: mkfile,v 1.33 2004-07-19 22:00:27 govereau Exp $
# ------------------------------------------------------------------

NAME    = tigerc
VERSION = `date +%Y.%m.%d`

# load configure settings
<|./configure

#
# misc. options
#
LINE       = '# %L "%F"%N'
OCAML_OPTS =
P          =

### Everything below this line should not need to be edited ###

# ------------------------------------------------------------------
# source files - in order
# ------------------------------------------------------------------
ML = option.ml error.ml symbol.ml ast.ml parser.ml lexer.ml \
     tree.ml frame.ml environment.ml translate.ml semantics.ml \
     canonical.ml codegen.ml \
     driver.ml

MLI = option.mli error.mli symbol.mli ast.mli parser.mli \
      tree.mli frame.mli environment.mli translate.mli semantics.mli \
      canonical.mli codegen.mli

CMO = ${ML:%.ml=%$P.cmo}
CMX = ${ML:%.ml=%$P.cmx}

# .nw files for documentation
NW = symbol.nw ast.nw parser.nw \
     environment.nw semantics.nw tree.nw \
     translate.nw frame.nw \
     canonical.nw codegen.nw \
     driver.nw error.nw option.nw \
     stdlib.nw gc.nw runtime.nw

INC  = ${NW:%.nw=%.inc}
HTML = ${NW:%.nw=%.html}

# ------------------------------------------------------------------
# rules
# ------------------------------------------------------------------
<rules.mk

all:V:      bin     tigerrt
bin:V:      $NAME$P
all.opt:V:  $NAME$P.opt tigerrt
doc:V:      doc.pdf doc.ps
depend:V:   DEPEND
html:V:	    $HTML index.html
tar:V:      tigerc.$VERSION.tar.gz

$NAME: dep-chk $CMO
    ocamlc  $OCAML_OPTS -o $target $CMO

$NAME-p: dep-chk $CMO
    ocamlcp $OCAML_OPTS -o $target $CMO

$NAME$P.opt: dep-chk $CMX
    ocamlopt $P $OCAML_OPTS -o $target $CMX

# documentation
doc.dvi: doc.tex tiger.eps $INC
index.html:D: doc.tex
    noweave -filter l2h -html $prereq > $target

tiger.eps:D: $ML $MLI
    ocamldep $ML $MLI                | \
    ocamldot                         | \
    egrep -v "(Symbol|Option|Error)" | \
    dot -Tps                         > tiger.eps

# tar ball
# put client back into tarball
#              tigerc-$VERSION/client    \
tigerc.$VERSION.tar.gz: all.opt
    ln -s . tigerc-$VERSION
    tar czf   tigerc.$VERSION.tar.gz    \
              tigerc-$VERSION/*.nw      \
              tigerc-$VERSION/*.ml      \
              tigerc-$VERSION/*.mli     \
              tigerc-$VERSION/*.c       \
              tigerc-$VERSION/mkfile    \
              tigerc-$VERSION/rules.mk  \
              tigerc-$VERSION/README    \
              tigerc-$VERSION/doc.tex   \
              tigerc-$VERSION/tigerc    \
              tigerc-$VERSION/tigerc.opt\
              tigerc-$VERSION/test/*.tig
    rm -f tigerc-$VERSION

# rule overrides
parser.mly: parser.nw
    notangle -R$target $prereq | cpif $target

parser.ml parser.mli: parser.mly
    ocamlyacc -v $prereq

lexer.mll : parser.nw
    notangle -R$target $prereq | cpif $target

# ------------------------------------------------------------------
# Tiger runtime and interpreter client
# ------------------------------------------------------------------

tigerrt:V: stdlib.a runtime.o #client

# required libraries
ILIBS = -lm -llua40 -llualib40 -lqc--interp

#
# runtime dependencies
#
runtime.o : runtime.c--
    $QCMM -stop .o -o $target $prereq

gc.o : gc.c gc.h

alloc.c--:D: gc.nw
	notangle -R$target $prereq | cpif $target
alloc.o: alloc.c--
	$QCMM -stop .o -o $target $prereq

stdlibcmm.c--:D: stdlib.nw
	notangle -R$target $prereq | cpif $target
stdlibcmm.o: stdlibcmm.c--
	$QCMM -stop .o -o $target $prereq

stdlib.o : stdlib.c stdlib.h gc.h
stdlib.a : stdlib.o stdlibcmm.o gc.o alloc.o

client.c:D: runtime.nw
    notangle -L"$LINE" -R$target $prereq > $target
client.o: client.c
client: client.o stdlib.o stdlibcmm.o gc.o
    gcc -Wall -g -I $QCMMINC -o $target $prereq -L $QCMMLIB $ILIBS 

# ------------------------------------------------------------------
# cleaning
# ------------------------------------------------------------------
clean.opt:V:
            rm -f *.cmx *.o *.cmxa *.a

clean:V:    clean.opt
            rm -f *.cmi *.cmo *.cma

clobber:V:  clean
            rm -f DEPEND
            rm -f *.ml *.mli *.c *.h *.c-- *~
            rm -f *.mll *.mly *.mlb *.output
            rm -f *.inc *.dvi *.log *.aux
            rm -f *.ps doc.pdf *.html
            rm -f tiger.dot tiger.eps
            rm -f $NAME $NAME.opt client

# ------------------------------------------------------------------
# dependencies
# ------------------------------------------------------------------

dep-chk:VQ:
    if test DEPEND -nt mkfile; then true;
    else
      echo
      echo "     You must run 'mk depend'"
      echo 
      exit 1
    fi

DEPEND:D: $ML $MLI mkfile
    ocamldep $prereq | sed 's/\.cmo/$P.cmo/g;s/\.cmx/$P.cmx/g' > DEPEND

<DEPEND



####### can't wait for PG

NRDEST=/usr/local/lib/tiger
NRQCMM=/usr/local/lib/qc--

nrinstall:QV: tigerc runtime.o stdlib.a
	mkdir -p $NRDEST
	copy -v $prereq $NRDEST
		notangle  -L'$file "%F"%N$line %L%N' -Rtiger.lua \
	   ~nr/c--/qc--/doc/release.nw > $NRQCMM/tiger.lua
