# ------------------------------------------------------------------
# general rules for building sources
# $Id: rules.mk,v 1.11 2004-06-06 23:06:50 govereau Exp $
# ------------------------------------------------------------------

# --- noweb ---

%.ml:D:  %.nw
    notangle -L"$LINE" -R$target $prereq > $target

%.mli:  %.nw
    notangle -L"$LINE" -R$target $prereq | cpif $target

%.c:D:  %.nw
    notangle -L"$LINE" -R$target $prereq > $target

%.h:  %.nw
    notangle -L"$LINE" -R$target $prereq | cpif $target

%.c--:D:  %.nw
    notangle -L"$LINE" -R$target $prereq > $target

# --- ocaml ---

%.cmi: %.mli
    ocamlc $OCAML_OPTS -c $stem.mli

%.cmo: %.ml
    ocamlc $OCAML_OPTS -c $stem.ml

%-p.cmo: %.ml
    if [ -r $stem.cmo ]; then /bin/mv -f $stem.cmo $stem-noprof.cmo; fi
    ocamlcp $OCAML_OPTS -o $target -c $stem.ml && /bin/mv -f $stem.cmo $target
    if [ -r $stem-noprof.cmo ]; then /bin/mv -f $stem-noprof.cmo $stem.cmo; fi

%.cma: %.cmo
    ocamlc $OCAML_OPTS -a $prereq -o $target

%.cmx: %.ml
    ocamlopt $OCAML_OPTS -c $stem.ml

%-p.cmx: %.ml
    if [ -r $stem.cmx ]; then /bin/mv -f $stem.cmx $stem-noprof.cmx; fi
    if [ -r $stem.o ]; then /bin/mv -f $stem.o $stem-noprof.o; fi
    ocamlopt -o $target -p $OCAML_OPTS -c $stem.ml &&
	mv $stem.cmx $target &&
	mv $stem.o   $stem-p.o
    if [ -r $stem-noprof.cmx ]; then /bin/mv -f $stem-noprof.cmx $stem.cmx; fi
    if [ -r $stem-noprof.o ]; then /bin/mv -f $stem-noprof.o $stem.o; fi

%.ml: %.mll
    ocamllex $stem.mll

%.mli %.ml: %.mly
    ocamlyacc -v $stem.mly

# --- c code ---

%.o: %.c
    gcc -Wall -I $QCMMINC -c $stem.c -o $stem.o

%.a: %.o
    ar cr $target $prereq


# --- latex ---
%.inc:D: %.nw
    noweave -n $prereq > $target

%.tex:D: %.nw
    noweave $prereq > $target

%.dvi: %.tex
	latex '\scrollmode \input '"$stem"
	ltxcount=3
	while egrep -s 'Rerun (LaTeX|to get cross-references right)' $stem.log &&
	      [ $ltxcount -gt 0 ]
	do
	  latex '\scrollmode \input '"$stem"
	  ltxcount=`expr $ltxcount - 1`
	done

%.ps: %.dvi
    dvips -Ppdf -o $target $prereq

%.pdf: %.ps
    ps2pdf $prereq

# --- html ---
%.html:D: %.nw
    noweave -filter l2h -filter ./autodefs.ocaml -index -html $prereq > $target
