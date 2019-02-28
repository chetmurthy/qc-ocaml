# Copyright 2019 Chetan Murthy, All rights reserved.

OCAMLFIND=ocamlfind
OCAMLCFLAGS=-g
PACKAGES=-package oUnit,oUnit.advanced,pcre,ocamlgraph,dot
PACKAGES1=-package camlp5,oUnit,oUnit.advanced,pcre,ocamlgraph,dot -syntax camlp5o

ML= misc_functions.ml gmap.ml gset.ml coll.ml qasmsyntax.ml qasmlex.ml qasmparser.ml qasmpp.ml qasmdag0.ml
MLI= gmap.mli gset.mli

SRC=qasmlex.ml qasmsyntax.ml coll.ml gmap.ml gset.ml qasmsyntax_tests.ml large_tests.ml
SRCP5=qasmparser.ml qasmpp.ml misc_functions.ml qasmdag0.ml

CMO= $(ML:.ml=.cmo)
CMX= $(ML:.ml=.cmx)
CMI= $(ML:.ml=.cmi)
OBJECTS = $(CMO) $(CMX) $(CMI)

RESULT=libqasm
TESTS=qasmsyntax_tests.byte large_tests.byte

all: $(RESULT).cma $(RESULT).cmxa $(TESTS)

everything::
	make realclean
	make .depend
	make all
	make test

large-test: large_tests.byte
	./large_tests.byte

test:: qasmsyntax_tests.byte
	./qasmsyntax_tests.byte

$(RESULT).cma: $(CMO)
	$(OCAMLFIND) ocamlc -a -o $(RESULT).cma $(CMO)

$(RESULT).cmxa: $(CMX)
	$(OCAMLFIND) ocamlopt -a -o $(RESULT).cmxa $(CMX)

qasmsyntax_tests.byte: $(RESULT).cma qasmsyntax_tests.cmo
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES) -linkpkg -linkall -o $@ $^

large_tests.byte: $(RESULT).cma large_tests.cmo
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES) -linkpkg -linkall -o $@ $^

qasmparser.cmo: qasmparser.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES1) -c $<

qasmparser.cmx: qasmparser.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES1) -c $<

qasmpp.cmo: qasmpp.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES1) -c $<

qasmpp.cmx: qasmpp.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES1) -c $<

#qasmsyntax.cmo: qasmsyntax.ml
#	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES1) -c $<

#qasmsyntax.cmx: qasmsyntax.ml
#	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES1) -c $<

qasmdag0.cmo: qasmdag0.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES1) -c $<

qasmdag0.cmx: qasmdag0.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES1) -c $<

misc_functions.cmo: misc_functions.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES1) -c $<

misc_functions.cmx: misc_functions.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES1) -c $<

clean::
	rm -f *.cm* *.o *.a *.byte *.opt qasmlex.ml oUnit*

realclean:: clean
	rm -f .depend

.SUFFIXES: .cmi .cmo .cmx .ml .mli .mll

.ml.cmo:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES) -c $<

.mll.ml:
	ocamllex $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) $(PACKAGES) -c $<

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES) -c $<

.NOTPARALLEL:

.depend: $(SRC) $(SRCP5)
	$(OCAMLFIND) ocamldep $(PACKAGES) $(SRC) > .depend.NEW && \
	$(OCAMLFIND) ocamldep $(PACKAGES1) $(SRCP5) >> .depend.NEW && \
	mv .depend.NEW .depend
-include .depend
