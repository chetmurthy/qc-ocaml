# Copyright 2019 Chetan Murthy

OCAMLFIND=ocamlfind
OCAMLCFLAGS=
PACKAGES=-package camlp5,oUnit,oUnit.advanced -syntax camlp5o

OCAMLMKLIB=ocamlmklib
OCAMLMKLIB_FLAGS=

ML = qasmsyntax.ml qasmlex.ml
MLI = 


CMO= $(ML:.ml=.cmo)
CMX= $(ML:.ml=.cmx)
CMI= $(ML:.ml=.cmi)
OBJECTS = $(CMO) $(CMX) $(CMI)

RESULT=libqasm

all: $(RESULT).cma $(RESULT).cmxa dll$(RESULT).so

test:: qasmlexer_tests.byte
	./qasmlexer_tests.byte

$(RESULT).cma $(RESULT).cmxa dll$(RESULT).so: $(OBJECTS)
	    $(OCAMLMKLIB) -verbose -o $(RESULT) $(CMO) $(CMX) $(OCAMLMKLIB_FLAGS)


qasmlexer_tests.byte: qasmlexer_tests.cmo $(RESULT).cma
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) $(PACKAGES) -linkpkg -linkall -o $@ $^

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

.depend: *.ml*
	$(OCAMLFIND) ocamldep $(PACKAGES) $^ > .depend.NEW && mv .depend.NEW .depend
-include .depend
