# Copyright 2019 Chetan Murthy, All rights reserved.

OCAMLFIND=ocamlfind
OCAMLCFLAGS=-g
PACKAGES=rresult,uuidm,nettls-gnutls,netclient,ppx_deriving_yojson,ppx_sexp_conv,oUnit,oUnit.advanced,pcre,ocamlgraph,dot,yojson,containers,inifiles
PACKAGESP5=camlp5,$(PACKAGES) -syntax camlp5o

ML= misc_functions.ml exc.ml http_helpers.ml yojson_helpers.ml gmap.ml gset.ml coll.ml qc_environment.ml qasmsyntax.ml qc_symbolic.ml qasmlex.ml qasmparser.ml qasmpp.ml qasmdag0.ml qasm_passes.ml qc_layout.ml qobj_types.ml qobj_compile.ml qrpc_types.ml qrpc_api.ml
MLI= gmap.mli gset.mli

SRC=exc.ml http_helpers.ml yojson_helpers.ml qc_environment.ml qasmlex.ml qasmsyntax.ml qc_symbolic.ml coll.ml gmap.ml gset.ml qc_tests.ml large_tests.ml qobj_types.ml qobj_compile.ml qrpc_types.ml qrpc_api.ml
SRCP5=qasmparser.ml qasmpp.ml misc_functions.ml qasmdag0.ml qasm_passes.ml qc_layout.ml

CMO= $(ML:.ml=.cmo)
CMX= $(ML:.ml=.cmx)
CMI= $(ML:.ml=.cmi)
OBJECTS = $(CMO) $(CMX) $(CMI)

RESULT=libqasm
TESTS=qc_tests.byte large_tests.byte

all: $(RESULT).cma $(RESULT).cmxa $(TESTS)

everything::
	make realclean
	make .depend
	make all
	make test

large-test: large_tests.byte
	./large_tests.byte

test:: qc_tests.byte
	./qc_tests.byte

$(RESULT).cma: $(CMO)
	$(OCAMLFIND) ocamlc -a -o $(RESULT).cma $(CMO)

$(RESULT).cmxa: $(CMX)
	$(OCAMLFIND) ocamlopt -a -o $(RESULT).cmxa $(CMX)

qc_tests.byte: $(RESULT).cma qc_tests.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $^

large_tests.byte: $(RESULT).cma large_tests.cmo
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -linkpkg -linkall -o $@ $^

$(SRC:.ml=.cmo): %.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

$(SRC:.ml=.cmx): %.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

$(SRCP5:.ml=.cmo): %.cmo: %.ml
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGESP5) -c $<

$(SRCP5:.ml=.cmx): %.cmx: %.ml
	$(OCAMLFIND) ocamlopt $(OCAMLCFLAGS) -package $(PACKAGESP5) -c $<

clean::
	rm -f *.cm* *.o *.a *.byte *.opt qasmlex.ml oUnit*

realclean:: clean
	rm -f .depend

.SUFFIXES: .cmi .cmo .cmx .ml .mli .mll

.mll.ml:
	ocamllex $<

.mli.cmi:
	$(OCAMLFIND) ocamlc $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.NOTPARALLEL:

.depend: $(SRC) $(SRCP5)
	$(OCAMLFIND) ocamldep -package $(PACKAGES) $(SRC) > .depend.NEW && \
	$(OCAMLFIND) ocamldep -package $(PACKAGESP5) $(SRCP5) >> .depend.NEW && \
	mv .depend.NEW .depend
-include .depend
