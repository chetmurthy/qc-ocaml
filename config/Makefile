# Makefile,v
# Copyright (c) INRIA 2007-2017

RM=rm

YAWRAP=$(TOP)/tools/ya-wrap-ocamlfind
OCAMLFIND=ocamlfind
NOT_OCAMLFIND=not-ocamlfind
OCAMLTOPLEVEL=ocaml
CAMLP5LIB=$(shell $(OCAMLFIND) query camlp5)
MKCAMLP5=mkcamlp5
CAMLP5R=camlp5r -I $(CAMLP5LIB)
INCLUDES=
OCAMLCFLAGS= $(INCLUDES)
PACKAGES=unix,camlp5
PACKAGES := $(PACKAGES),camlp5.parser_quotations
#PACKAGES := $(PACKAGES),camlp5.quotations
#SYNTAX = camlp5o
PRPACKAGE = camlp5.pr_o

export PERL5LIB := $(TOP):$(PERL5LIB)

top: all

.mli.cmi:
	$(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmo:
#	$(YAWRAP) $(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -package $(PACKAGES),$(PRPACKAGE) $< > $*_ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o -c $*_ppo.ml
	$(YAWRAP) $(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmx:
	$(YAWRAP) $(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -c $<

.ml.cmt:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmo
	cp .cmt/$*.cmt $*.cmt

.mli.cmti:
	@mkdir -p .cmt
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES),pa_ppx_dock -bin-annot -c $< -o .cmt/$*.cmi
	cp .cmt/$*.cmti $*.cmti

.mll.ml:
	ocamllex $<

.SUFFIXES: .cmo .cmi .mll .ml .mli .cmx .cma .cmxa .cmt .cmti

clean::
	$(RM) -rf *.cm* *.o *.a *.byte *.opt *.log *.cache *.ppo *ppo.ml _build/* .cmt *.odoc .depend.NEW

realclean:: clean
