# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

LAUNCH=env TOP=$(TOP) $(TOP)/tools/LAUNCH
OCAMLFIND=$(LAUNCH) ocamlfind
NOT_OCAMLFIND=$(LAUNCH) not-ocamlfind
MKCAMLP5=$(LAUNCH) mkcamlp5
PACKAGES := $(PACKAGES),oUnit,pa_ppx.testutils,pa_ppx.runtime_fat,qc_ocaml
SYNTAX := camlp5o

TESTS= \
	qlam_tests.byte qasm2_tests.byte qasm0_tests.byte \

LARGE_TESTS= \
	large_tests.byte \

ALL_TESTS = $(TESTS) $(LARGE_TESTS)

ML=\
	qlam_tests.ml qasm2_tests.ml qasm0_tests.ml large_tests.ml


CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(ALL_TESTS) $(ALL_TESTS:.byte=.opt) $(TESTS:.byte=.test)
large-tests: $(LARGE_TESTS) $(LARGE_TESTS:.byte=.opt) $(LARGE_TESTS:.byte=.test)

%.test : %.opt
	mkdir -p _build && ./$<

.PRECIOUS: $(CMO) $(CMX)
%_tests.byte: %_tests.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall -syntax camlp5o $^ -o $@

%_tests.opt: %_tests.cmx
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall -syntax camlp5o $^ -o $@

clean::
	rm -f  *.ppo.*

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) -syntax camlp5o \
	*.ml \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend

.ml.cmo:
#	$(NOT_OCAMLFIND) preprocess $(OCAMLCFLAGS) -ppopt -sep -ppopt ";;\n" -package $(PACKAGES),camlp5.pr_o -syntax $(SYNTAX) $< > $@.ppo.ml
#	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $@.ppo.ml
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<

.ml.cmx:
	$(OCAMLFIND) ocamlopt $(DEBUG) $(WARNERR) $(OCAMLCFLAGS) -package $(PACKAGES) -syntax $(SYNTAX) -c $<
