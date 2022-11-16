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
PACKAGES := $(PACKAGES),pcre,oUnit,pa_ppx.testutils,pa_ppx.runtime_fat,qc_ocaml,pa_ppx.deriving_plugins.show
SYNTAX := camlp5o

TESTS= \
	qlam_tests.byte qclatex_tests.byte qcdot_tests.byte qasm2_tests.byte\

LARGE_TESTS= \
	large_tests.byte \

ALL_TESTS = $(TESTS) $(LARGE_TESTS)

ML= \
	test_helpers.ml \
	qlam_tests.ml qclatex_tests.ml qcdot_tests.ml qasm2_tests.ml large_tests.ml


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
%_tests.byte: test_helpers.cmo %_tests.cmo
	$(OCAMLFIND) ocamlc $(DEBUG) $(WARNERR) -package $(PACKAGES) \
	-linkpkg -linkall -syntax camlp5o $^ -o $@

%_tests.opt: test_helpers.cmx %_tests.cmx
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
