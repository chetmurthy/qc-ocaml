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
PACKAGES := $(PACKAGES),bos,rresult,uuidm,nettls-gnutls,netclient,pa_ppx.base,pa_ppx.utils,pa_ppx.deriving_plugins.sexp,pa_ppx.deriving_plugins.yojson,pcre,ocamlgraph,dot,yojson,inifiles,pa_ppx.deriving_plugins.std,camlp-streams

TARGET := qc_ocaml.cma
ML= misc_functions.ml exc.ml http_helpers.ml yojson_helpers.ml \
	qc_environment.ml qasmsyntax.ml qc_symbolic.ml qasm2_lexer.ml \
	qasmparser.ml qasmpp.ml qasmdag0.ml qasm_io.ml qasm_passes.ml qc_layout.ml \
	qobj_types.ml qobj_compile.ml qrpc_types.ml qrpc_api.ml qasm0_tokens.ml qasm0_lexer.ml qasm0_2circ.ml
MLI= gmap.mli gset.mli

CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(TARGET) $(TARGET:.cma=.cmxa)
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

doc: $(CMT) $(CMTI)


$(TARGET): $(CMO)
	$(OCAMLFIND) ocamlc $(DEBUG) $^ -a -o $(TARGET)

$(TARGET:.cma=.cmxa) $(TARGET:.cma=.a): $(CMX)
	$(OCAMLFIND) ocamlopt $(DEBUG) $^ -a -o $(TARGET:.cma=.cmxa)

META: META.pl
	./META.pl > META

install::
	mkdir -p $(DESTDIR)/lib
	./META.pl $(DESTDIR)/lib > META
	$(NOT_OCAMLFIND) reinstall-if-diff qc_ocaml -destdir $(DESTDIR)/lib META $(TARGET) $(TARGET:.cma=.cmxa) $(TARGET:.cma=.a) $(CMI) $(wildcard *.cmt*)
	$(RM) -f META

local-install::
	$(MAKE) DESTDIR=$(WD)/$(TOP)/local-install/ install

clean::
	rm -rf META

EXTERNAL := $(shell $(OCAMLFIND) query -predicates byte -format '%m' $(PACKAGES) | grep local-install)
$(CMO) $(CMI) $(CMX): $(EXTERNAL)

depend:: qasm0_lexer.ml qasm2_lexer.ml
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(DEBUG) -package $(PACKAGES) -syntax camlp5o $(ML) $(MLI) >> .depend.NEW \
		&& mv .depend.NEW .depend

-include .depend
