# Makefile,v
# Copyright (c) INRIA 2007-2017

WD=$(shell pwd)
TOP=..
include $(TOP)/config/Makefile

DESTDIR=
RM=rm

OCAMLFIND=ocamlfind
PACKAGES := re

BIN = ya-wrap-ocamlfind

ML= \
	ya_wrap_ocamlfind.ml \


CMO := $(ML:.ml=.cmo)
CMI := $(ML:.ml=.cmi)
CMX := $(ML:.ml=.cmx)
CMT := $(ML:.ml=.cmt)
CMTI := $(MLI:.mli=.cmti)

all: $(BIN)

perl:
	cp ya-wrap-ocamlfind.PL ya-wrap-ocamlfind

.PRECIOUS: $(CMO) $(CMX)

ya-wrap-ocamlfind: ya_wrap_ocamlfind.ml
	$(OCAMLFIND) ocamlc -linkall -linkpkg -package $(PACKAGES) $< -o $@

clean::
	rm -f *.cm* ya-wrap-ocamlfind

depend::
	echo "$(CMO) $(CMI) $(CMX): $(EXTERNAL)" > .depend.NEW
	$(OCAMLFIND) ocamldep $(OCAMLCFLAGS) -package $(PACKAGES) \
	$(ML) \
	>> .depend.NEW
	mv .depend.NEW .depend

-include .depend
