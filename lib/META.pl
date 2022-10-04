#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "qc-ocaml" library:
version = "$Version::version"
description = "qc-ocaml library"

archive(byte) = "qc_ocaml.cma"
archive(native) = "qc_ocaml.cmxa"
requires = "vector,bos,rresult,uuidm,nettls-gnutls,netclient,camlp-streams,pa_ppx.base.link,pa_ppx.utils,pa_ppx.runtime_fat,pcre,ocamlgraph,dot,yojson,inifiles"

EOF
