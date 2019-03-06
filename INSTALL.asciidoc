
How to set up your environment for this tool (for Ubuntu)

(1) install "opam" on Ubuntu

  % sudo apt-get install opam

(2) install Ocaml 4.07.1
--------------------------------------------------------------------------------
export VERSION=4.07.1

export OPAMROOT=$HOME/Hack/Ocaml/$VERSION

mkdir -p $OPAMROOT
echo "export OPAMROOT=$OPAMROOT" >> $OPAMROOT/dot.bashrc

opam init --dot-profile=$OPAMROOT/dot.bashrc \
    -j 32 \
    --yes \
    --bare \
    --inplace-build \
    --enable-completion \
    --enable-shell-hook \
    --shell-setup

eval $(opam env)

opam switch create ocaml-base-compiler.$VERSION --repositories=default,beta=git+https://github.com/ocaml/ocaml-beta-repository.git
--------------------------------------------------------------------------------

And to set up the environment in a shell for each of these installs, just source the dot.bashrc in $OPAMROOT/dot.bashrc

--------------------------------------------------------------------------------

Ocaml version: 4.07.1

OPAM packages used by qc-ocaml:

WARNING TODO: ocamldot still cannot be installed by opam -- figure this out and fix

opam install camlp5 camlzip cmdliner conf-gnutls \
     ocamlfind lablgtk3 menhir oasis containers \
	 ocamlfind ocamlgraph ocamlnet ocurl ounit ppx_test \
	 pcre ssl ppx_deriving_yojson yojson ocaml-inifiles \
	 ppx_deriving_cmdliner

