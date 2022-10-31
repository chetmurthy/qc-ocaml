open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_syntax.SYN ;

value g = Grammar.gcreate (Plexer.gmake ());
value qcirc = Grammar.Entry.create g "qcirc";
value qbinding = Grammar.Entry.create g "qbinding";
value env = Grammar.Entry.create g "env";
value env_item = Grammar.Entry.create g "env_item";
value top = Grammar.Entry.create g "top";
value qcirc_eoi = Grammar.Entry.create g "qcirc_eoi";
value top_eoi = Grammar.Entry.create g "top_eoi";
value env_eoi = Grammar.Entry.create g "env_eoi";

value tokens_fun strm =
  list_of_stream_eof (fun [ ("EOI",_) -> True | _ -> False ]) strm ;

value tokens = Grammar.Entry.of_parser g "tokens" tokens_fun ;

value with_input_file fname f arg =
  let oinput_file = Pcaml.input_file.val in do {
    Pcaml.input_file.val := fname ;
    try let rv = f arg in do { Pcaml.input_file.val := oinput_file ; rv }
    with exc -> do {
      Pcaml.input_file.val := oinput_file ;
      raise exc
    }
  }
;

value parse_qelib ?{file="<string>"} s =
  s |> with_input_file file (Grammar.Entry.parse env_eoi)
;

value qelib_from_string s =
  s |> Stream.of_string |> parse_qelib
;

value qelib_from_file s =
  let s = find_file_from ~{path=include_path.val} s in
  s |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Stream.of_string
  |> parse_qelib ~{file=s}
;

EXTEND
  GLOBAL: qcirc qbinding env env_item top
          qcirc_eoi env_eoi top_eoi ;

  env: [ [
    l = LIST0 env_item -> l
  ] ]
  ;

  env_item: [ [
      "gate" ; gname = qgatename ; gargs = gate_args ;
        "=" ; qc = qcirc ; ";" -> QGATE loc (DEF loc gname (gargs, qc))
    | "gate" ; gname = qgatename ; gargs = gate_args ; ";" -> QGATE loc (OPAQUE loc gname gargs)
    | "include" ; s = STRING ; ";" ->
       if Std.ends_with ~{pat=".qli"} s then
         QINCLUDE loc QLAM s (qelib_from_file s)
       else
         Fmt.(raise_failwithf loc "QLAM parser only accepts QLAM (.qli) includes")
    | "coupling_map" ; mname = ident ; "[" ;
      edges = LIST1 [ n = INT ; dir=direction ; m = INT -> (int_of_string n,dir,int_of_string m) ] ;
      positions = [ ";" ; l = LIST1 position -> l | -> [] ] ;
      "]" ; ";" -> QCOUPLING_MAP loc mname (CouplingMap.mk edges positions)
    | "layout" ; mname = ident ; "[" ;
      l = LIST1 layout_item SEP "," ;
      "]" ; ";" -> QLAYOUT loc mname (Layout.mk l)
  ] ]
  ;
  direction: [ [ "->" -> CouplingMap.LR | "<-" -> CouplingMap.RL | "<->" -> CouplingMap.BIDI ] ] ;
  position: [ [ n=INT ; "@" ; "(" ; x=signed_int; ","; y=signed_int ; ")" -> (int_of_string n, (x,y)) ] ] ;
  signed_int: [ [ n= INT -> int_of_string n  | "-" ; n=INT -> - (int_of_string n) ] ] ;
  layout_item: [ [
        OPT "logical" ; lbit = explicit_bit ; ":"  ; pbit = physical_bit -> (lbit, pbit)
      | pbit = physical_bit ; ":" ; OPT "logical" ; lbit = explicit_bit -> (lbit, pbit)
    ] ]
  ;
  gate_args: [ [
      "(" ; pvl = paramvars ; ")" ; (qvl,cvl) = qvars_cvars -> (pvl, qvl, cvl)
  ] ]
  ;

  top: [ [
    e = env ; qc = qcirc -> (e, qc)
    ] ]
  ;

  physical_bit : [ [ "<" ; "physical" ; n = INT ; ">" -> Physical (int_of_string n) ] ] ;
  explicit_bit : [ [ "#" ; n=INT -> BI.EXPLICIT (int_of_string n) ] ] ;
  bitid: [ [ lbit = explicit_bit -> lbit | -> BI.UNIQUE (Unique.mk()) ] ] ;
  qcirc: [ [
      "let" ; l = LIST1 qbinding SEP "and" ; "in" ; qc = qcirc -> QLET loc l qc
    | (qvl,cvl) = paren_qvars_cvars -> QWIRES loc qvl cvl
    | ["qubit"| "qbit"] ; u = bitid ; "(" ; ")" -> QCREATE loc u
    | "qdiscard" ; qv = qvar -> QDISCARD loc qv
    | "barrier" ; qvl = ne_qvars -> QBARRIER loc qvl
    | "measure" ; qv = qvar -> QMEASURE loc qv
    | "reset" ; qv = qvar -> QRESET loc qv
    | gn = qgatename ; pl = params ; (qvl,cvl) = qvars_cvars ->
       QGATEAPP loc gn pl qvl cvl
  ] ]
  ;

  paramvars: [ [ l = LIST0 paramvar SEP "," -> l ] ] ;
  qvars: [ [ l = LIST0 qvar -> l ] ] ;
  ne_qvars: [ [ l = LIST1 qvar -> l ] ] ;
  cvars: [ [ l = LIST0 cvar -> l ] ] ;
  ne_cvars: [ [ l = LIST1 cvar -> l ] ] ;
  qvars_cvars: [ [
      qvl = qvars ; ":" ; cvl = ne_cvars -> (qvl, cvl)
    | qvl = qvars -> (qvl, [])
  ] ]
  ;
  paren_qvars_cvars: [ [
      "(" ; qvl = LIST0 qvar SEP "," ; ":" ; cvl = LIST1 cvar SEP "," ; ")" -> (qvl, cvl)
    | "(" ; qvl = LIST0 qvar SEP "," ; ")" -> (qvl, [])
  ] ]
  ;

  params: [ [
      "(" ; l = LIST0 param SEP "," ; ")" -> l
    | -> []
  ] ] ;

  param: [
    "add" LEFTA [
      e1 = SELF ; "+" ; e2 = SELF -> BINOP loc ADD e1 e2
    | e1 = SELF ; "-" ; e2 = SELF -> BINOP loc SUB e1 e2
    ]
  | "mul" LEFTA [
      e1 = SELF ; "*" ; e2 = SELF -> BINOP loc MUL e1 e2
    | e1 = SELF ; "/" ; e2 = SELF -> BINOP loc DIV e1 e2
    ]
  | "uminus" LEFTA [ "-" ; e1 = SELF -> UNOP loc UMINUS e1 ]
  | "pow" RIGHTA [
      e1 = SELF ; "**" ; e2 = SELF -> BINOP loc POW e1 e2
    ]
  | "simple" [
      id = paramvar -> ID loc id
    | c = paramconst -> CONST loc c
    | "(" ; p = SELF ; ")" -> p
    | uf = ufun ; "(" ; p = param ; ")" -> uf p
    ]
  ]
  ;

  ufun: [ [
      "sin" -> (fun x ->  UFUN loc SIN x)
    | "cos" -> (fun x -> UFUN loc COS x)
    | "tan" -> (fun  x -> UFUN loc TAN x)
    | "exp" -> (fun x -> UFUN loc EXP x)
    | "ln" ->  (fun x -> UFUN loc LN x)
    | "sqrt" -> (fun x -> UFUN loc SQRT x)
    ] ]
  ;

  paramconst: [ [
      f = FLOAT -> REAL (RealNumeral.mk f)
    | n = INT -> NNINT (int_of_string n)
    | "pi" -> PI
    ] ]
  ;

  qbinding: [ [
      (qvl,cvl) = paren_qvars_cvars ; "=" ; qc = qcirc -> (loc, qvl, cvl, qc)
    | qv = qvar ; "=" ; qc = qcirc -> (loc, [qv], [], qc)
  ] ]
  ;

  ident: [ [
       id = LIDENT -> ID.mk id
     | id = UIDENT -> ID.mk id
  ] ]
  ;
  paramvar: [ [ x = ident -> PV loc x ] ] ;
  qvar: [ [ x = ident -> QV loc x ] ] ;
  cvar: [ [ x = ident -> CV loc x ] ] ;
  qgatename: [ [ x = ident -> QG.ofID ~{loc=loc} x ] ] ;

  top_eoi: [ [ x = top ; EOI -> x ] ] ;
  qcirc_eoi: [ [ x = qcirc ; EOI -> x ] ] ;
  env_eoi: [ [ x = env ; EOI -> x ] ] ;

END;

value parse_qcircuit ?{file="<string>"} s =
  s |> with_input_file file (Grammar.Entry.parse top_eoi)
;

value qcircuit_from_string s =
  s |> Stream.of_string |> parse_qcircuit
;

value read_qcircuit s =
  s |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Stream.of_string
  |> parse_qcircuit ~{file=s}
;
