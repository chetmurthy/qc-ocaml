open Pa_ppx_utils ;
open Qc_misc ;
open Qlam_syntax ;

value include_path = ref [] ;
value add_include (s : string) = Std.push include_path s ;

value g = Grammar.gcreate (Plexer.gmake ());
value qcirc = Grammar.Entry.create g "qcirc";
value qgate = Grammar.Entry.create g "qgate";
value qbinding = Grammar.Entry.create g "qbinding";
value env = Grammar.Entry.create g "env";
value env_item = Grammar.Entry.create g "env_item";
value top = Grammar.Entry.create g "top";


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

value read_include s =
  let s = find_file_from ~{path=include_path.val} s in
  s |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Stream.of_string
  |> with_input_file s (Grammar.Entry.parse env)
;

EXTEND
  GLOBAL: qcirc qgate qbinding env env_item top ;

  env: [ [
    l = LIST0 env_item -> l
  ] ]
  ;

  env_item: [ [
      "gate" ; gname = qgatename ; "(" ; pvl = paramvars ; ")" ; (qvl,cvl) = qvars_cvars ;
        "=" ; qc = qcirc ; ";" -> QEnv.QGATEDEF gname (pvl, qvl, cvl, qc)
              | "include" ; s = STRING ; ";" -> QEnv.QINCLUDE s (read_include s)
  ] ]
  ;

  top: [ [
    e = env ; qc = qcirc -> (e, qc)
    ] ]
  ;

  qgate: [ [
      gname = qgatename -> QC.QGATE loc gname
    | "gatefun" ; "[" ; "(" ; pvl = paramvars ; ")" ; (qvl,cvl) = qvars_cvars ;
      qc = qcirc ; "]" -> QC.QGATELAM loc (pvl, qvl, cvl, qc)
  ] ]
  ;

  qcirc: [ [
      "let" ; l = LIST1 qbinding SEP "and" ; "in" ; qc = qcirc -> QC.QLET loc l qc
    | (qvl,cvl) = paren_qvars_cvars -> QC.QWIRES loc qvl cvl
    | ["qubit"| "qbit"] ; "(" ; ")" -> QC.QBIT loc
    | "qdiscard" ; qvl = ne_qvars -> QC.QDISCARD loc qvl
    | "barrier" ; qvl = ne_qvars -> QC.QBARRIER loc qvl
    | "measure" ; qvl = ne_qvars -> QC.QMEASURE loc qvl
    | "reset" ; qvl = ne_qvars -> QC.QRESET loc qvl
    | g = qgate ; pl = params ; (qvl,cvl) = qvars_cvars ->
       QC.QGATEAPP loc g pl qvl cvl
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
      "(" ; qvl = LIST0 qvar SEP "," ; "/" ; cvl = LIST1 cvar SEP "," ; ")" -> (qvl, cvl)
    | "(" ; qvl = LIST0 qvar SEP "," ; ")" -> (qvl, [])
  ] ]
  ;

  params: [ [
      "(" ; l = LIST0 param SEP "," ; ")" -> l
    | -> []
  ] ] ;

  param: [
    "add" LEFTA [
      e1 = SELF ; "+" ; e2 = SELF -> PE.BINOP loc PE.ADD e1 e2
    | e1 = SELF ; "-" ; e2 = SELF -> PE.BINOP loc PE.SUB e1 e2
    ]
  | "mul" LEFTA [
      e1 = SELF ; "*" ; e2 = SELF -> PE.BINOP loc PE.MUL e1 e2
    | e1 = SELF ; "/" ; e2 = SELF -> PE.BINOP loc PE.DIV e1 e2
    ]
  | "uminus" LEFTA [ "-" ; e1 = SELF -> PE.UNOP loc PE.UMINUS e1 ]
  | "pow" RIGHTA [
      e1 = SELF ; "**" ; e2 = SELF -> PE.BINOP loc PE.POW e1 e2
    ]
  | "simple" [
      id = paramvar -> PE.ID loc id
    | c = paramconst -> PE.CONST loc c
    | "(" ; p = SELF ; ")" -> p
    | uf = ufun ; "(" ; p = param ; ")" -> uf p
    ]
  ]
  ;

  ufun: [ [
      "sin" -> (fun x ->  PE.UFUN loc PE.SIN x)
    | "cos" -> (fun x -> PE.UFUN loc PE.COS x)
    | "tan" -> (fun  x -> PE.UFUN loc PE.TAN x)
    | "exp" -> (fun x -> PE.UFUN loc PE.EXP x)
    | "ln" ->  (fun x -> PE.UFUN loc PE.LN x)
    | "sqrt" -> (fun x -> PE.UFUN loc PE.SQRT x)
    ] ]
  ;

  paramconst: [ [
      f = FLOAT -> PC.REAL (RealNumeral.mk f)
    | n = INT -> PC.NNINT (int_of_string n)
    | "pi" -> PC.PI
    ] ]
  ;

  qbinding: [ [
      (qvl,cvl) = paren_qvars_cvars ; "=" ; qc = qcirc -> (qvl, cvl, qc)
    | qv = qvar ; "=" ; qc = qcirc -> ([qv], [], qc)
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
  qgatename: [ [ x = ident -> QG loc x ] ] ;

END;

value read_qcircuit s =
  s |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Stream.of_string
  |> with_input_file s (Grammar.Entry.parse top)
;
