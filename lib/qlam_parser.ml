open Pa_ppx_utils ;
open Qc_misc ;
open Qlam_syntax ;

value include_path = ref [] ;
value add_include (s : string) = Std.push include_path s ;

value g = Grammar.gcreate (Plexer.gmake ());
value qcirc = Grammar.Entry.create g "qcirc";
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

value read_inc s =
  let s = find_file_from ~{path=include_path.val} s in
  s |> Fpath.v |> Bos.OS.File.read
  |> Rresult.R.get_ok |> Stream.of_string
  |> with_input_file s (Grammar.Entry.parse env)
;

EXTEND
  GLOBAL: qcirc qbinding env env_item top ;

  env: [ [
    l = LIST0 env_item -> l
  ] ]
  ;

  env_item: [ [
      "gate" ; gname = qgatename ; "(" ; pvl = paramvars ; ")" ; (qvl,cvl) = qvars_cvars ;
        "=" ; qc = qcirc ; ";" -> QEnv.QGATEDEF gname (pvl, qvl, cvl, qc)
              | "include" ; s = STRING ; ";" -> QEnv.QINCLUDE s (read_inc s)
  ] ]
  ;

  top: [ [
    e = env ; qc = qcirc -> (e, qc)
    ] ]
  ;

  qgate: [ [
      "qbit" ; "(" ; ")" -> QC.QBIT
    | "qdiscard" ; qvl = ne_qvars -> QC.QDISCARD qvl
    | "barrier" ; qvl = ne_qvars -> QC.QBARRIER qvl
    | "measure" ; qvl = ne_qvars -> QC.QMEASURE qvl
    | "reset" ; qvl = ne_qvars -> QC.QRESET qvl
    | gname = qgatename -> QC.QGATE gname
    | "gatefun" ; "[" ; "(" ; pvl = paramvars ; ")" ; (qvl,cvl) = qvars_cvars ;
      qc = qcirc ; "]" -> QC.QGATELAM (pvl, qvl, cvl, qc)
  ] ]
  ;

  qcirc: [ [
      "let" ; l = LIST1 qbinding ; "in" ; qc = qcirc -> QC.QLET l qc
    | (qvl,cvl) = paren_qvars_cvars -> QC.QWIRES qvl cvl
    | g = qgate ; pl = params ; (qvl,cvl) = qvars_cvars ->
       QC.QGATEAPP g pl qvl cvl
  ] ]
  ;

  paramvars: [ [ l = LIST0 paramvar SEP "," -> l ] ] ;
  qvars: [ [ l = LIST0 qvar SEP "," -> l ] ] ;
  ne_qvars: [ [ l = LIST1 qvar SEP "," -> l ] ] ;
  cvars: [ [ l = LIST0 cvar SEP "," -> l ] ] ;
  ne_cvars: [ [ l = LIST1 cvar SEP "," -> l ] ] ;
  qvars_cvars: [ [
      (qvl, cvl) = paren_qvars_cvars -> (qvl, cvl)
    | qv = qvar -> ([qv], [])
  ] ]
  ;
  paren_qvars_cvars: [ [
      "(" ; qvl = qvars ; "/" ; cvl = ne_cvars ; ")" -> (qvl, cvl)
    | "(" ; qvl = qvars ; ")" -> (qvl, [])
  ] ]
  ;

  params: [ [
      "(" ; l = LIST1 param SEP "," ; ")" -> l
    | -> []
  ] ] ;

  param: [
    "add" LEFTA [
      e1 = SELF ; "+" ; e2 = SELF -> PE.BINOP PE.ADD e1 e2
    | e1 = SELF ; "-" ; e2 = SELF -> PE.BINOP PE.SUB e1 e2
    ]
  | "mul" LEFTA [
      e1 = SELF ; "*" ; e2 = SELF -> PE.BINOP PE.MUL e1 e2
    | e1 = SELF ; "/" ; e2 = SELF -> PE.BINOP PE.DIV e1 e2
    ]
  | "pow" LEFTA [ "-" ; e1 = SELF -> PE.UNOP PE.UMINUS e1 ]
  | "simple" [ id = paramvar -> PE.ID id
    | c = paramconst -> PE.CONST c
    | "(" ; p = SELF ; ")" -> p
    | uf = ufun ; "(" ; p = param ; ")" -> uf p
    ]
  ]
  ;

  ufun: [ [
      "sin" -> (fun x ->  PE.UFUN PE.SIN x)
    | "cos" -> (fun x -> PE.UFUN PE.COS x)
    | "tan" -> (fun  x -> PE.UFUN PE.TAN x)
    | "exp" -> (fun x -> PE.UFUN PE.EXP x)
    | "ln" ->  (fun x -> PE.UFUN PE.LN x)
    | "sqrt" -> (fun x -> PE.UFUN PE.SQRT x)
    ] ]
  ;

  paramconst: [ [
      f = FLOAT -> PC.REAL (RealNumeral.mk f)
    | n = INT -> PC.NNINT (int_of_string n)
    | "pi" -> PC.PI
    ] ]
  ;

  qbinding: [ [
      (qvl,cvl) = qvars_cvars ; "=" ; qc = qcirc -> (qvl, cvl, qc)
  ] ]
  ;

  ident: [ [
       id = LIDENT -> ID.mk id
     | id = UIDENT -> ID.mk id
  ] ]
  ;
  paramvar: [ [ (s,n) = ident -> PE.PV s n ] ] ;
  qvar: [ [ (s,n) = ident -> QV s n ] ] ;
  cvar: [ [ (s,n) = ident -> CV s n ] ] ;
  qgatename: [ [ (s,n) = ident -> QGATE s n ] ] ;

END;
