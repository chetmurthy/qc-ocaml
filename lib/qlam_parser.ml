open Qc_misc ;
open Qlam_syntax ;

value g = Grammar.gcreate (Plexer.gmake ());
value qcirc = Grammar.Entry.create g "qcirc";
value qgate = Grammar.Entry.create g "qgate";

EXTEND
  GLOBAL: qcirc qgate ;
  qgate: [ [
      "qbit" ; "(" ; ")" -> QC.QBIT
    | "qdiscard" ; qvl = LIST1 qvar -> QC.QDISCARD qvl
    | "barrier" ; qvl = LIST1 qvar -> QC.QBARRIER qvl
    | "measure" ; qvl = LIST1 qvar -> QC.QMEASURE qvl
    | "reset" ; qvl = LIST1 qvar -> QC.QRESET qvl
    | gname = qgatename -> QC.QGATE gname
    | "gatefun" ; "[" ; "(" ; pvl = LIST1 paramvar ; ")" ; "(" ; qvl = LIST1 qvar ; "/" ; cvl = LIST1 cvar ; ")" ;
      qc = qcirc ; "]" -> QC.QGATELAM pvl qvl cvl qc
  ] ]
  ;

  qcirc: [ [
      "let" ; l = LIST1 qbinding ; "in" ; qc = qcirc -> QC.QLET l qc
    | "(" ; qvl = LIST1 qvar ; ")" -> QC.QWIRES qvl []
    | "(" ; qvl = LIST1 qvar ; "/" ; cvl = LIST1 cvar ; ")" -> QC.QWIRES qvl cvl
    | g = qgate ; pl = params ; "(" ; qvl = LIST1 qvar ; "/" ; cvl = LIST1 cvar ; ")" ->
       QC.QGATEAPP g pl qvl cvl
  ] ]
  ;

  params: [ [
      "(" ; l = LIST1 param ; ")" -> l
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
      qvl = LIST1 qvar ; "=" ; qc = qcirc -> (qvl, [], qc)
    | qvl = LIST1 qvar ; "/" ; cvl = LIST1 cvar ; "=" ; qc = qcirc -> (qvl, cvl, qc)
  ] ]
  ;

  ident: [ [
       id = LIDENT -> (id, 0)
  ] ]
  ;
  paramvar: [ [ (s,n) = ident -> PE.PV s n ] ] ;
  qvar: [ [ (s,n) = ident -> QV s n ] ] ;
  cvar: [ [ (s,n) = ident -> CV s n ] ] ;
  qgatename: [ [ (s,n) = ident -> QGATE s n ] ] ;
END;
