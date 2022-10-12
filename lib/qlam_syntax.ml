open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Pa_ppx_base.Pp_MLast ;
open Qc_misc ;

value ident pps x = Fmt.(pf pps "%s" (ID.unmk x)) ;

type loc = Ploc.t ;
value loc_to_yojson (_ : loc) = `String "<loc>" ;
value equal_loc _ _ = True ;
value compare_loc _ _ = 0 ;

module type VARSIG = sig
  type t = 'a[@@deriving (eq, ord, show);];
  value toID : t -> ID.t ;
  value ofID : ID.t -> t ;
  value pp_hum : Fmt.t t ;
end ;
module type FVSIG = sig
  module M : VARSIG ;
  type t = 'a [@@deriving (show);] ;
  value mt : t ;
  value add : t -> M.t -> t ;
  value mem : t -> M.t -> bool ;
  value subtract : t -> t -> t ;
  value ofList : list M.t -> t ;
  value toList : t -> list M.t ;
  value union : t -> t -> t ;
  value fresh : t -> M.t -> M.t ;
  value equal : t -> t -> bool ;
  value pp_hum : Fmt.t t ;
end ;

module type MAPSIG = sig
  module M : VARSIG ;
  include Map.S with type key = M.t ;
  value pp_hum : Fmt.t 'a -> Fmt.t (t 'a) ;
end ;

module VarMap(M : VARSIG) : (MAPSIG with module M = M) = struct
  module M = M ;
  module IT = Map.Make(M) ;
  include IT ;
  value pp_hum ppval pps l =
    Fmt.(pf pps "%a" (list ~{sep=const string "; "} (parens (pair ~{sep=const string ", "} M.pp_hum ppval))) (bindings l))
  ;
end ;

module FreeVarSet(M : VARSIG) : (FVSIG with module M = M) = struct
  module M = M ;
  type t = list M.t[@@deriving (show);] ;
  value mt = [] ;
  value mem s x = s |> List.exists (fun y -> M.equal x y) ;
  value add s x = if mem s x then s else [x::s] ;
  value subtract l1 l2 =
    l1 |> List.filter (fun x -> not (mem l2 x)) ;
  value ofList l = l ;
  value toList l = l ;
  value union l1 l2 = List.fold_left add l1 l2 ;
  value fresh l x =
    let (s,_) = M.toID x in
    let n = List.fold_left (fun acc v ->
      let (s', m) = M.toID v in
      if s <> s' then acc else max acc m)
          Int.min_int l in
    if n = Int.min_int then
      M.ofID (s,-1)
    else M.ofID (s, n+1) ;

  value equal l1 l2 =
    l1 |> List.for_all (mem l2)
    && l2 |> List.for_all (mem l1)  ;

  value pp_hum pps l = Fmt.(pf pps "%a" (brackets (list ~{sep=const string "; "} M.pp_hum)) l) ;
end ;

module SYN = struct
module PC = struct

type t = [
    REAL of RealNumeral.t
  | NNINT of int
  | PI
  ][@@deriving (to_yojson, show, eq, ord);]
;
value pp pps = fun [
    REAL s -> Fmt.(pf pps "%s" (RealNumeral.unmk s))
  | NNINT n -> Fmt.(pf pps "%d" n)
  | PI -> Fmt.(pf pps "pi")
    ]
;
end ;

type pvar_t = [ PV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module PV = struct
  type t = pvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ PV _ x -> x ] ;
  value ofID x = PV Ploc.dummy x ;
  value to_loc = fun [ PV loc _ -> loc ] ;

  value pvar pps = fun [ (PV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" pvar x) ;
end ;

module PVMap = VarMap(PV) ;
module PVFVS = FreeVarSet(PV) ;


type qvar_t = [ QV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QV = struct
  type t = qvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ QV _ x -> x ] ;
  value ofID x = QV Ploc.dummy x ;
  value to_loc = fun [ QV loc _ -> loc ] ;
  value qvar pps = fun [ (QV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" qvar x) ;
end ;
module QVMap = VarMap(QV) ;
module QVFVS = FreeVarSet(QV) ;

type cvar_t = [ CV of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module CV = struct
  type t = cvar_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ CV _ x -> x ] ;
  value ofID x = CV Ploc.dummy x ;
  value to_loc = fun [ CV loc _ -> loc ] ;
  value cvar pps = fun [ (CV _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" cvar x) ;
end ;
module CVMap = VarMap(CV) ;
module CVFVS = FreeVarSet(CV) ;

type qgn_t = [ QG of loc and ID.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QG = struct
  type t = qgn_t[@@deriving (to_yojson, show, eq, ord);];
  value toID = fun [ QG _ x -> x ] ;
  value ofID x = QG Ploc.dummy x ;
  value to_loc = fun [ QG loc _ -> loc ] ;
  value qgn pps = fun [ (QG _ id) -> ident pps id ] ;
  value pp_hum pps x = Fmt.(pf pps "%a" qgn x) ;
end ;
module QGMap = VarMap(QG) ;
module QGFVS = FreeVarSet(QG) ;

module PE = struct

type binop_t = [ ADD | SUB | MUL | DIV | POW ][@@deriving (to_yojson, show, eq, ord);] ;
type unop_t = [ UMINUS ][@@deriving (to_yojson, show, eq, ord);] ;
type ufun_t = [ SIN | COS | TAN | EXP | LN | SQRT ][@@deriving (to_yojson, show, eq, ord);] ;

value string_of_ufun = fun [
    SIN -> "sin"
  | COS -> "cos"
  | TAN -> "tan"
  | EXP -> "exp"
  | LN -> "ln"
  | SQRT -> "sqrt"
] ;

type t = [
  ID of loc and pvar_t
| CONST of loc and PC.t
| BINOP of loc and binop_t and t and t
| UNOP of loc and unop_t and t
| UFUN of loc and ufun_t and t
  ][@@deriving (to_yojson, show, eq, ord);]
;
value rec pp0 pps = fun [
  ID _ pv -> Fmt.(pf pps "%a" PV.pp_hum pv)
| CONST _ pc ->  PC.pp pps pc
| UFUN _ fsym pe ->
   Fmt.(pf pps "%s(%a)" (string_of_ufun fsym) pp pe)
| x -> Fmt.(pf pps "(%a)" pp x)
]

and pp1 pps = fun [
    UNOP _ UMINUS pe ->
    Fmt.(pf pps "- %a" pp1 pe)
  | x -> pp0 pps x
]

and pp2 pps = fun [
  BINOP _ POW pe1 pe2 ->
   Fmt.(pf pps "%a ** %a" pp1 pe1 pp2 pe2)
| x -> pp1 pps x
    ]

and pp3 pps = fun [
  BINOP _ MUL pe1 pe2 ->
   Fmt.(pf pps "%a * %a" pp3 pe1 pp2 pe2)
| BINOP _ DIV pe1 pe2 ->
   Fmt.(pf pps "%a / %a" pp3 pe1 pp2 pe2)
| x -> pp2 pps x
    ]

and pp4 pps = fun [
  BINOP _ ADD pe1 pe2 ->
   Fmt.(pf pps "%a + %a" pp4 pe1 pp3 pe2)
| BINOP _ SUB pe1 pe2 ->
   Fmt.(pf pps "%a - %a" pp4 pe1 pp3 pe2)
| x -> pp3 pps x
    ]
and pp pps x = pp4 pps x
;
end
;

module QC = struct
open Fmt ;

type qgatelam_t = (qgateargs_t * t)
and qgateargs_t = (list pvar_t * list qvar_t * list cvar_t)

and t = [
  QLET of loc and list qbinding_t and t
| QWIRES of loc and list qvar_t and list cvar_t
| QGATEAPP of loc and qgn_t and list PE.t and list qvar_t and list cvar_t
| QBARRIER of loc and list qvar_t
| QBIT of loc | QDISCARD of loc and list qvar_t
| QMEASURE of loc and list qvar_t
| QRESET of loc and list qvar_t
  ]
and qbinding_t =
  (loc * list qvar_t * list cvar_t * t)
[@@deriving (to_yojson, show, eq, ord);] ;

value loc_of_qcirc = fun [
  QLET loc _ _ -> loc
| QWIRES loc _ _ -> loc
| QGATEAPP loc _ _ _ _ -> loc
| QBARRIER loc _ -> loc
| QBIT loc -> loc
| QDISCARD loc _ -> loc
| QMEASURE loc _ -> loc
| QRESET loc _ -> loc
] ;

value and_sep pps () = Fmt.(pf pps "@ and ") ;

value paren_qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "(%a)" (list ~{sep=(const string ", ")} QV.pp_hum) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "(%a : %a)" (list ~{sep=(const string ", ")} QV.pp_hum) qvl  (list ~{sep=(const string ", ")} CV.pp_hum) cvl)
] ;

value qvars_cvars pps = fun [
    (qvl, []) ->
     Fmt.(pf pps "%a" (list ~{sep=const string " "} QV.pp_hum) qvl)
  | (qvl, cvl) ->
     Fmt.(pf pps "%a : %a" (list ~{sep=sp} QV.pp_hum) qvl  (list ~{sep=const string " "} CV.pp_hum) cvl)
] ;

value comm_nl pps = fun [
  "" -> Fmt.(pf pps "")
| s -> Fmt.(pf pps "%s@." (cleanws ~{lf=True} s))
] ;

value rec qcirc pps = fun [
    QLET loc bl qc ->
    let comm = Ploc.comment loc in
     Fmt.(pf pps "@[<v>%alet @[%a@] in@ %a@]" comm_nl comm (list ~{sep=and_sep} binding) bl qcirc qc)
  | QWIRES _ qvl cvl -> paren_qvars_cvars pps (qvl, cvl)
  | QGATEAPP _ qg [] qvl cvl ->
     Fmt.(pf  pps "%a %a" QG.pp_hum qg qvars_cvars (qvl, cvl))
  | QGATEAPP _ qg pel qvl cvl ->
     Fmt.(pf  pps "%a (%a) %a" QG.pp_hum qg (list ~{sep=(const string ", ")} PE.pp) pel qvars_cvars (qvl, cvl))
  | QBARRIER _ qvl -> Fmt.(pf pps "barrier %a" qvars_cvars (qvl, []))
  | QBIT _ -> Fmt.(pf pps "qubit()")
  | QDISCARD _ qvl -> Fmt.(pf pps "qdiscard %a" qvars_cvars (qvl, []))
  | QMEASURE _ qvl -> Fmt.(pf pps "measure %a" qvars_cvars (qvl, []))
  | QRESET _ qvl -> Fmt.(pf pps "reset %a" qvars_cvars (qvl, []))
]

and binding pps = fun [
    (_, [qv],  [], qc) ->
    Fmt.(pf pps "%a = %a" QV.pp_hum qv qcirc qc)
  | (_, qvl,  cvl, qc) ->
    Fmt.(pf pps "%a = %a" paren_qvars_cvars (qvl,cvl) qcirc qc)
] ;
end ;

module QEnv = struct

type gate_item = [
  DEF of QG.t and QC.qgatelam_t
| OPAQUE of QG.t and QC.qgateargs_t
  ][@@deriving (to_yojson, show, eq, ord);] ;

type item = [
  QGATE of loc and gate_item
| QINCLUDE of loc and file_type_t and string and t
]
and t = list item
[@@deriving (to_yojson, show, eq, ord);] ;

value gate_item pps = fun [
    DEF gname ((pvl, qvl, cvl), qc) ->
    Fmt.(pf pps "@[<v 2>gate %a (%a) %a =@ %a@]@,;"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           QC.qvars_cvars (qvl, cvl)
           QC.qcirc qc)
  | OPAQUE gname (pvl, qvl, cvl) ->
    Fmt.(pf pps "@[gate %a (%a) %a ;@]"
           QG.pp_hum gname
           (list ~{sep=(const string ", ")} PV.pp_hum) pvl
           QC.qvars_cvars (qvl, cvl))
] ;

value item pps = fun [
    QGATE _ gitem -> gate_item pps gitem
  | QINCLUDE _ _ s _ ->
     Fmt.(pf pps "include %a ;" (quote string) s)
] ;

value newline_sep pps () = Fmt.(pf pps "@.") ;

value pp pps l =
  Fmt.(pf pps "@[<v>%a@]" (list ~{sep=newline_sep} item) l) ;

end ;

type t = (QEnv.t * QC.t)[@@deriving (to_yojson, show, eq, ord);] ;

value pp pps (env, qc) =
  Fmt.(pf pps "%a@.%a%!" QEnv.pp env QC.qcirc qc) ;
end ;

module Ops = struct
open SYN ;
open SYN.QC ;

value pe_freevars pe =
  let rec fvrec = fun [
        SYN.PE.ID _ pv ->  (SYN.PVFVS.ofList [pv])
      | CONST _ _ -> SYN.PVFVS.mt
      | BINOP _ _ pe1 pe2 -> SYN.PVFVS.union (fvrec pe1) (fvrec pe2)
      | UNOP _ _ pe -> fvrec pe
      | UFUN _ _ pe -> fvrec pe
      ] in
  fvrec pe
;

value circuit_freevars qc =
  let rec fvrec = fun [
        SYN.QC.QLET loc bl qc ->
        let (pvs, qvs, cvs) = fvrec qc in
        let qvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) -> qvl) |> SYN.QVFVS.ofList in
        let cvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) -> cvl) |> SYN.CVFVS.ofList in
        let qvs = SYN.QVFVS.subtract qvs qvars in
        let cvs = SYN.CVFVS.subtract cvs cvars in
        List.fold_left (fun (pvs, qvs,  cvs) (_, _, _, qc) ->
            let (pvs', qvs', cvs') = fvrec qc in
            (SYN.PVFVS.union pvs pvs', SYN.QVFVS.union qvs qvs', SYN.CVFVS.union cvs cvs'))
          (SYN.PVFVS.mt,  qvs, cvs) bl
      | QWIRES _ qvl cvl -> (SYN.PVFVS.mt,  SYN.QVFVS.ofList qvl, SYN.CVFVS.ofList cvl)
      | QGATEAPP _ _ pel qvl  cvl ->
         let pvl = List.fold_left (fun pvl pe -> SYN.PVFVS.union pvl (pe_freevars pe)) SYN.PVFVS.mt pel in
         (pvl, SYN.QVFVS.ofList qvl, SYN.CVFVS.ofList cvl)
      | QBARRIER _ qvl  -> (SYN.PVFVS.mt,  SYN.QVFVS.ofList qvl, SYN.CVFVS.mt)
      | QBIT _ -> (SYN.PVFVS.mt, SYN.QVFVS.mt, SYN.CVFVS.mt)
      | QDISCARD _ qvl -> (SYN.PVFVS.mt,  SYN.QVFVS.ofList qvl,  SYN.CVFVS.mt)
      | QMEASURE _ qvl -> (SYN.PVFVS.mt, SYN.QVFVS.ofList qvl, SYN.CVFVS.mt)
      | QRESET _ qvl -> (SYN.PVFVS.mt, SYN.QVFVS.ofList qvl, SYN.CVFVS.mt)
  ] in
  fvrec qc
;

(** [lower_circuit qc] will return an alpha-equal (in the sense of lambda-calculus,
    to be sure) circuit where ids of the form (s, n) have been renamed to the least
    value of "n" that is greater than all other ids of the form (s, m) for that same
    string "s".

    Hence,  it "lowers" bound-variables.

    To choose a new name for a bound-variable:

    (1) pick a fresh name that is lowest-numbered and does not conflict with any known free-variables
    (2) add that name to the known free-variables
    (3) if the fresh name is different from original name, add (orig,fresh) to renaming-map.

 *)

value lower_circuit qc =
  let (fv_pvs, fv_qvs, fv_cvs) = circuit_freevars qc in
  let rec lowrec (fv_qvs, fv_cvs, ren_qv, ren_cv) qc =
    let rename_qv qv =
      match QVMap.find qv ren_qv with [
          exception Not_found -> qv
        | x -> x
        ] in
    let rename_cv cv =
      match CVMap.find cv ren_cv with [
          exception Not_found -> cv
        | x -> x
        ] in
    match qc with [
      QLET loc bl qc ->
      let bl = bl |> List.map (fun (loc, qvl, cvl, qc) -> (loc, qvl, cvl, lowrec (fv_qvs, fv_cvs, ren_qv, ren_cv) qc)) in
      
      let rebind_qv (rev_qvs, (fv_qvs, ren_qv)) qv =
        let fresh_qv = QVFVS.fresh fv_qvs qv in
        let fv_qvs = QVFVS.add fv_qvs fresh_qv in
        let ren_qv = 
          if equal_qvar_t qv fresh_qv then ren_qv
          else QVMap.add qv fresh_qv ren_qv in
        ([fresh_qv :: rev_qvs], (fv_qvs, ren_qv)) in
      
      let rebind_cv (rev_cvs, (fv_cvs, ren_cv)) cv =
        let fresh_cv = CVFVS.fresh fv_cvs cv in
        let fv_cvs = CVFVS.add fv_cvs fresh_cv in
        let ren_cv = 
          if equal_cvar_t cv fresh_cv then ren_cv
          else CVMap.add cv fresh_cv ren_cv in
        ([fresh_cv :: rev_cvs], (fv_cvs, ren_cv)) in
      
      let (rev_bl,(fv_qvs, fv_cvs, ren_qv, ren_cv)) =
        List.fold_left (fun (rev_bl, (fv_qvs, fv_cvs, ren_qv, ren_cv)) (loc, qvl, cvl, qc) ->
            let (rev_qvl, (fv_qvs, ren_qv)) =
              List.fold_left rebind_qv ([], (fv_qvs, ren_qv)) qvl in
            let (rev_cvl, (fv_cvs, ren_cv)) =
              List.fold_left rebind_cv ([], (fv_cvs, ren_cv)) cvl in
            let qvl = List.rev rev_qvl in
            let cvl = List.rev rev_cvl in
            ([ (loc, qvl, cvl, qc) :: rev_bl ], (fv_qvs, fv_cvs, ren_qv, ren_cv)))
          ([], (fv_qvs, fv_cvs, ren_qv, ren_cv)) bl in
      let bl = List.rev rev_bl in
      let qc = lowrec (fv_qvs, fv_cvs, ren_qv, ren_cv) qc in
      QLET loc bl qc

    | QWIRES loc qvl cvl ->
       QWIRES loc (List.map rename_qv qvl) (List.map rename_cv cvl)

    | QGATEAPP loc g pel qvl cvl -> QGATEAPP loc g pel (List.map rename_qv qvl) (List.map rename_cv cvl)
    | QBARRIER loc qvl -> QBARRIER loc (List.map rename_qv qvl)
    | QBIT loc -> QBIT loc
    | QDISCARD loc qvl -> QDISCARD loc (List.map rename_qv qvl)
    | QMEASURE loc qvl -> QMEASURE loc (List.map rename_qv qvl)
    | QRESET loc qvl -> QRESET loc (List.map rename_qv qvl)
    ] in
  lowrec (fv_qvs, fv_cvs, QVMap.empty, CVMap.empty) qc
;

module AlphaEq = struct
value add_qvs m l1 l2 =
  List.fold_left2 (fun m v1 v2 -> QVMap.add v1 v2 m) m l1 l2
;
value add_cvs m l1 l2 =
  List.fold_left2 (fun m v1 v2 -> CVMap.add v1 v2 m) m l1 l2
;
value check_qv_corr loc lr rl v1 v2 =
  try
    equal_qvar_t (QVMap.find v1 lr) v2 && equal_qvar_t (QVMap.find v2 rl) v1
  with Not_found -> Fmt.(raise_failwithf loc "alpha_equal(check_qv_corr %a %a): internal error" QV.pp_hum v1 QV.pp_hum v2)
;
value check_cv_corr loc lr rl v1 v2 =
  try
    equal_cvar_t (CVMap.find v1 lr) v2 && equal_cvar_t (CVMap.find v2 rl) v1
  with Not_found -> Fmt.(raise_failwithf loc "alpha_equal(check_cv_corr %a %a): internal error" CV.pp_hum v1 CV.pp_hum v2)
;

value circuit qc1 qc2 =
  let rec alpharec (qv_lr, qv_rl, cv_lr, cv_rl) = fun [
    (QLET _ bl1 qc1, QLET _ bl2 qc2) ->
    (List.length bl1 = List.length bl2)
    && (List.for_all2 (fun (_, qvl1, cvl1, qc1) (_, qvl2, cvl2, qc2) ->
            (List.length qvl1 = List.length qvl2)
            && (List.length cvl1 = List.length cvl2)
            && alpharec (qv_lr, qv_rl, cv_lr, cv_rl) (qc1, qc2))
          bl1 bl2)
    && (let (qv_lr, qv_rl, cv_lr, cv_rl) =
          List.fold_left2 (fun (qv_lr, qv_rl, cv_lr, cv_rl)
                               (_, qvl1, cvl1, _) (_, qvl2, cvl2, _) ->
              let qv_lr = add_qvs qv_lr qvl1 qvl2 in
              let qv_rl = add_qvs qv_rl qvl2 qvl1 in
              let cv_lr = add_cvs cv_lr cvl1 cvl2 in
              let cv_rl = add_cvs cv_rl cvl2 cvl1 in
              (qv_lr, qv_rl, cv_lr, cv_rl))
            (qv_lr, qv_rl, cv_lr, cv_rl) bl1 bl2 in
        alpharec (qv_lr, qv_rl, cv_lr, cv_rl) (qc1, qc2))

  | (QWIRES loc qvl1 cvl1, QWIRES _ qvl2 cvl2) ->
     (List.length qvl1 = List.length qvl2)
    && (List.length cvl1 = List.length cvl2)
    && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    && (List.for_all2 (fun cv1 cv2 -> check_cv_corr loc cv_lr cv_rl cv1 cv2) cvl1 cvl2)

  | (QGATEAPP loc _ _ qvl1 cvl1, QGATEAPP _ _ _ qvl2 cvl2) ->
     (List.length qvl1 = List.length qvl2)
     && (List.length cvl1 = List.length cvl2)
    && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    && (List.for_all2 (fun cv1 cv2 -> check_cv_corr loc cv_lr cv_rl cv1 cv2) cvl1 cvl2)

  | (QBARRIER loc qvl1, QBARRIER _ qvl2) ->
     (List.length qvl1 = List.length qvl2)
    && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    
  | (QBIT _, QBIT _) -> True
                      
  | (QDISCARD loc qvl1, QDISCARD _ qvl2) ->
     (List.length qvl1 = List.length qvl2)
     && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    
  | (QMEASURE loc qvl1, QMEASURE _ qvl2) ->
     (List.length qvl1 = List.length qvl2)
     && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    
  | (QRESET loc qvl1, QRESET _ qvl2) ->
     (List.length qvl1 = List.length qvl2)
     && (List.for_all2 (fun qv1 qv2 -> check_qv_corr loc qv_lr qv_rl qv1 qv2) qvl1 qvl2)
    
  | _ -> False
  ] in
  let (_, qvfvs1, cvfvs1) = circuit_freevars qc1 in
  let (_, qvfvs2, cvfvs2) = circuit_freevars qc2 in
  QVFVS.equal qvfvs1 qvfvs2
  && CVFVS.equal cvfvs1 cvfvs2
  && (let qvmap = List.fold_left (fun m v -> QVMap.add v v m) QVMap.empty (QVFVS.toList qvfvs1) in
      let cvmap = List.fold_left (fun m v -> CVMap.add v v m) CVMap.empty (CVFVS.toList cvfvs1) in
      alpharec (qvmap, qvmap, cvmap, cvmap) (qc1, qc2))
;
end ;

module Fresh = struct

type t = {
    it : ref int
  } ;
value mk ?{base=0} () = { it = ref base } ;
value next { it=it } = do {
  let n = it.val in
  it.val := n+1 ;
  n
} ;

value qcircuit ~{counter} ~{qvmap} ~{cvmap} qc =
  let fresh_qvar = fun [ (QV loc (s,_)) -> QV loc (s, next counter) ] in
  let fresh_cvar = fun [ (CV loc (s,_)) -> CV loc (s, next counter) ] in
  let rec freshrec (qvmap, cvmap) qc =
    let map_qvar qv =
      match QVMap.find qv qvmap with [
          exception Not_found -> qv
        | x -> x
        ] in
    let map_cvar cv =
      match CVMap.find cv cvmap with [
          exception Not_found -> cv
        | x -> x
        ] in
    match qc with [
        QLET loc bl qc ->
        let bl =
          bl
          |> List.map (fun (loc, qvl, cvl, qc) ->
                 (loc, qvl, cvl, freshrec (qvmap, cvmap) qc)) in
        let (bl, (qvmap, cvmap)) =
          List.fold_right (fun (loc, qvl, cvl, qc) (bl, (qvmap, cvmap)) ->
              let (qvl, qvmap) =
                List.fold_right (fun qv (qvl, qvmap) ->
                    let qv' = fresh_qvar qv in
                    ([qv'::qvl], QVMap.add qv qv' qvmap)) qvl ([], qvmap) in
              let (cvl, cvmap) =
                List.fold_right (fun cv (cvl, cvmap) ->
                    let cv' = fresh_cvar cv in
                    ([cv'::cvl], CVMap.add cv cv' cvmap)) cvl ([], cvmap) in
              ([(loc, qvl, cvl, qc) :: bl], (qvmap, cvmap))
            ) bl ([], (qvmap, cvmap)) in
        let qc = freshrec (qvmap, cvmap) qc in
        QLET loc bl qc

    | QWIRES loc qvl cvl -> QWIRES loc (List.map map_qvar qvl) (List.map map_cvar cvl)

    | QGATEAPP loc gn pel qvl cvl -> QGATEAPP loc gn pel (List.map map_qvar qvl) (List.map map_cvar cvl)

    | QBARRIER loc qvl -> QBARRIER loc (List.map map_qvar qvl)
    | (QBIT _) as qc -> qc
    | QDISCARD loc qvl -> QDISCARD loc (List.map map_qvar qvl)
    | QMEASURE loc qvl -> QMEASURE loc (List.map map_qvar qvl)
    | QRESET loc qvl -> QRESET loc (List.map map_qvar qvl)
    ] in
  freshrec (qvmap, cvmap) qc
;

value qgatelam ~{counter} ((pvl, qvl, cvl), qc) =
  let fresh_qvar = fun [ (QV loc (s,_)) -> QV loc (s, next counter) ] in
  let fresh_cvar = fun [ (CV loc (s,_)) -> CV loc (s, next counter) ] in
  let qvmap = QVMap.empty in
  let cvmap = CVMap.empty in
  let (qvl, qvmap) =
    List.fold_right (fun qv (qvl, qvmap) ->
        let qv' = fresh_qvar qv in
        ([qv'::qvl], QVMap.add qv qv' qvmap)) qvl ([], qvmap) in
  let (cvl, cvmap) =
    List.fold_right (fun cv (cvl, cvmap) ->
        let cv' = fresh_cvar cv in
        ([cv'::cvl], CVMap.add cv cv' cvmap)) cvl ([], cvmap) in
  ((pvl, qvl, cvl), qcircuit ~{qvmap=qvmap} ~{cvmap=cvmap} ~{counter=counter} qc)
;

end ;

module BetaReduce = struct

value subst_pe pvmap pe =
  let open PE in
  let rec substrec = fun [
        (ID loc pv) as pe ->
        match PVMap.find pv pvmap with [
            exception Not_found -> pe
          | x -> x
          ]
  | (CONST _ _) as pe -> pe
  | BINOP loc bop pe1 pe2 -> BINOP loc bop (substrec pe1) (substrec pe2)
  | UNOP loc uop pe -> UNOP loc uop (substrec pe)
  | UFUN loc fop pe -> UFUN loc fop (substrec pe)
  ] in
  substrec pe
;

value subst (pvmap, qvmap, cvmap, qvfvs, cvfvs) qc =
  let subst_qvar qv =
    match QVMap.find qv qvmap with [
        exception Not_found -> qv
      | x -> x
      ] in
  let subst_cvar cv =
    match CVMap.find cv cvmap with [
        exception Not_found -> cv
      | x -> x
      ] in
  let subst_pe pe = subst_pe pvmap pe in
  let rec substrec = fun [
    QLET loc bl qc -> do {
      bl |> List.iter (fun (loc, qvl, cvl, _) ->
                if qvl |> List.exists (QVFVS.mem qvfvs) then
                  Fmt.(raise_failwithf loc "BetaReduce.subst: internal error: binding qvars %a clash with subst %a"
                         (list ~{sep=const string " "} QV.pp_hum) qvl
                         QVFVS.pp_hum qvfvs
                  )
                else if cvl |> List.exists (CVFVS.mem cvfvs) then
                  Fmt.(raise_failwithf loc "BetaReduce.subst: internal error: binding cvars %a clash with subst %a"
                         (list ~{sep=const string " "} CV.pp_hum) cvl
                         CVFVS.pp_hum cvfvs
                  )
                else ());
      let bl = bl |> List.map (fun (loc, qvl,cvl, qc) -> (loc, qvl, cvl, substrec qc)) in
      QLET loc bl (substrec qc)
    }

  | QWIRES loc qvl cvl -> QWIRES loc (List.map subst_qvar qvl) (List.map subst_cvar cvl)
  | QGATEAPP loc gn pel qvl cvl -> QGATEAPP loc gn (List.map subst_pe pel) (List.map subst_qvar qvl) (List.map subst_cvar cvl)
  | QBARRIER loc qvl -> QBARRIER loc (List.map subst_qvar qvl)
  | (QBIT _) as qc -> qc
  | QDISCARD loc qvl -> QDISCARD loc (List.map subst_qvar qvl)
  | QMEASURE loc qvl -> QMEASURE loc (List.map subst_qvar qvl)
  | QRESET loc qvl -> QRESET loc (List.map subst_qvar qvl)
  ] in
  substrec qc
;

value qcircuit genv = fun [
  QGATEAPP loc gn pel qel cel ->
  let ((pvl,qvl, cvl), qc) = 
    match QGMap.find gn genv with [
      exception Not_found -> Fmt.(raise_failwithf loc "BetaReduce.qcircuit: gate %a not found" QG.pp_hum gn)
    | x -> x ] in
  if List.length pel <> List.length pvl then
    Fmt.(raise_failwithf loc "BetaReduce.qcircuit: param-vars/actuals differ in length")
  else if List.length qel <> List.length qvl then
    Fmt.(raise_failwithf loc "BetaReduce.qcircuit: q-vars/actuals differ in length")
  else if List.length cel <> List.length cvl then
    Fmt.(raise_failwithf loc "BetaReduce.qcircuit: c-vars/actuals differ in length")
  else 
  let pvmap = List.fold_left2 (fun pvmap pv pe -> PVMap.add pv pe pvmap) PVMap.empty pvl pel in
  let qvmap = List.fold_left2 (fun qvmap qv qe -> QVMap.add qv qe qvmap) QVMap.empty qvl qel in
  let cvmap = List.fold_left2 (fun cvmap cv ce -> CVMap.add cv ce cvmap) CVMap.empty cvl cel in
  subst (pvmap, qvmap, cvmap, QVFVS.ofList qel, CVFVS.ofList cel) qc

| x -> Fmt.(raise_failwithf (loc_of_qcirc x) "BetaReduce: can only be applied to gate-application, not %a" qcirc x)
] ;

end ;

(** TODO

(1) alpha-equality

(2) raise_fresh

(3) beta-reduce

(4) unroll

(5) A-normalize


 *)


end ;

module TYCHK = struct

module Env = struct

type qvar_binding_t = {
    used : mutable bool
  ; loc : Ploc.t
  ; it : option SYN.QC.qbinding_t
  } ;

type cvar_binding_t = option SYN.QC.qbinding_t ;
type pvar_binding_t = unit ;

type t = {
    gates : SYN.QGMap.t (SYN.QC.qgateargs_t * (int * int))
  ; qvars : SYN.QVMap.t qvar_binding_t
  ; cvars : SYN.CVMap.t cvar_binding_t
  ; pvars : SYN.PVMap.t pvar_binding_t
  }
;

value mk () = {
  gates = SYN.QGMap.empty
; qvars = SYN.QVMap.empty
; cvars = SYN.CVMap.empty
; pvars = SYN.PVMap.empty
} ;

value mk_for_gate env = { (mk()) with gates = env.gates } ;

value add_gate loc env (gid, glam) =
  if SYN.QGMap.mem gid env.gates then
    Fmt.(raise_failwithf loc "add_gate: gate %a already in env" SYN.QG.pp_hum gid)
  else
    { (env) with gates = SYN.QGMap.add gid glam env.gates }
;

value add_qvar loc env (id, qvb) =
  { (env) with qvars = SYN.QVMap.add id qvb env.qvars }
;

value add_cvar loc env (id, cv) =
  { (env) with cvars = SYN.CVMap.add id cv env.cvars }
;

value add_pvar loc env (id, pv) =
  { (env) with pvars = SYN.PVMap.add id pv env.pvars }
;

value has_gate loc env id = SYN.QGMap.mem id env.gates ;
value has_qvar loc env id = SYN.QVMap.mem id env.qvars ;
value has_cvar loc env id = SYN.CVMap.mem id env.cvars ;
value has_pvar loc env id = SYN.PVMap.mem id env.pvars ;

value find_gate loc env gid = match SYN.QGMap.find gid env.gates with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_gate: gate %a not found" SYN.QG.pp_hum gid)
] ;

value find_qvar loc env qid = match SYN.QVMap.find qid env.qvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_qvar: qvar %a not found" SYN.QV.pp_hum qid)
] ;

value find_cvar loc env cid = match SYN.CVMap.find cid env.cvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_cvar: cvar %a not found" SYN.CV.pp_hum cid)
] ;

value find_pvar loc env pid = match SYN.PVMap.find pid env.pvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_pvar: pvar %a not found" SYN.PV.pp_hum pid)
] ;

end ;

value qvar_find_mark_used loc env qv =
  match Env.find_qvar loc env qv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "circuit: undeclared qvar %a" SYN.QV.pp_hum qv)
    | x -> if x.used then
             Fmt.(raise_failwithf loc "circuit: qvar %a used more than once" SYN.QV.pp_hum qv)
           else x.used := True ]
;

value cvar_find loc env cv = 
  if not (Env.has_cvar loc env cv) then
    Fmt.(raise_failwithf loc "circuit: undeclared cvar %a" SYN.CV.pp_hum cv)
  else ()
;

value rec circuit env qc = match qc with [
  SYN.QC.QWIRES loc qvl cvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    cvl |> List.iter (cvar_find loc env) ;
    (List.length qvl, List.length cvl)
  }

| QBIT _ -> (1, 0)
| QDISCARD loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (0,0)
  }
| QBARRIER loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,0)
  }
| QRESET loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,0)
  }
| QMEASURE loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,List.length qvl)
  }
| QGATEAPP loc gn pal qal cal ->
   let ((pfl, qfl, cfl), ty) =
     match Env.find_gate loc env gn with [
         exception Not_found ->
           Fmt.(raise_failwithf loc "gate-application: gate %a not found" SYN.QG.pp_hum gn)
       | x -> x
       ] in
   if List.length pal <> List.length pfl then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with param-var length mismatch" SYN.QG.pp_hum gn)
   else if List.length qal <> List.length qal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with qvar length mismatch" SYN.QG.pp_hum gn)
   else if List.length cal <> List.length cal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with cvar length mismatch" SYN.QG.pp_hum gn)
   else do {
    qal |> List.iter (qvar_find_mark_used loc env) ;
    ty
  }
| QLET loc bl qc -> do {
    let qvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map SYN.QV.toID qvl)) in
    if not (Std.distinct qvars) then
      Fmt.(raise_failwithf loc "TYCHK.circuit: qvars in binding MUST be distinct")
    else () ;
    let cvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map SYN.CV.toID cvl)) in
    if not (Std.distinct cvars) then
      Fmt.(raise_failwithf loc "TYCHK.circuit: cvars in binding MUST be distinct")
    else () ;
    let bl =
      bl
      |>  List.map (fun ((loc, qvl,cvl,qc) as b) ->
              let (qlen, clen) = circuit env qc in
              if qlen <> List.length qvl then
                Fmt.(raise_failwithf loc "circuit: binding qvar length differs from circuit")
              else if clen <> List.length cvl then
                Fmt.(raise_failwithf loc "circuit: binding cvar length differs from circuit")
              else
                b) in
    let cv_bindings =
      bl |> List.concat_map (fun ((loc, qvl, cvl, qc) as b) ->
                cvl |> List.map (fun cv -> (cv, Some b))) in
    let qv_bindings =
      bl |> List.concat_map (fun ((loc, qvl, cvl, qc) as b) ->
                qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = Some b }))) in
    let env = List.fold_left (Env.add_cvar loc) env cv_bindings in
    let env = List.fold_left (Env.add_qvar loc) env qv_bindings in
    let ty = circuit env qc in
    qv_bindings
    |> List.iter (fun (qv, qvb) ->
           if not qvb.Env.used then
             Fmt.(raise_failwithf qvb.Env.loc "TYCHK.circuit: qvar %a not used" SYN.QV.pp_hum qv)
           else ()) ;
    ty
  }
] ;

value top_circuit env qc = circuit env qc ;

value gate_item loc env gitem = match gitem with [
  SYN.QEnv.DEF gn (((pvl, qvl, cvl) as glam), qc) -> do {
    let (fv_pvs, fv_qvs, fv_cvs) = Ops.circuit_freevars qc in
    let fv_pvs = SYN.PVFVS.subtract fv_pvs (SYN.PVFVS.ofList pvl) in
    let fv_qvs = SYN.QVFVS.subtract fv_qvs (SYN.QVFVS.ofList qvl) in
    let fv_cvs = SYN.CVFVS.subtract fv_cvs (SYN.CVFVS.ofList cvl) in
    if SYN.PVFVS.mt <> fv_pvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free param-vars %a" SYN.QG.pp_hum gn SYN.PVFVS.pp fv_pvs)
    else if SYN.QVFVS.mt <> fv_qvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free qvars %a" SYN.QG.pp_hum gn SYN.QVFVS.pp fv_qvs)
    else if SYN.CVFVS.mt <> fv_cvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free cvars %a" SYN.QG.pp_hum gn SYN.CVFVS.pp fv_cvs)
    else
    let env' = Env.mk_for_gate env in
    let qvbl = qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = None })) in
    let env' = List.fold_left (Env.add_qvar loc) env' qvbl in
    let env' = List.fold_left (fun env cv -> Env.add_cvar loc env (cv, None)) env' cvl in
    let env' = List.fold_left (fun env pv -> Env.add_pvar loc env (pv, ())) env' pvl in
    let ty = top_circuit env' qc in
    if not (qvbl |> List.for_all (fun (_, qvb) -> qvb.Env.used)) then
      Fmt.(raise_failwithf loc "gate_item: not all qvars were used (failure of linearity)")
    else
      Env.add_gate loc env (gn, (glam, ty))
  }
| OPAQUE gn ((pvl, qvl, cvl) as glam) ->
   let ty = (List.length qvl, List.length cvl) in
   Env.add_gate loc env (gn,  (glam, ty))
] ;


value rec env_item env ei = match ei with [
  SYN.QEnv.QINCLUDE loc _ fname l ->
   List.fold_left env_item env l
| QGATE loc gitem -> gate_item loc env gitem
] ;

value env env_items =
  let env = Env.mk () in
  List.fold_left env_item env env_items
;

value program (env_items, qc) =
  let env = Env.mk () in
  let env = List.fold_left env_item env env_items in
  let ty = top_circuit env qc in
  (env, ty)
;

end ;
