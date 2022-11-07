
open Misc_functions ;
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_misc ;
open Qlam_syntax ;
open Qlam_env ;

open SYN ;

value pe_freevars pe =
  let rec fvrec = fun [
        ID _ pv ->  (PVFVS.ofList [pv])
      | CONST _ _ -> PVFVS.mt
      | BINOP _ _ pe1 pe2 -> PVFVS.union (fvrec pe1) (fvrec pe2)
      | UNOP _ _ pe -> fvrec pe
      | UFUN _ _ pe -> fvrec pe
      ] in
  fvrec pe
;

value circuit_freevars qc =
  let rec fvrec = fun [
        QLET loc bl qc ->
        let (pvs, qvs, cvs) = fvrec qc in
        let qvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) -> qvl) |> QVFVS.ofList in
        let cvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) -> cvl) |> CVFVS.ofList in
        let qvs = QVFVS.subtract qvs qvars in
        let cvs = CVFVS.subtract cvs cvars in
        List.fold_left (fun (pvs, qvs,  cvs) (_, _, _, qc) ->
            let (pvs', qvs', cvs') = fvrec qc in
            (PVFVS.union pvs pvs', QVFVS.union qvs qvs', CVFVS.union cvs cvs'))
          (PVFVS.mt,  qvs, cvs) bl
      | QWIRES _ qvl cvl -> (PVFVS.mt,  QVFVS.ofList qvl, CVFVS.ofList cvl)
      | QGATEAPP _ _ pel qvl  cvl ->
         let pvl = List.fold_left (fun pvl pe -> PVFVS.union pvl (pe_freevars pe)) PVFVS.mt pel in
         (pvl, QVFVS.ofList qvl, CVFVS.ofList cvl)
      | QBARRIER _ qvl  -> (PVFVS.mt,  QVFVS.ofList qvl, CVFVS.mt)
      | QCREATE _ _ -> (PVFVS.mt, QVFVS.mt, CVFVS.mt)
      | QDISCARD _ qv -> (PVFVS.mt,  QVFVS.ofList [qv],  CVFVS.mt)
      | QMEASURE _ qv -> (PVFVS.mt, QVFVS.ofList [qv], CVFVS.mt)
      | QRESET _ qv -> (PVFVS.mt, QVFVS.ofList [qv], CVFVS.mt)
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
    | QCREATE loc u -> QCREATE loc u
    | QDISCARD loc qv -> QDISCARD loc (rename_qv qv)
    | QMEASURE loc qv -> QMEASURE loc (rename_qv qv)
    | QRESET loc qv -> QRESET loc (rename_qv qv)
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
    
  | (QCREATE _ u1, QCREATE _ u2) -> True
                      
  | (QDISCARD loc qv1, QDISCARD _ qv2) ->
     check_qv_corr loc qv_lr qv_rl qv1 qv2
    
  | (QMEASURE loc qv1, QMEASURE _ qv2) ->
     check_qv_corr loc qv_lr qv_rl qv1 qv2
    
  | (QRESET loc qv1, QRESET _ qv2) ->
     check_qv_corr loc qv_lr qv_rl qv1 qv2
    
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

include Counter ;

module Internal = struct

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
    | QCREATE loc bi -> QCREATE loc bi
    | QDISCARD loc qv -> QDISCARD loc (map_qvar qv)
    | QMEASURE loc qv -> QMEASURE loc (map_qvar qv)
    | QRESET loc qv -> QRESET loc (map_qvar qv)
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

value gate_item ~{counter} gitem = match gitem with [
    DEF loc gn glam -> DEF loc gn (qgatelam ~{counter=counter} glam)
  | OPAQUE loc gn gargs -> OPAQUE loc gn gargs
  ]
;

value rec env_item ~{counter} = fun [
      QGATE loc gitem -> QGATE loc (gate_item ~{counter=counter} gitem)
   | QINCLUDE loc fty fname l ->
      QINCLUDE loc fty fname (environ ~{counter=counter} l)
   | QCOUPLING_MAP loc id m -> QCOUPLING_MAP loc id m
   | QLAYOUT loc id l -> QLAYOUT loc id l
    ] 

and environ ~{counter} envitems = List.map (env_item ~{counter=counter}) envitems
;

value program ~{counter} ((envitems, qc) as p) =
  (environ ~{counter=counter} envitems,
   qcircuit ~{counter=counter} ~{qvmap=QVMap.empty} ~{cvmap=CVMap.empty} qc)
;

end ;


module MaxID = struct
value qcircuit qc =
  let max_id = ref (-1) in
  let open Qlam_migrate in
  let dt = make_dt() in
  let migrate_id dt ((_, n) as x) = do {
    max_id.val := max max_id.val n ;
    x
  } in
  let dt = { (dt) with migrate_id = migrate_id } in do {
    ignore(dt.migrate_qcirc_t dt qc) ;
    max_id.val
  }
;
value program p =
  let max_id = ref (-1) in
  let open Qlam_migrate in
  let dt = make_dt() in
  let migrate_id dt ((_, n) as x) = do {
    max_id.val := max max_id.val n ;
    x
  } in
  let dt = { (dt) with migrate_id = migrate_id } in do {
    ignore(dt.migrate_program_t dt p) ;
    max_id.val
  }
;
end ;

value qcircuit qc =
  let counter = mk ~{base=1 + MaxID.qcircuit qc} () in
  Internal.qcircuit ~{counter=counter} ~{qvmap=QVMap.empty} ~{cvmap=CVMap.empty} qc
;

value program ((envitems, qc) as p) =
  let counter = mk ~{base=1 + MaxID.program p} () in
  Internal.program ~{counter=counter} p
;

end ;

module BetaReduce = struct

value subst_pe pvmap pe =
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
  | (QCREATE _ _) as qc -> qc
  | QDISCARD loc qv -> QDISCARD loc (subst_qvar qv)
  | QMEASURE loc qv -> QMEASURE loc (subst_qvar qv)
  | QRESET loc qv -> QRESET loc (subst_qvar qv)
  ] in
  substrec qc
;

value qcircuit ~{counter} genv = fun [
  QGATEAPP loc gn pel qel cel ->
  let ((pvl,qvl, cvl), qc) = 
    match QGMap.find gn genv with [
      exception Not_found -> Fmt.(raise_failwithf loc "BetaReduce.qcircuit: gate %a not found" QG.pp_hum gn)
    | x -> Fresh.Internal.qgatelam ~{counter=counter} x ] in
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

| x -> Fmt.(raise_failwithf (loc_of_qcirc x) "BetaReduce.qcircuit: can only be applied to gate-application, not %a" PP.qcirc x)
] ;

end ;

module ANorm = struct

(** [separate_let_bindings qcirc] rewrites lets with multiple bindings, where one or more is itself a let
    so that a binding with a let is always a singleton.  That is to say,

    let x = (let w = E1 in E2) and x' = (let w' = E1' in E2') and y = E3 and z = E4 in
    E5

    gets rewritten to

    let x = (let w = E1 in E2) in
    let x' = (let w' = E1' in E2') in
    let y = E3 and z = E4 in
    E5

    For this to be safe, x, x' must not appear in the freevars of E3, E4.
 *)

module FVS = struct
type t = (QVFVS.t * CVFVS.t) ;
value mt = (QVFVS.mt, CVFVS.mt) ;
value union (q1, c1) (q2, c2) = (QVFVS.union q1 q2, CVFVS.union c1 c2) ;
value concat l = List.fold_left union mt l ;
value subtract_ids (q, c) (qvl, cvl) = 
  (QVFVS.(subtract q (ofList qvl)), CVFVS.(subtract c (ofList cvl))) ;
value subtract (q1, c1) (q2, c2) = 
  (QVFVS.(subtract q1 q2), CVFVS.(subtract c1 c2)) ;
value intersect (q1, c1) (q2, c2) = 
  (QVFVS.(intersect q1 q2), CVFVS.(intersect c1 c2)) ;
value of_ids (qvl, cvl) = (QVFVS.ofList qvl, CVFVS.ofList cvl) ;
value pp_hum pps (q, c) = Fmt.(pf pps "{q=%a; c=%a}" QVFVS.pp_hum q CVFVS.pp_hum c) ;
end ;

value compute_binding_fvs bindingf ((loc, qvl, cvl, qc) : qbinding_t) : (qbinding_t * FVS.t) =
  let (qc, fvs) = bindingf qc in
  ((loc, qvl, cvl, qc), fvs)
;

value rebuild_let_fvs loc bl_fvs (qc,fvs) =
  if bl_fvs = [] then (qc, fvs) else
  (* the bindings of the rest *)
  let bl = List.map fst bl_fvs in
  (* qvars bound in the rest *)
  let bl_qvl = bl |> List.map qbinding_qvl |> List.concat in
  (* cvars bound in the rest *)
  let bl_cvl = bl |> List.map qbinding_cvl |> List.concat in

  (* subtract IDs bound in [bl] from [fvs] *)
  let fvs = FVS.subtract_ids fvs (bl_qvl, bl_cvl) in

  (* fvs of rhs of bindings *)
  let bl_fvs = FVS.concat (List.map snd bl_fvs) in

  (* [fvs of qc] - [ids bound in bl] union [fvs of RHS of bl] *)
  let fvs = FVS.union fvs bl_fvs in
  
  (* let rest-bindings in qc *)
  (QLET loc bl qc, fvs)
;

value rebuild_letlist_fvs (b, b_fvs) (qc, qc_fvs) =
  let (ll, b_qc) = SYN.to_letlist (qbinding_qc b) in
  let _ = assert ([] <> ll) in
  let bl =
    ll
    |> List.map snd
    |> List.concat in
  let qc =
    let (loc, qvl, cvl, _) = b in
    QLET loc [(loc, qvl, cvl, b_qc)] qc  in
  let qc = List.fold_right (fun b qc ->
               let loc = qbinding_loc b in
               QLET loc [b] qc) bl qc in
  (qc, FVS.union b_fvs qc_fvs)
;

(** [anorm qcirc] A-normalizes the circuit.  For this restricted language, this consists in flattening
    let-trees, viz. (where all-CAPS identifiers stand for circuit-expressions and lower-caps identifiers
    for qubits/wires.

    let x =
      let y = CIRC in z
    in W

    into

    let y = CIRC in
    let x = z in
    W

    This introduces renamings, which can be eliminated by a further pass.

    There is a subtlety here, which is that (above) y might be free in W, which would mean that the naive
    rewrite would *capture* y in W.

    We will *assume* that this never happens, but will *check* for it (it's a simple bottom-up calculation of
    free-variables) and raise an error if it happens.

    NOTE WELL: a let-binding (e.g. "let x = let y = ...") will always appear separate from other bindings;
    [separate_let_binding] does that transformation.

 *)

(** [anormalizable loc bindings_and_fvs qc_and_fvs]

    a LET with a collection of top-level bindings  is A-normalizable if:

   (1) all binding-ids of all top-level bindings MUST NOT capture any of the RHS of the top-level bindings.

   (2) for all bindings that are let-lists (e.g. "let ... in let ... in ..."), all binding-ids
       of all bindings in let-lists MUST NOT capture any of the RHS of the top-level bindings, nor
       of the body of the LET itself.

   (3) {all binding-ids of all top-level bindings} concat {all binding-ids in let-lists} MUST be distinct.

 *)
value anormalize_let loc bl_fvs (qc, qc_fvs) = do {
    let is_let_binding ((_, _, _, qc), _) = match qc with [
      SYN.QLET _ _ _ -> True | _ -> False
    ] in
    let top_binders =
      let qvl = bl_fvs |> List.map fst |>  List.map qbinding_qvl |>  List.concat in
      let cvl = bl_fvs |> List.map fst |>  List.map qbinding_cvl |>  List.concat in
      (qvl, cvl) in
    let top_binders_fvs = FVS.of_ids top_binders in

    let letlist_binders =
      let letlist_l : list (list (loc * list qbinding_t) * qcirc_t) =
        bl_fvs
        |>  List.map fst
        |> List.map qbinding_qc
        |> List.map SYN.to_letlist (* list letlists *) in
      let letlist_bl =
        letlist_l
        |> List.map fst (* list (list (loc * list qbinding)) *)
        |> List.concat (* list (loc * list qbinding) *)
        |> List.map snd (* list (list qbinding) *)
        |> List.concat (* list (list qbinding) *) in
      let letlist_qvl = letlist_bl |> List.map qbinding_qvl |> List.concat in
      let letlist_cvl = letlist_bl |> List.map qbinding_cvl |> List.concat in
      (letlist_qvl, letlist_cvl) in
    let letlist_binders_fvs = FVS.of_ids letlist_binders in

    let binder_ids =
      (top_binders |> fst |> List.map QV.toID)
      @(top_binders |> snd |> List.map CV.toID)
      @(letlist_binders |> fst |> List.map QV.toID)
      @(letlist_binders |> snd |> List.map CV.toID) in

    bl_fvs
    |> List.iter (fun (b, binding_fvs) ->
           if FVS.(mt <> (intersect top_binders_fvs binding_fvs)) then
             Fmt.(raise_failwithf loc "anormalize_let: top binders of let-bindings %a capture RHS of let-binding %a"
                    FVS.pp_hum top_binders_fvs PP.qbinding b)
           else if FVS.(mt <> (intersect letlist_binders_fvs binding_fvs)) then
             Fmt.(raise_failwithf loc "anormalize_let: letlist binders of let-bindings %a capture RHS of let-binding %a"
                    FVS.pp_hum letlist_binders_fvs PP.qbinding b)
           else ()
         ) ;
    if FVS.(mt <> (intersect letlist_binders_fvs qc_fvs)) then
      Fmt.(raise_failwithf loc "anormalize_let: letlist binders of let-bindings %a capture body of LET"
             FVS.pp_hum letlist_binders_fvs)
    else if not (Std.distinct binder_ids) then
      Fmt.(raise_failwithf loc "anormalize_let: binders are not distinct %a"
             (list ~{sep=const string " "} ID.pp_hum) binder_ids)
    else
    let (letbindings_fvs, restbindings_fvs) = filter_split is_let_binding bl_fvs in
    let (qc, qc_fvs) = rebuild_let_fvs loc restbindings_fvs (qc, qc_fvs) in
    List.fold_right rebuild_letlist_fvs letbindings_fvs (qc,qc_fvs)
  } ;

value qcircuit qc =
  let rec anrec qc = match qc with [
    SYN.QLET loc bl qc ->
     let bl_fvs =
       bl |> List.map (fun (loc, qvl, cvl, qc) ->
                 let (qc, fvs) = anrec qc in
                 ((loc, qvl, cvl, qc), fvs)
               ) in
     let (qc, qc_fvs) = anrec qc in
     anormalize_let loc bl_fvs (qc, qc_fvs)

  | QWIRES _ qvl cvl -> (qc, FVS.(of_ids (qvl, cvl)))
  | QGATEAPP _ _ pvl qvl cvl -> (qc, FVS.(of_ids (qvl, cvl)))
  | QBARRIER _ qvl -> (qc, FVS.(of_ids (qvl, [])))
  | QCREATE _ _ -> (qc, FVS.mt)
  | QDISCARD _ qv -> (qc, FVS.(of_ids ([qv], [])))
  | QMEASURE _ qv -> (qc, FVS.(of_ids ([qv], [])))
  | QRESET _ qv -> (qc, FVS.(of_ids ([qv], [])))
  ] in
  let (qc, _) = anrec qc in
  qc
;


value qgatelam ((pvl, qvl, cvl), qc) =
  let qc = qcircuit qc in
  ((pvl, qvl, cvl), qc)
;


value gate_item gitem = match gitem with [
    DEF loc gn glam -> DEF loc gn (qgatelam glam)
  | OPAQUE loc gn gargs -> OPAQUE loc gn gargs
  ]
;

value environ envitems =
  let rec env_item = fun [
      QGATE loc gitem -> QGATE loc (gate_item gitem)
   | QINCLUDE loc fty fname l ->
      QINCLUDE loc fty fname (List.map env_item l)
   | QCOUPLING_MAP loc id m -> QCOUPLING_MAP loc id m
   | QLAYOUT loc id l -> QLAYOUT loc id l
   ] in
  List.map env_item envitems
;

value program ((envitems, qc) as p) =
  (environ envitems, qcircuit qc)
;

end ;

module NameNorm = struct

value is_rename_binding = fun [ (_, _, _, QWIRES _ _ _) -> True | _ -> False ] ;

value qcircuit qc =
  let rec nnrec (qvmap, cvmap) qc =
    let map_qv qv = match QVMap.find qv qvmap with [ exception Not_found -> qv | x -> x ] in 
    let map_cv cv = match CVMap.find cv cvmap with [ exception Not_found -> cv | x -> x ] in 
    match qc  with [
    SYN.QLET loc bl qc ->
    let (rename_bindings, rest_bindings) = filter_split is_rename_binding bl in
    let rest_bindings = List.map (fun (loc, qvl, cvl, qc) -> (loc, qvl, cvl, nnrec (qvmap, cvmap) qc)) rest_bindings in
    let (qvmap,cvmap) =
      List.fold_left (fun (qvmap,cvmap) b ->
             let (loc, qvl, cvl, qel, cel) = match b with [
                   (loc, qvl, cvl, QWIRES _ qel cel) -> (loc, qvl, cvl, qel, cel)
                 | _ -> assert False ] in
             if List.length qvl <> List.length qel then
               Fmt.(raise_failwithf loc "NameNorm.nnorm: malformed binding: qvar lengths don't match: len(%a) <> len(%a)"
                      (brackets (list ~{sep=const string " "} QV.pp_hum)) qvl
                      (brackets (list ~{sep=const string " "} QV.pp_hum)) qel)
             else if List.length cvl <> List.length cel then
               Fmt.(raise_failwithf loc "NameNorm.nnorm: malformed binding: cvar lengths don't match: len(%a) <> len(%a)"
                      (brackets (list ~{sep=const string " "} CV.pp_hum)) cvl
                      (brackets (list ~{sep=const string " "} CV.pp_hum)) cel)
             else
               let qvmap = List.fold_right2 QVMap.add qvl qel qvmap in
               let cvmap = List.fold_right2 CVMap.add cvl cel cvmap in
               (qvmap, cvmap))
        (qvmap, cvmap) rename_bindings in
    let qc = nnrec (qvmap, cvmap) qc in
    if rest_bindings = [] then qc else
      SYN.QLET loc rest_bindings qc

  | QWIRES loc qvl cvl -> QWIRES loc (List.map map_qv qvl) (List.map map_cv cvl)
  | QGATEAPP loc gn pvl qvl cvl -> QGATEAPP loc gn pvl (List.map map_qv qvl) (List.map map_cv cvl)
  | QBARRIER loc qvl -> QBARRIER loc (List.map map_qv qvl)
  | QCREATE _ _ -> qc
  | QDISCARD loc qv -> QDISCARD loc (map_qv qv)
  | QMEASURE loc qv -> QMEASURE loc (map_qv qv)
  | QRESET loc qv -> QRESET loc (map_qv qv)
  ] in
  nnrec (QVMap.empty, CVMap.empty) qc
;

value qgatelam ((pvl, qvl, cvl), qc) =
  let qc = qcircuit qc in
  ((pvl, qvl, cvl), qc)
;


value gate_item gitem = match gitem with [
    DEF loc gn glam -> DEF loc gn (qgatelam glam)
  | OPAQUE loc gn gargs -> OPAQUE loc gn gargs
  ]
;

value environ envitems =
  let rec env_item = fun [
      QGATE loc gitem -> QGATE loc (gate_item gitem)
   | QINCLUDE loc fty fname l ->
      QINCLUDE loc fty fname (List.map env_item l)
   | QCOUPLING_MAP loc id m -> QCOUPLING_MAP loc id m
   | QLAYOUT loc id l -> QLAYOUT loc id l
   ] in
  List.map env_item envitems
;

value program ((envitems, qc) as p) =
  (environ envitems, qcircuit qc)
;

end ;

module Env = struct

end ;

module Unroll = struct

type env_t = QGMap.t qgatelam_t ;
value mk_env ~{only} ~{except} env =
  let (only, except) = match (only, except) with [
        (None, None) -> (None, Some [])
      | (Some l, None) -> (Some l, None)
      | (None, Some l) -> (None, Some l)
      | (Some l1, Some l2) ->
         Fmt.(failwithf "Qlam_ops.Env.mk: cannot specify both ~{only=%a} and ~{except=%a}"
                (list ~{sep=const string ", "} QG.pp_hum) l1
                (list ~{sep=const string ", "} QG.pp_hum) l2)
      ] in
  let accept gn =
    (match only with [ None -> True | Some l -> List.exists (QG.equal gn) l ])
    && (match except with [ None -> False | Some l -> not(List.exists (QG.equal gn) l) ]) in
  let rec flatrec m = fun [
      QGATE _ (DEF _ gn x) -> if accept gn then QGMap.add gn x m else m
    | QGATE _ (OPAQUE _ _ _) -> m
    | QINCLUDE _ _ _ l ->
       List.fold_left flatrec m l
    ] in
  List.fold_left flatrec QGMap.empty env
;


(** unroll: takes a map [gatename -> gatelam] and a qcircuit,  unrolls all gates possible.

    A gate that is not in the map, is not unrolled.  So the selection of the map completely 
    determines what is unrolled.
 *)

value unroll ?{only} ?{except} (env : SYN.environ_t) qc =
  let genv = mk_env ~{only=only} ~{except=except} env in
  let counter = Fresh.(mk ~{base=1 + MaxID.qcircuit qc} ()) in
  let qc = Fresh.Internal.qcircuit ~{counter=counter} ~{qvmap=QVMap.empty} ~{cvmap=CVMap.empty} qc in
  let rec unrec qc = match qc with [
    QLET loc bl qc ->
    QLET loc (List.map (fun (loc, qvl, cvl, qc) -> (loc, qvl, cvl, unrec qc)) bl) (unrec qc)

  | QGATEAPP _ gn _ _ _ when QGMap.mem gn genv ->
     let qc = BetaReduce.qcircuit ~{counter=counter} genv qc in
     unrec qc

  | (QGATEAPP _ _ _ _ _
     | QWIRES _ _ _
    | QBARRIER _ _
    | QCREATE _ _ | QDISCARD _ _
    | QMEASURE _ _
    | QRESET _ _) -> qc
  ]
  in unrec qc
;

end ;

module Hoist = struct

  (** Hoist let-bindings as far "up" as possible.

    Method:

    (1) assign a distinct numeric index to each let-binding

    (2) edge from [binding i]->[binding j] if a variable bound in [i]
     is among the freevars of the RHS of [binding j].

    (3) apply tsort to this graph, producing layers (lists of bindings
     that are independent of each other)

    (4) and rebuild the let-list from these layers

 *)


(* representation of a node -- must be hashable *)
module Node = struct
   type t = int ;
   value compare (v1: t) (v2: t) = Stdlib.compare v1 v2 ;
   value hash = Hashtbl.hash ;
   value equal = (=) ;
end ;

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = ID.t ;

   value compare = Stdlib.compare ;
   value equal = (=) ;
   value default = ("", -1) ;
end ;

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge) ;

(* more modules available, e.g. graph traversal with depth-first-search *)
module D = Graph.Traverse.Dfs(G) ;

(* Stable Tsort
 *
 * We have two datastructures we use for the algorithm:
 *
 * (a) [todo]: this holds a list of zero-indegree vertexes
 *
 * (b) [indegree]: this is a map from vertex to indegree, for vertexes of NONZERO indegree
 *
 * If at any time, indegree is not empty, but todo is empty, we have a failure (b/c there's a cycle)
 *
 * Algorithm:
 *
 * (1) basis case: iterate across vertexes, populating [todo] and [indegree]
 *
 * (2) step case:
 *     (a) grab [todo] list as [t]
 *     (b) zero [todo] list
 *     (c) for each vertex [v] in [t], decrement [indegree v]
 *         if [indegree v] is zero, delete from [indegree] and insert into [todo]
 *     (d) sort [t]
 *     (e) deliver [v] in [t] to [f]
 *
 *)

value tsort g =
  let todo = ref [] in
  let indegree = Hashtbl.create 97 in do {

    G.iter_vertex (fun v ->
        let d = G.in_degree g v in
        if d = 0 then Std.push todo v
        else Hashtbl.add indegree v d
      ) g;
    
    let rec dorec acc =
      if todo.val = [] then
        if Hashtbl.length indegree = 0 then acc
        else failwith "tsort: DAG was cyclic!"
      else
        let work = todo.val in do {
          todo.val := [] ;
          List.iter (fun src ->
              G.iter_succ_e (fun (_, _, dst) ->
                  if not (Hashtbl.mem indegree dst) then failwith "tsort: DAG was cyclic" else
                    let d = Hashtbl.find indegree dst in
                    if d = 1 then do {
                      Hashtbl.remove indegree dst ;
                      Std.push todo dst
                    }
                    else
                      Hashtbl.replace indegree dst (d-1)
                ) g src
            ) work ;
          let work = List.sort compare work in
          let acc = [work :: acc] in
          dorec acc
        }
    in List.rev (dorec [])
  }
;

value make_dag loc ll =
  let bl =
    ll
    |> List.concat_map (fun (loc, bl) ->
           bl |> List.map (fun (loc', a, b, c) -> (ploc_encl_with_comments loc loc', a,b,c))) in
  let bl_fvs = bl |> List.map (fun ((_, _, _, qc) as b) ->
                         let (_, q, c) = circuit_freevars qc in
                         (b, (q,c))) in
  let numbered_bl_fvs = bl_fvs |> List.mapi (fun i b -> (i,b)) in
  let bl_array = Array.of_list bl in
  let all_bound_ids =
    bl
    |> List.concat_map (fun (_, qvl, cvl, _) ->
           (List.map QV.toID qvl)@(List.map CV.toID cvl)) in
  if not (Std.distinct all_bound_ids) then
    Fmt.(raise_failwithf loc "Hoist.make_dag: not all binder-vars are distinct")
  else
  let var2node =
    List.fold_left (fun m (node, ((_, qvl, cvl, _), _)) ->
        let bound_ids = (List.map QV.toID qvl)@(List.map CV.toID cvl) in
        List.fold_left (fun m id -> IDMap.add id node m) m bound_ids
      ) IDMap.empty numbered_bl_fvs in
  let g = G.empty in
  let g = List.fold_left (fun g (node, _) -> G.add_vertex g node) g numbered_bl_fvs in
  let g = List.fold_left (fun g (node, ((_, qvl, cvl, _), fvs)) ->
    let free_ids = (fvs |> fst |> QVFVS.toList |> List.map QV.toID)
                   @(fvs |> snd |> CVFVS.toList |> List.map CV.toID) in
    List.fold_left (fun g id ->
        let e = G.E.create (IDMap.find id var2node) id node in
        G.add_edge_e g e)
      g free_ids)
            g numbered_bl_fvs in
  (g, bl_array)
;

value hoist qc =
  let (ll, qc0) = SYN.to_letlist qc in
  let (g, bl_array) = make_dag (loc_of_qcirc qc) ll in
  let layers = tsort g in
  let bll = layers |> List.map (List.map (Array.get bl_array)) in
  List.fold_right (fun bl qc -> SYN.QLET Ploc.dummy bl qc) bll qc0
;
end ;

(** TODO

(1) alpha-equality

(2) raise_fresh

(3) beta-reduce

(4) unroll

(5) A-normalize / name-normalize

(6) hoist (maximize parallelism)

(7) SabreSwap

  (a) coupling map
  (b) 


 *)

module GraphExtra(G : (Graph.Sig.P with type E.label = int)) = struct
  module G = G ;


value distance g v1 v2 =
    match G.find_edge g v1 v2 with [
        exception Not_found -> max_int
      | e -> G.E.label e
      ] ;
value add_distances g v1 v2 v3 =
    let d12 = distance g v1 v2 in
    let d23 = distance g v2 v3 in
    if d12 = max_int || d23 = max_int then max_int
    else d12+d23 ;
value upsert_edge g v1 newdist v2 =
  if v1 = v2 then g else
    let dist0 = distance g v1 v2  in
    if newdist < dist0 then
      G.(add_edge_e (G.remove_edge g v1 v2) (E.create v1 newdist v2))
    else g ;


value add_transitive_closure ?{reflexive=False} g0 =
    let phi v g =
      let g = if reflexive then upsert_edge g v 0 v else g in
      G.fold_succ
        (fun sv g -> G.fold_pred (fun pv g -> upsert_edge g pv (add_distances g pv v sv) sv) g v g)
        g v g
    in
    G.fold_vertex phi g0 g0
;
  value transitive_closure ?{reflexive=False} g0 =
    add_transitive_closure ~{reflexive=reflexive} g0
;

value of_edges edges = 
  List.fold_left (fun g (i,j) ->
      G.(add_edge_e g (G.E.create i 1 j)))
  G.empty edges
;

module Weight = struct
  type edge = G.E.t ;
  type t = G.E.label ;
  value weight e = G.E.label e ;
  value compare = Int.compare ;
  value add x y = x + y ;
  value zero = 0 ;
end ;

module DSP = Graph.Path.Dijkstra(G)(Weight) ;

end ;

module CM = struct
open CouplingMap ;

(* representation of a node -- must be hashable *)
module Node = struct
   type t = int ;
   value compare (v1: t) (v2: t) = Stdlib.compare v1 v2 ;
   value hash = Hashtbl.hash ;
   value equal = (=) ;
end ;

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = int ;

   value compare = Stdlib.compare ;
   value equal = (=) ;
   value default = max_int ;
end ;

(* a functional/persistent graph *)
module DAG = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge) ;
module DAGX = GraphExtra(DAG) ;

module G = Graph.Persistent.Graph.ConcreteLabeled(Node)(Edge) ;
module GX = GraphExtra(G) ;

type graphs_t 'a = { underlying : 'a ; tclosure : 'a } ;
type t = {
    g : graphs_t G.t
  ; dag : graphs_t DAG.t
  ; positions : IntMap.t (int * int)
  ; syntax : SYN.CouplingMap.t
  } ;

value mk syntax =
  let {SYN.CouplingMap.edges=edges; positions=positions} = syntax in
  let g = GX.of_edges edges in
  let dag = DAGX.of_edges edges in
  let positions = IntMap.ofList positions in
  { g = { underlying = g ; tclosure = GX.transitive_closure g }
  ; dag = { underlying = dag ; tclosure = DAGX.transitive_closure dag }
  ; positions = positions
  ; syntax = syntax
  } ;

value has_pair it v1 v2 = DAG.mem_edge it.dag.underlying v1 v2 ;
value has_path it v1 v2 = DAG.mem_edge it.dag.tclosure v1 v2 ;
value path it v1 v2 =
  let (edges, _) = DAGX.DSP.shortest_path it.dag.underlying v1 v2 in
  [v1:: List.map (fun (s,_,d) -> d) edges]
;
value dot ?{terse=True} ?{tclos=False} cm =
  let g = if tclos then cm.dag.tclosure else cm.dag.underlying in
  let positions = cm.positions in
  let open Odot in
  let dot_vertex_0 v acc =
    let color = "lightblue" in
    let label = string_of_int v in
    let attrs = [(Simple_id "color", Some (Simple_id "black"));
                 (Simple_id "fillcolor", Some (Simple_id color));
                 (Simple_id "label", Some (Double_quoted_id label));
                 (Simple_id "style", Some (Simple_id "filled"))
                ] in
    let attrs = match IntMap.find v positions with [
          exception Not_found -> attrs
        | (x,y) -> 
           let pos = Fmt.(str "%d,%d!" x y) in
           attrs@[(Simple_id "pos", Some (Double_quoted_id pos))]
        ] in
    ([(Stmt_node (Simple_id (string_of_int v), None) attrs) :: acc]) in
    let dot_edge_0 (s, label, d) acc =
      ([(Stmt_edge
        (Edge_node_id (Simple_id (string_of_int s), None),
         [Edge_node_id (Simple_id (string_of_int d), None)],
         [
           (Simple_id "label", Some (Double_quoted_id (string_of_int label)))
        ])) :: acc]) in

    let l =
      []
      |> DAG.fold_vertex dot_vertex_0 g
      |> DAG.fold_edges_e dot_edge_0 g
      |> List.rev in
    let l = 
      [(Odot.Stmt_attr
         (Odot.Attr_node
            [(Odot.Simple_id "label", Some (Odot.Double_quoted_id "\\N"))])) :: l] in

    {strict = False; kind = Digraph; id = Some (Simple_id "G");
     stmt_list = l }
;

value dot_to_file fname p =
  Std.apply_to_out_channel (fun oc -> Odot.print oc p) fname
;
end ;
module CouplingMap = CM ;

module LO = struct
  module M = BI_Phys_BIJ ;
  type t = { layout : M.t ; logical : BISet.t ; physical : PQSet.t } [@@deriving (to_yojson, show, eq, ord);] ;
  value pp_hum pps l = M.pp_hum pps l.layout ;
  value empty = { layout = M.empty ; logical = BISet.mt ; physical = PQSet.mt } ;
  value assign l (log,phys) = {
      layout = M.insert log phys l.layout
    ; logical = BISet.add l.logical log
    ; physical = PQSet.add l.physical phys
    } ;
  value mk l = List.fold_left assign empty l.Layout.it ;

  value logical_to_physical l lbit = M.find lbit l.layout ;
  value physical_to_logical l phybit = M.find_rng phybit l.layout ;

  value has_logical_pair l cmap l1 l2 =
    let pq1 = logical_to_physical l l1 in
    let pq2 = logical_to_physical l l2 in
    CM.has_pair cmap (PQ.toInt pq1) (PQ.toInt pq2) ;

  value has_logical_path l cmap l1 l2 =
    let pq1 = logical_to_physical l l1 in
    let pq2 = logical_to_physical l l2 in
    CM.has_path cmap (PQ.toInt pq1) (PQ.toInt pq2) ;

  value logical_path l cmap l1 l2 =
    let pq1 = logical_to_physical l l1 in
    let pq2 = logical_to_physical l l2 in
    let physpath = CM.path cmap (PQ.toInt pq1) (PQ.toInt pq2) in
    List.map (fun n -> physical_to_logical l (PQ.ofInt n)) physpath ;

  value swap l (logical_i,logical_j) =
    let m = l.layout in
    let phys_i = M.find logical_i m in
    let phys_j = M.find logical_j m in
    let m = M.remove logical_i m in
    let m = M.remove logical_j m in
    let m = M.insert logical_i phys_j m in
    let m = M.insert logical_j phys_i m in
    { (l) with layout = m }
  ;
end ;

module TYCHK = struct

type env_gate_t = {
    args : qgateargs_t
  ; res : (int * int)
  } ;

module Env = struct

type qvar_binding_t = {
    used : mutable bool
  ; loc : Ploc.t
  ; it : option qbinding_t
  } ;

type cvar_binding_t = option qbinding_t ;
type pvar_binding_t = unit ;

type t = {
    genv : GEnv.t env_gate_t SYN.CouplingMap.t SYN.Layout.t
  ; qvars : QVMap.t qvar_binding_t
  ; cvars : CVMap.t cvar_binding_t
  ; pvars : PVMap.t pvar_binding_t
  }
;

value mk genv = {
  genv = genv
; qvars = QVMap.empty
; cvars = CVMap.empty
; pvars = PVMap.empty
} ;

value add_qvar loc env (id, qvb) =
  { (env) with qvars = QVMap.add id qvb env.qvars }
;

value add_cvar loc env (id, cv) =
  { (env) with cvars = CVMap.add id cv env.cvars }
;

value add_pvar loc env (id, pv) =
  { (env) with pvars = PVMap.add id pv env.pvars }
;

value has_gate loc env id = GEnv.has_gate loc env.genv id ;
value has_qvar loc env id = QVMap.mem id env.qvars ;
value has_cvar loc env id = CVMap.mem id env.cvars ;
value has_pvar loc env id = PVMap.mem id env.pvars ;

value find_mach ?{loc=Ploc.dummy} env mid = GEnv.find_mach ~{loc=loc} env.genv mid ;
value find_gate ?{loc=Ploc.dummy} env gid = GEnv.find_gate ~{loc=loc} env.genv gid ;

value find_qvar ?{loc=Ploc.dummy} env qid = match QVMap.find qid env.qvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_qvar: qvar %a not found" QV.pp_hum qid)
] ;

value find_cvar ?{loc=Ploc.dummy} env cid = match CVMap.find cid env.cvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_cvar: cvar %a not found" CV.pp_hum cid)
] ;

value find_pvar ?{loc=Ploc.dummy} env pid = match PVMap.find pid env.pvars with [
  x -> x
| exception Not_found ->
   Fmt.(raise_failwithf loc "find_pvar: pvar %a not found" PV.pp_hum pid)
] ;

end ;

value qvar_find_mark_used loc env qv =
  match Env.find_qvar ~{loc=loc} env qv with [
      exception Not_found ->
                Fmt.(raise_failwithf loc "circuit: undeclared qvar %a" QV.pp_hum qv)
    | x -> if x.used then
             Fmt.(raise_failwithf loc "circuit: qvar %a used more than once" QV.pp_hum qv)
           else x.used := True ]
;

value cvar_find loc env cv = 
  if not (Env.has_cvar loc env cv) then
    Fmt.(raise_failwithf loc "circuit: undeclared cvar %a" CV.pp_hum cv)
  else ()
;

value rec circuit env qc = match qc with [
  QWIRES loc qvl cvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    cvl |> List.iter (cvar_find loc env) ;
    (List.length qvl, List.length cvl)
  }

| QCREATE _ _ -> (1, 0)
| QDISCARD loc qv -> do {
    qvar_find_mark_used loc env qv ;
    (0,0)
  }
| QBARRIER loc qvl -> do {
    qvl |> List.iter (qvar_find_mark_used loc env) ;
    (List.length qvl,0)
  }
| QRESET loc qv -> do {
    qvar_find_mark_used loc env qv ;
    (1,0)
  }
| QMEASURE loc qv -> do {
    qvar_find_mark_used loc env qv ;
    (1,1)
  }
| QGATEAPP loc gn pal qal cal ->
   let {args=(pfl, qfl, cfl); res=res} =
     match Env.find_gate ~{loc=loc} env gn with [
         exception Not_found ->
           Fmt.(raise_failwithf loc "gate-application: gate %a not found" QG.pp_hum gn)
       | x -> x
       ] in
   if List.length pal <> List.length pfl then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with param-var length mismatch" QG.pp_hum gn)
   else if List.length qal <> List.length qal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with qvar length mismatch" QG.pp_hum gn)
   else if List.length cal <> List.length cal then
     Fmt.(raise_failwithf loc "circuit: gate-application %a with cvar length mismatch" QG.pp_hum gn)
   else do {
    qal |> List.iter (qvar_find_mark_used loc env) ;
    res
  }
| QLET loc bl qc -> do {
    let qvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map QV.toID qvl)) in
    if not (Std.distinct qvars) then
      Fmt.(raise_failwithf loc "TYCHK.circuit: qvars in binding MUST be distinct")
    else () ;
    let cvars = bl |>  List.concat_map (fun (_, qvl, cvl, _) ->
      (List.map CV.toID cvl)) in
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
             Fmt.(raise_failwithf qvb.Env.loc "TYCHK.circuit: qvar %a not used" QV.pp_hum qv)
           else ()) ;
    ty
  }
] ;

value gather_qubits qc =
  let acc = ref [] in
  let open Qlam_migrate in
  let dt = make_dt() in
  let old_migrate_qcirc_t = dt.migrate_qcirc_t in
  let migrate_qcirc_t dt qc = match qc with [
        SYN.QCREATE _ u -> do { Std.push acc u ; qc }
      | _ -> old_migrate_qcirc_t dt qc
      ] in
  let dt = { (dt) with migrate_qcirc_t = migrate_qcirc_t } in do {
    ignore (dt.migrate_qcirc_t dt qc) ;
    acc.val
  }
;

value check_unique qc =
  if not (Std.distinct (gather_qubits qc)) then
    Fmt.(raise_failwithf (loc_of_qcirc qc) "check_unique: qubit() expressions are not unique")
  else ()
;

value top_circuit env qc = do {
    check_unique qc ;
    circuit env qc
}
;

value gate_item genv gitem = match gitem with [
  DEF loc gn (((pvl, qvl, cvl) as glam), qc) -> do {
    let (fv_pvs, fv_qvs, fv_cvs) = circuit_freevars qc in
    let fv_pvs = PVFVS.subtract fv_pvs (PVFVS.ofList pvl) in
    let fv_qvs = QVFVS.subtract fv_qvs (QVFVS.ofList qvl) in
    let fv_cvs = CVFVS.subtract fv_cvs (CVFVS.ofList cvl) in
    if PVFVS.mt <> fv_pvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free param-vars %a" QG.pp_hum gn PVFVS.pp fv_pvs)
    else if QVFVS.mt <> fv_qvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free qvars %a" QG.pp_hum gn QVFVS.pp fv_qvs)
    else if CVFVS.mt <> fv_cvs then
      Fmt.(raise_failwithf loc "TYCHK.gate_item: gate %a has free cvars %a" QG.pp_hum gn CVFVS.pp fv_cvs)
    else
    let env' = Env.mk genv in
    let qvbl = qvl |> List.map (fun qv -> (qv, { Env.used = False ; loc = loc ; it = None })) in
    let env' = List.fold_left (Env.add_qvar loc) env' qvbl in
    let env' = List.fold_left (fun env cv -> Env.add_cvar loc env (cv, None)) env' cvl in
    let env' = List.fold_left (fun env pv -> Env.add_pvar loc env (pv, ())) env' pvl in
    let ty = top_circuit env' qc in
    if not (qvbl |> List.for_all (fun (_, qvb) -> qvb.Env.used)) then
      Fmt.(raise_failwithf loc "gate_item: not all qvars were used (failure of linearity)")
    else
      {args=glam; res=ty}
  }
| OPAQUE loc gn ((pvl, qvl, cvl) as glam) ->
   let ty = (List.length qvl, List.length cvl) in
   {args=glam; res=ty}
] ;

value environ ?{env0=[]} env_items = GEnv.mk_of_environ gate_item (env0@env_items) ;

value program ?{env0=[]} (env_items, qc) =
  let genv = environ ~{env0=env0} env_items in
  let ty = top_circuit (Env.mk genv) qc in
  (genv, ty)
;

end ;

module AB = struct
(** AssignBits computes the qubit/clbit assignment of every variable in the circuit.

To do this, we must first compute this assignment for every gate --
that is, how the gate maps the qubits assigned to its inputs, to
qubits assigned to its outputs.


 *)

type qubit_t = [ QUBIT of BI.t | QVAR of QV.t ][@@deriving (to_yojson, show, eq, ord);] ;
module QUBit = struct
type t = qubit_t[@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps = fun [
  QUBIT bi -> Fmt.(pf pps "%a" BI.pp_hum bi)
| QVAR qv -> Fmt.(pf pps "%a" QV.pp_hum qv)
] ;
end  ;
module QUBSet = EntitySet(QUBit) ;
module QUBMap = EntityMap(QUBit)(QUBSet) ;
type clbit_t = [ CLBIT of CV.t | CVAR of CV.t ][@@deriving (to_yojson, show, eq, ord);] ;
module CLBit = struct
type t = clbit_t[@@deriving (to_yojson, show, eq, ord);] ;
value pp_hum pps = fun [
  CLBIT cv -> Fmt.(pf pps "{clbit %a}" CV.pp_hum cv)
| CVAR cv -> Fmt.(pf pps "%a" CV.pp_hum cv)
] ;
end ;
module CLBSet = EntitySet(CLBit) ;
module CLBMap = EntityMap(CLBit)(CLBSet) ;
type env_gate_t = {
    args : qgateargs_t
  ; ty : (int * int)
  ; gate_env : (QVMap.t qubit_t * CVMap.t clbit_t)
  ; result : (list QV.t * list CV.t)
 } ;

module Env = struct
  type t = {
      qvmap : QVMap.t qubit_t
    ; cvmap : CVMap.t clbit_t
    ; qubits : list qubit_t
    ; clbits : list clbit_t
    }
  ;
  value pp_hum pps x =
    Fmt.(pf pps "{qvmap: %a; cvmap: %a; qubits: %a; clbits: %a}"
           (QVMap.pp_hum QUBit.pp_hum) x.qvmap
           (CVMap.pp_hum CLBit.pp_hum) x.cvmap
         (brackets (list ~{sep=const string ", "} QUBit.pp_hum)) x.qubits
         (brackets (list ~{sep=const string ", "} CLBit.pp_hum)) x.clbits
    ) ;

  value empty = {
      qvmap = QVMap.empty
    ; cvmap = CVMap.empty
    ; qubits = []
    ; clbits = []
    }
  ;
  value qv_swap_find it qv = QVMap.swap_find it.qvmap qv ;
  value cv_swap_find it cv = CVMap.swap_find it.cvmap cv ;
  value add_qbinding it qv qr = { (it) with qvmap = QVMap.add qv qr it.qvmap } ;
  value add_cbinding it cv cr = { (it) with cvmap = CVMap.add cv cr it.cvmap } ;
  value add_qbindings it l =
    let qm = QVMap.(union (fun _ _ newval -> Some newval) it.qvmap (ofList l)) in
    { (it) with qvmap = qm }
  ;
  value add_cbindings it l =
    let cm = CVMap.(union (fun _ _ newval -> Some newval) it.cvmap (ofList l)) in
    { (it) with cvmap = cm }
  ;
  value add_qubit it qb = { (it) with qubits = it.qubits @ [qb] } ;
  value add_clbit it cb = { (it) with clbits = it.clbits @ [cb] } ;

  value qubits env = env.qubits ;
  value clbits env = env.clbits ;
end ;

value rec binding genv env (loc, qvar_formals, cvar_formals, qc) =
  match qc with [
    QMEASURE _ qvar_actual ->
      if List.length qvar_formals <> 1 then
        Fmt.(raise_failwithf loc "AssignBits.binding: internal error: QMEASURE qvar formals/actuals length mismatch")
      else if List.length cvar_formals <> 1 then
        Fmt.(raise_failwithf loc "AssignBits.binding: internal error: QMEASURE cvar formals/qvar actuals length mismatch")
      else
        let qvar_formal = List.hd qvar_formals in
        let cvar_formal = List.hd cvar_formals in
        let cvar_result = CLBIT cvar_formal in
        let qvar_result = Env.qv_swap_find env qvar_actual in
        let env = Env.add_qbinding env qvar_formal qvar_result in
        let env = Env.add_cbinding env cvar_formal cvar_result in
        let env = Env.add_clbit env cvar_result in
        env

  | _ ->
     let (env, (qrl, crl)) = qcircuit0 genv env qc in
      if List.length qvar_formals <> List.length qrl then
        Fmt.(raise_failwithf loc "AssignBits.binding: internal error: qvar formals/actuals length mismatch")
      else if List.length cvar_formals <> List.length crl then
        Fmt.(raise_failwithf loc "AssignBits.binding: internal error: cvar formals/actuals length mismatch")
      else
        let qvar_additional_mapping = List.map2 (fun qformal qresult -> (qformal, qresult)) qvar_formals qrl in
        let cvar_additional_mapping = List.map2 (fun cformal cresult -> (cformal, cresult)) cvar_formals crl in
        let env = List.fold_left2 Env.add_qbinding env qvar_formals qrl in
        let env = List.fold_left2 Env.add_cbinding env cvar_formals crl in
        env
  ]

and qcircuit0 genv env qc =
  let rec arec qc = match qc with [
    QLET loc bl qc ->
      let env = List.fold_left (binding genv) env bl in
      qcircuit0 genv env qc

  | QWIRES _ qvl cvl ->
     (env, (List.map (Env.qv_swap_find env) qvl,List.map (Env.cv_swap_find env) cvl))

  | QGATEAPP loc gn pel qvar_actuals cvar_actuals ->
     let {args=(_, qvar_formals, cvar_formals); result=(gate_qresults, gate_cresults)} = GEnv.find_gate ~{loc=loc} genv gn in
     if List.length qvar_formals <> List.length qvar_actuals then
       Fmt.(raise_failwithf loc "AssignBits.qcircuit: internal error: qvar formals/actuals length mismatch")
     else if cvar_formals <> [] then
       Fmt.(raise_failwithf loc "AssignBits.qcircuit: internal error: cvar formals should be empty")
     else if cvar_actuals <> [] then
       Fmt.(raise_failwithf loc "AssignBits.qcircuit: internal error: cvar actuals should be empty")
     else
       (* (1) each actual is mapped to a bit by [qc]vmap.
          
          (2) gate_qresults is the list of formals in the order of results.
          
          (3) so to compute the result of the gate, take gate_qresults,
          map formal->actual, then map actual->bit.
          
        *)
       let q_formal2actual = Std.combine qvar_formals qvar_actuals |> QVMap.ofList in
       let qresults = gate_qresults |> List.map (QVMap.swap_find q_formal2actual) |> List.map (Env.qv_swap_find env) in
       (env, (qresults, []))

  | QCREATE _ bi ->
     let qb = QUBIT bi in
     let env = Env.add_qubit env qb in
     (env, ([qb], []))

  | QMEASURE loc _ ->
     Fmt.(raise_failwithf loc "AssignBits.qcircuit: QMEASURE found in non-let-binding context")

  | QBARRIER _ qvl ->
     (env, (List.map (Env.qv_swap_find env) qvl,[]))

  | QRESET _ qv ->
     (env, ([Env.qv_swap_find env qv],[]))

  | QBARRIER _ qvl ->
     (env, (List.map (Env.qv_swap_find env) qvl,[]))

  | QDISCARD _ _ -> (Env.empty, ([], []))
  ] in
  arec qc
;

value qcircuit genv env qc = qcircuit0 genv env qc ;

value gate_item genv gitem = match gitem with [
  DEF loc gn (((pvl, qvl, cvl) as glam), qc) ->
    let env = Env.empty in
    let env = Env.add_qbindings env (qvl |> List.map (fun qv -> (qv, QVAR qv))) in
    let env = Env.add_cbindings env (cvl |> List.map (fun cv -> (cv, CVAR cv))) in
    let (env, (qrl, crl)) = qcircuit genv env qc in
    if cvl <> [] then
      Fmt.(raise_failwithf loc "AssignBits.gate_item: internal error gate %a has input cvars %a"
             QG.pp_hum gn
             (list ~{sep=const string " "} CV.pp) cvl)
    else if crl <> [] then
      Fmt.(raise_failwithf loc "AssignBits.gate_item: internal error gate %a has output cvars %a"
             QG.pp_hum gn
             (list ~{sep=const string " "} pp_clbit_t) crl)
    else if Env.qubits env <> [] then
      Fmt.(raise_failwithf loc "AssignBits.gate_item: internal error gate %a allocates qubits (forbidden)!"
             QG.pp_hum gn)
    else if Env.clbits env <> [] then
      Fmt.(raise_failwithf loc "AssignBits.gate_item: internal error gate %a allocates clbits (forbidden)!"
             QG.pp_hum gn)
    else
    let qresults = qrl |> List.map (fun [
        QVAR qv -> qv
      | QUBIT _ -> 
         Fmt.(raise_failwithf loc "AssignBits.gate_item: gate %a create qubits (forbidden)" QG.pp_hum gn)
      ]) in
    let rv = (qresults, []) in
    ((env.Env.qvmap, env.Env.cvmap),  rv)

| OPAQUE loc gn ((pvl, qvl, cvl) as glam) ->
    if cvl <> [] then
      Fmt.(raise_failwithf loc "AssignBits.gate_item: internal error gate %a has input cvars %a"
             QG.pp_hum gn
             (list ~{sep=const string " "} CV.pp) cvl)
    else
    let rv = (qvl, []) in
    ((QVMap.empty, CVMap.empty), rv)
] ;

value upgrade_gate_item genv (prev_gitem, gitem) =
  let (gate_env,  result) = gate_item genv gitem in
  let res = prev_gitem.TYCHK.res in
  let glam = args_of_gate_item gitem in
  let rv = { args = glam ; ty = res ; gate_env = gate_env ; result = result } in
  rv
;

value upgrade_coupling_map x = x ;
value upgrade_layout x = x ;

value environ genv0 ?{env0=[]} env_items =
  GEnv.upgrade_environ
    ~{gate_item=upgrade_gate_item}
    ~{coupling_map=upgrade_coupling_map}
    ~{layout=upgrade_layout}
    genv0 (env0@env_items) ;

value program genv0 ?{env0=[]} (env_items, qc) =
  let genv = environ genv0 ~{env0=env0} env_items in
  let (env, _) = qcircuit genv Env.empty qc in
  (genv, env)
;

end ;
module AssignBits = AB ;

module Upgrade = struct

value upgrade_gate_item _ (prev,_) = prev ;
value upgrade_coupling_map x = CM.mk x ;
value upgrade_layout x = LO.mk x ;

value environ genv0 ?{env0=[]} env_items =
  GEnv.upgrade_environ
    ~{gate_item=upgrade_gate_item}
    ~{coupling_map=upgrade_coupling_map}
    ~{layout=upgrade_layout}
    genv0 (env0@env_items) ;

value program genv0 ?{env0=[]} (env_items, qc) =
  let genv = environ genv0 ~{env0=env0} env_items in
  (genv, (env_items, qc))
;
end ;

module Standard = struct
value program ?{env0=[]} (environ, qc) =
  let p = (environ,qc) in
  let p = Fresh.program p in
  let p = ANorm.program p in
  let p = NameNorm.program p in
  let (genv, ty) = TYCHK.program ~{env0=env0} p in
  let (genv, p) = Upgrade.program  ~{env0=env0} genv p in
  (genv, p)
;

end ;

module Latex = struct
(** Generate Latex for a quantum circuit.

    Method:

    (1) compute qubits/clbits the circuit uses

    (2) map each "logical qubit" (BI.t) and clbit to a "physical line"
    (which will be the line on the diagram)

    (3) compute the "Hoist"ed version of the circuit

    (4) for each layer in the hoisted circuit, break it into one or
    more sub-layers, such that no gates in a sub-layer overlap in
    their set of qubits.
    
    (5) the total # of layers to the circuit is #sub-layers



 *)
open Qc_latex ;

value binding_wire_range aenv (qubit2wire, clbit2wire) (loc, qvl, cvl, qc) =
  let (_, qvfvs, cvfvs) = circuit_freevars qc in
  let qvl = QVFVS.(toList (union qvfvs (ofList qvl))) in
  let cvl = CVFVS.(toList (union cvfvs (ofList cvl))) in
  let qubits = List.map (AB.Env.qv_swap_find aenv) qvl in
  let clbits = List.map (AB.Env.cv_swap_find aenv) cvl in
  let qwires = qubits |> List.map (AB.QUBMap.swap_find qubit2wire) |> List.map snd in
  let cwires = clbits |> List.map (AB.CLBMap.swap_find clbit2wire) |> List.map snd in
  let all_wires = qwires@cwires in
  let min_wire = List.fold_left min max_int all_wires in
  let max_wire = List.fold_left max min_int all_wires in
  (min_wire, max_wire)
;

value rec layers sbr = match sbr with [
  [] -> []
| [b :: t] ->
   let rec sepchain (rev_chain, rev_rest) = fun [
         [] -> (List.rev rev_chain, List.rev rev_rest)
       | [b1::t] ->
          let b0 = List.hd rev_chain in
          let ((_, b0_max), _) = b0 in
          let ((b1_min, _), _) = b1 in
          if b0_max < b1_min then sepchain ([b1::rev_chain], rev_rest) t
          else sepchain (rev_chain,  [b1::rev_rest]) t
       ] in
   let (chain, rest) = sepchain ([b], []) t in
         [chain:: layers rest]
] ;

value render_binding m (qc_assign_env, qubit2wire, clbit2wire) col (loc, qvl, cvl, qc) =
  match qc with [
      QCREATE loc _ -> Fmt.(raise_failwithf loc "render_binding: internal error: QCREATE should never occur here")
    | QWIRES loc _ _ -> Fmt.(raise_failwithf loc "render_binding: internal error: QWIRES should never occur here")
    | QLET loc _ _ -> Fmt.(raise_failwithf loc "render_binding: internal error: QLET should never occur here")
    | QMEASURE loc _ ->
       let qv = List.hd qvl in
       let cv = List.hd cvl in
       let qubit = AB.Env.qv_swap_find qc_assign_env qv in
       let clbit = AB.Env.cv_swap_find qc_assign_env cv in
       let (quwirename, quwirenum) = AB.QUBMap.swap_find qubit2wire qubit in
       let (clwirename, clwirenum) = AB.CLBMap.swap_find clbit2wire clbit in
       let cdstick = ME.DSTICK clwirename (quwirenum-clwirenum) in do {
        Matrix.set m quwirenum col METER ;
        Matrix.set m clwirenum col cdstick 
      }
    | QRESET _ qv ->
       let qubit = AB.Env.qv_swap_find qc_assign_env qv in
       let (_, quwirenum) = AB.QUBMap.swap_find qubit2wire qubit in
       Matrix.set m quwirenum col (GATE {x|\mathrm{\left|0\right\rangle}|x})
    | QBARRIER _ qvl ->
       let qubits = List.map (AB.Env.qv_swap_find qc_assign_env) qvl in
       let sorted_qwires =
         qubits
         |>  List.map (AB.QUBMap.swap_find qubit2wire)
         |> List.map snd 
         |> List.sort Stdlib.compare in
       let wire_intervals = collapse_intervals sorted_qwires in
       wire_intervals
       |> List.iter (fun (i,j) ->
              Matrix.set m i col (BARRIER (j-i))
            )

    | QGATEAPP loc gn _ [qv] [] ->
       let qubit = AB.Env.qv_swap_find qc_assign_env qv in
       let (_, quwirenum) = AB.QUBMap.swap_find qubit2wire qubit in
       Matrix.set m quwirenum col (GATE Fmt.(str {x|\mathrm{%a}|x} SYN.QG.pp_hum gn))

    | QGATEAPP loc ((SYN.CX _|SYN.GENGATE _ ("cx",-1)) as gn) _ [ctrl_qv;targ_qv] [] ->
       let ctrl_qubit = AB.Env.qv_swap_find qc_assign_env ctrl_qv in
       let targ_qubit = AB.Env.qv_swap_find qc_assign_env targ_qv in
       let (_, ctrl_quwire) = AB.QUBMap.swap_find qubit2wire ctrl_qubit in
       let (_, targ_quwire) = AB.QUBMap.swap_find qubit2wire targ_qubit in do {
        Matrix.set m ctrl_quwire col (CTRL (targ_quwire - ctrl_quwire)) ;
        Matrix.set m targ_quwire col TARG
      }

    | QGATEAPP loc ((SYN.SWAP _|SYN.GENGATE _ ("swap",-1)) as gn) _ [qv1;qv2] [] ->
       let qubit1 = AB.Env.qv_swap_find qc_assign_env qv1 in
       let qubit2 = AB.Env.qv_swap_find qc_assign_env qv2 in
       let (_, quwire1) = AB.QUBMap.swap_find qubit2wire qubit1 in
       let (_, quwire2) = AB.QUBMap.swap_find qubit2wire qubit2 in do {
        Matrix.set m quwire1 col (SWAP None) ;
        Matrix.set m quwire2 col (SWAP (Some (-(quwire2-quwire1))))
      }

    | QGATEAPP loc gn _ _ _ ->
       Fmt.(raise_failwithf loc "render_binding: unhandled gate %a" SYN.QG.pp_hum gn)
    ]
;
value render_bindings m (qc_assign_env, qubit2wire, clbit2wire) ll =
  ll |> List.iteri (fun layernum bl ->
      let bl = List.map snd bl in
      List.iter (render_binding m (qc_assign_env, qubit2wire, clbit2wire) (3+layernum)) bl)
;

(** remove_bit_overlap:

    (1) for eaching binding, compute [min,max] wire-numbers
    (2) sort by min-wire-number
    (3) repeatedly, until the list is empty:
        (4) select first binding, and find the next non-overlapping binding, keep going, removing all these bindings
        (5) this removed list of non-overlapping bindings is a new layer
        (6) and the remaining bindings are the list that needs to be further processed.
 *)
value remove_bit_overlap aenv (qubit2wire, clbit2wire) (loc, bl) =
  let bl_ranges = List.map (fun b -> (binding_wire_range aenv (qubit2wire, clbit2wire) b, b)) bl in
  let sorted_bl_ranges = List.sort (fun ((a, _), _) ((b, _), _) -> Stdlib.compare a b) bl_ranges in
  layers sorted_bl_ranges
;

(** fixup_barriers

    It seems that a barrier in column i+1 needs to be moved to column
    i, and a QW in column i+1.  This code does that.
 *)
value fixup_barriers m =
  let fixlist l =
    let rec frec = fun [
          [m; ((ME.BARRIER _) as n) :: t] -> [ME.L[m;n] :: frec [QW :: t]]
        | [m :: t] -> [m :: frec t]
        | [] -> []
        ] in
    frec l in
  let open Matrix in
  { (m) with it = Array.map (fun a -> a |> Array.to_list |> fixlist |> Array.of_list) m.it }
;
(** generate_matrix

    Generates the Matrix.t from this circuit.

    (1) set up all left-hand-side labels

    (2) set up classical wires

    (3) check that first layer is all qubits (what else could it be?)

    (4) Then for every ith-numbered layer, render each binding

 *)
value generate_matrix (num_qubits, num_clbits) (qc_assign_env, qubit2wire, clbit2wire) (ll, qc) =
  let num_bits = num_qubits + num_clbits in
  let m = Matrix.mk num_bits (2 + (List.length ll) + 2) in do {
  qubit2wire
  |> AB.QUBMap.iter (fun _ (name, num) -> do {
                       let txt = (Fmt.str {x|%s :  |x} name) in
                       Matrix.set m num 0 (NGHOST txt) ;
                       Matrix.set m num 1 (LSTICK(Some txt)) ;
       }) ;
  clbit2wire
  |> AB.CLBMap.iter (fun _ (name, num) -> do {
                    Matrix.set_row m num CW ;
                    let txt = (Fmt.str {x|\mathrm{{%s} :  }|x} name) in
                    Matrix.set m num 0 (NGHOST txt) ;
                    Matrix.set m num 1 (LSTICK (Some txt)) ;
                    Matrix.set m num 2 (CWIDTH 1) ;
       }) ;
  assert (
      let bl = List.map snd (List.hd ll) in
      bl |> List.for_all (fun [ (_, _, _, SYN.QCREATE _ _) -> True | _ -> False ])) ;
  render_bindings m (qc_assign_env, qubit2wire, clbit2wire) (List.tl ll) ;
  m |> fixup_barriers
  }
;

(** latex is given a circuit and possibly a mapping from qubits/clbits to wire-names/numbers.

    For this mapping to be valid:

    (1) bits found in the circuit (as found by AssignBits) must be in the maps.
    (2) the wire-numbers need to be all distinct.
    (3) the sorted set of wire-numbers must fill some interval starting at zero.

    NOTE WELL: that this means there might be more wires than actually-used bits.

    (4) All gates must have the property that the result-qubits are in
    the same order as the argument qubits, and there are no
    result-clbits (of course).

    --> This is necessary to be able to draw them *at all*, and simply
    derives from the gates being defined in QASM.

    --> it also follows that all gate-instances in the circuit have
    the same property.

    wire-labels will be the pp_hum of bits.

    Next, we build the "layers" of the circuit (using hoist_creates
    and then to_letlist), and then further partition those layers so
    that each layer contains only non-overlapping gates/operations.

    Each binding applies to some wires, and the qwires are of course
    present as both inputs and outputs.  Classical wires will only be
    present as outputs.

    So for each layer, decide what each wire's slot will have as a
    value: the default being a bare wire.

 *)

value latexable_gate gn ge =
  let (_,  qvl, _) = ge.AB.args in
  let (qrl, crl) = ge.AB.result in
  crl = [] &&
    List.for_all2 QV.equal qvl qrl
;

value is_create_binding b = match qbinding_qc b with [ QCREATE _ _ -> True | _ -> False ] ;
value hoist_creates qc =
  let acc = ref [] in
  let rec hrec = fun [
    QLET loc bl qc ->
      let (create_bl, rest_bl) = bl |> filter_split is_create_binding in do {
        List.iter (Std.push acc) create_bl ;
        let qc = hrec qc in
        if rest_bl = [] then qc else QLET loc rest_bl qc
      }
  | qc -> qc
  ] in
  let loc = loc_of_qcirc qc in
  let qc = hrec qc in
  QLET loc (List.rev acc.val) qc
;

value latex genv0 ?{env0=[]} ?{qubit2wire} ?{clbit2wire} (envitems, qc) =
  let (gate_assign_env, qc_assign_env) = AB.program genv0 ~{env0=env0} (envitems, qc) in
  let _ = assert (QGMap.for_all latexable_gate gate_assign_env.GEnv.gates) in
  let qubits = AB.Env.qubits qc_assign_env in
  let num_qubits = List.length qubits in
  let clbits = AB.Env.clbits qc_assign_env in
  let num_clbits = List.length clbits in
  let num_bits = num_qubits + num_clbits in
  let qubit2wire = match qubit2wire with [
        Some m -> m
      | None ->
         qubits
         |> List.mapi (fun i qb -> (qb,(Fmt.(str "q_%d" i), i)))
         |> AB.QUBMap.ofList
      ] in
  let clbit2wire = match clbit2wire with [
        Some m -> m
      | None ->
         clbits
         |> List.mapi (fun i cb -> (cb,(Fmt.(str "c_%d" i), i+num_qubits)))
         |> AB.CLBMap.ofList
      ] in
  if not AB.(QUBSet.(subset (ofList qubits) (QUBMap.dom qubit2wire))) then
    Fmt.(failwithf "Latex.latex: qubits of circuit not subset of provided qubit2wire : %a != %a"
           AB.QUBSet.pp_hum (AB.QUBSet.ofList qubits) AB.QUBSet.pp_hum (AB.QUBMap.dom qubit2wire))
  else if not AB.(CLBSet.(subset (ofList clbits) (CLBMap.dom clbit2wire))) then
    Fmt.(failwithf "Latex.latex: clbits of circuit not subset of provided clbit2wire : %a != %a"
           AB.CLBSet.pp_hum (AB.CLBSet.ofList clbits) AB.CLBSet.pp_hum (AB.CLBMap.dom clbit2wire))
  else
  let wirenames_nums = (AB.QUBMap.rng qubit2wire)@(AB.CLBMap.rng clbit2wire) in
  let wirenums = List.map snd wirenames_nums in
  if wirenums = [] then
    Fmt.(failwithf "Latex.latex: no wires specified")
  else if not (Std.distinct wirenums) then
    Fmt.(failwithf "Latex.latex: repeated wires are forbidden: %a"
           (list ~{sep=const string " "} int) (Std2.hash_list_repeats wirenums))
  else
  let sorted_wirenums = List.sort Stdlib.compare wirenums in
  if 0 <> List.hd sorted_wirenums then
    Fmt.(failwithf "lowest-numbered wire must be 0")
  else if not (consecutive_ints sorted_wirenums) then
    Fmt.(failwithf "wires do not consecutively fill an interval: %a"
           (list ~{sep=const string " "} int) sorted_wirenums)
  else
  let out_of_range = wirenums |> List.filter (fun n -> not (0 <= n && n < num_bits)) in
  if out_of_range <> [] then
    Fmt.(failwithf "Latex.latex: wires out of range [0,%d): %a" num_bits
           (list ~{sep=const string " "} int) out_of_range)
  else
  let qc = hoist_creates qc in
  let (ll, qc) = SYN.to_letlist qc in
  let ll = ll |> List.concat_map (remove_bit_overlap qc_assign_env (qubit2wire, clbit2wire)) in
  ((ll,qc),  generate_matrix (num_qubits, num_clbits) (qc_assign_env, qubit2wire, clbit2wire) (ll, qc))
;

end ;

module BasicSwap = struct
(** BasicSwap

    argument:

    coupling map
    layout for qubits (assumes # qubits)
    circuit

    produces a circuit from the input, by inserting swaps in order to
    obey the constraints of the coupling-map.


    Method:

    (1) hoist the circuit
    (2) compute let-list

    (3) for each binding in each layer:

        (4) if it's a single-bit gate, just pass it
        (5) if it's not a CX/cx, fail
        (6) for CX/cx, if it's realizable with the coupling-map, pass it

        (7) otherwise, find a path between the two endpoints, and add
        swaps to move the lower-numbered endpoint up to be next to the
        higher-numbered endpoint.

 *)

value logical_to_explicit_qubit loc q =
  match q with [
      AB.QUBIT bi -> bi
    | _ ->
       Fmt.(raise_failwithf loc "layout_sensitive_binding: logical qubit was not explicitly named (all must be to apply BasicSwap): %a"
              AB.QUBit.pp_hum q)
    ]
;

value make_h loc qv = QGATEAPP loc (QG.ofID (ID.mk "h")) [] [qv] [] ;
value make_cx loc c t = QGATEAPP loc (QG.ofID (ID.mk "CX")) [] [c; t] [] ;

value update_layout aenv l (loc, qvl, _, qc) = match qc with [
      QGATEAPP loc (SYN.SWAP _) _ [qv1;qv2] [] ->
       let qubit1 = AB.Env.qv_swap_find aenv qv1 in
       let lqubit1 = logical_to_explicit_qubit loc qubit1 in
       let qubit2 = AB.Env.qv_swap_find aenv qv2 in
       let lqubit2 = logical_to_explicit_qubit loc qubit2 in
       LO.swap l (lqubit1, lqubit2) 
    | _ -> l
] ;

value layout_swap aenv cm l loc (qv1, qv2) =
  let b = (loc,  [qv2;qv1], [], QGATEAPP loc (SYN.SWAP loc) [] [qv1;qv2] []) in
  let l = update_layout aenv l b in
  ((loc, [b]), l)
;

value rec layout_sensitive_cx aenv cm (l, logical2qvar) ((loc, qvl,  cvl,  qc) as b) =
  let (ctrl_qv,targ_qv) = 
  match qc with [
      QGATEAPP loc ((SYN.CX _|SYN.GENGATE _ ("cx",-1)) as gn) _ [ctrl_qv;targ_qv] [] -> (ctrl_qv,targ_qv)
    ] in
  let ctrl_qubit = AB.Env.qv_swap_find aenv ctrl_qv in
  let ctrl_lqubit = logical_to_explicit_qubit loc ctrl_qubit in
  let targ_qubit = AB.Env.qv_swap_find aenv targ_qv in
  let targ_lqubit = logical_to_explicit_qubit loc targ_qubit in

  if LO.has_logical_pair l cm ctrl_lqubit targ_lqubit then
    ([(loc, [b])],l)
  else if LO.has_logical_pair l cm targ_lqubit ctrl_lqubit then
    (* convert to:
       let ctrl_qv = H ctrl_qv and targ_qv = H targ_qv in
       let (targ_qv, ctrl_qv) = cx targ_qv ctrl_qv in
       let ctrl_qv = H ctrl_qv and targ_qv = H targ_qv in
       let qvl = (ctrl_qv, targ_qv) in
       ...
     *)
    ([(loc, [(loc, [ctrl_qv], [], make_h loc ctrl_qv); (loc, [targ_qv], [], make_h loc targ_qv)]);
      (loc, [(loc, [targ_qv; ctrl_qv], [], make_cx loc targ_qv ctrl_qv)]);
      (loc, [(loc, [ctrl_qv], [], make_h loc ctrl_qv); (loc, [targ_qv], [], make_h loc targ_qv)]) ;
      (loc, [(loc, qvl, [],  QWIRES loc [ctrl_qv; targ_qv] [])])
     ], l)
  else if not (LO.has_logical_path l cm ctrl_lqubit targ_lqubit) then
    Fmt.(raise_failwithf loc "BasicSwap.process_binding: no path logical qubits %a -> %a"
           SYN.BI.pp_hum ctrl_lqubit SYN.BI.pp_hum targ_lqubit)
  else
    let logical_path = LO.logical_path l cm ctrl_lqubit targ_lqubit in
    let _ = assert (List.length logical_path > 2) in
    let qvpath = List.map (BIMap.swap_find logical2qvar) logical_path in
    let (qv1,qv2) = match qvpath with [ [qv1;qv2 :: _] -> (qv1,qv2) | _ -> assert False ] in
    let _ = assert (QV.equal qv1 ctrl_qv) in
    let (swap_bl, l) = layout_swap aenv cm l loc (qv1, qv2) in
    let (rest_bll, l) = layout_sensitive_cx aenv cm (l, logical2qvar) b in
    ([swap_bl :: rest_bll], l)
;

value layout_sensitive_binding aenv cm (l, logical2qvar) ((loc, qvl,  cvl,  qc) as b) =
  match qc with [
      QWIRES loc _ _ -> Fmt.(raise_failwithf loc "layout_sensitive_binding: internal error: QWIRES should never occur here")
    | QLET loc _ _ -> Fmt.(raise_failwithf loc "layout_sensitive_binding: internal error: QLET should never occur here")
    | (QMEASURE _ _ | QRESET _ _ | QBARRIER _ _) -> ([(loc, [b])], l)
    (* all single-qubit gates are fine *)
    | QCREATE loc _ -> ([(loc, [b])], l)
    | QGATEAPP _ _ _ [_] [] -> ([(loc, [b])], l)

    | QGATEAPP loc ((SYN.CX _|SYN.GENGATE _ ("cx",-1)) as gn) _ [ctrl_qv;targ_qv] [] ->
       layout_sensitive_cx aenv cm (l, logical2qvar) b 

    | QGATEAPP loc gn _ _ _ ->
       Fmt.(raise_failwithf loc "layout_sensitive_binding: unhandled gate %a" SYN.QG.pp_hum gn)
    ] ;

value process_binding aenv cm (l, logical2qvar) b =
  let (bll, l) =  layout_sensitive_binding aenv cm (l, logical2qvar) b in
  let  qvar2logical loc qv =
    let qubit = AB.Env.qv_swap_find aenv qv in
    logical_to_explicit_qubit loc qubit in
  let logical2qvar =
    List.fold_left (fun logical2qvar (_, bl) ->
        List.fold_left (fun logical2qvar (loc, qvl, _, _) ->
            List.fold_left (fun logical2qvar qv ->
                let bi = qvar2logical loc qv in
                BIMap.add bi qv logical2qvar)
              logical2qvar qvl)
          logical2qvar bl)
      logical2qvar bll in
  ((l, logical2qvar), bll)
;

value process_bindings aenv cm (l, logical2qvar) bl =
  let process1 = process_binding aenv cm in
  let rec prec (l, logical2qvar) = fun [
        [] -> ((l, logical2qvar), [])
      | [b :: bl] ->
         let ((l, logical2qvar), b_res) = process1 (l, logical2qvar) b in
         let ((l, logical2qvar),bl_res) = prec (l, logical2qvar) bl in
         ((l, logical2qvar), b_res @ bl_res)
      ] in
  prec (l, logical2qvar) bl
;

value basic_swap genv0 ?{env0=[]} ~{coupling_map} ~{layout=l} (envitems,qc) =
  let (gate_assign_env, qc_assign_env) = AB.program genv0 ~{env0=env0} (envitems, qc) in
  let qc = Hoist.hoist qc in
  let (ll, qc) = SYN.to_letlist qc in
  let logical2qvar = BIMap.empty in
  let (_,  rev_ll) =
    List.fold_left (fun ((l, logical2qvar), acc_rev_ll) (loc, bl) ->
        let ((l, logicalqvar), ll) = process_bindings qc_assign_env coupling_map (l, logical2qvar) bl in
        ((l, logicalqvar), [ll :: acc_rev_ll])
      )
      ((l,  logical2qvar), []) ll in
  let ll = List.concat (List.rev rev_ll) in
  let qc = SYN.of_letlist (ll, qc) in
  (envitems, Fresh.qcircuit qc)
;

end ;
