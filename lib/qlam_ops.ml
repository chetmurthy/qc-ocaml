
open Pa_ppx_utils ;
open Pa_ppx_base ;
open Ppxutil ;
open Qc_misc ;
open Qlam_syntax ;

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
      | QBIT _ -> (PVFVS.mt, QVFVS.mt, CVFVS.mt)
      | QDISCARD _ qvl -> (PVFVS.mt,  QVFVS.ofList qvl,  CVFVS.mt)
      | QMEASURE _ qvl -> (PVFVS.mt, QVFVS.ofList qvl, CVFVS.mt)
      | QRESET _ qvl -> (PVFVS.mt, QVFVS.ofList qvl, CVFVS.mt)
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

value max_id_index qc =
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
  | (QBIT _) as qc -> qc
  | QDISCARD loc qvl -> QDISCARD loc (List.map subst_qvar qvl)
  | QMEASURE loc qvl -> QMEASURE loc (List.map subst_qvar qvl)
  | QRESET loc qvl -> QRESET loc (List.map subst_qvar qvl)
  ] in
  substrec qc
;

value qcircuit ~{counter} genv = fun [
  QGATEAPP loc gn pel qel cel ->
  let ((pvl,qvl, cvl), qc) = 
    match QGMap.find gn genv with [
      exception Not_found -> Fmt.(raise_failwithf loc "BetaReduce.qcircuit: gate %a not found" QG.pp_hum gn)
    | x -> Fresh.qgatelam ~{counter=counter} x ] in
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

| x -> Fmt.(raise_failwithf (loc_of_qcirc x) "BetaReduce: can only be applied to gate-application, not %a" PP.qcirc x)
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

value anorm qc =
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
  | QBIT _ -> (qc, FVS.mt)
  | QDISCARD _ qvl -> (qc, FVS.(of_ids (qvl, [])))
  | QMEASURE _ qvl -> (qc, FVS.(of_ids (qvl, [])))
  | QRESET _ qvl -> (qc, FVS.(of_ids (qvl, [])))
  ] in
  let (qc, _) = anrec qc in
  qc
;

end ;

module Env = struct
type t = QGMap.t qgatelam_t ;

value mk ?{only} ?{except} env =
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
      QGATE _ (DEF gn x) -> if accept gn then QGMap.add gn x m else m
    | QGATE _ (OPAQUE _ _) -> m
    | QINCLUDE _ _ _ l ->
       List.fold_left flatrec m l
    ] in
  List.fold_left flatrec QGMap.empty env
;

end ;

module Unroll = struct

(** unroll: takes a map [gatename -> gatelam] and a qcircuit,  unrolls all gates possible.

    A gate that is not in the map, is not unrolled.  So the selection of the map completely 
    determines what is unrolled.
 *)

value unroll (genv : Env.t) qc =
  let counter = Fresh.(mk ~{base=1 + max_id_index qc} ()) in
  let qc = Fresh.qcircuit ~{counter=counter} ~{qvmap=QVMap.empty} ~{cvmap=CVMap.empty} qc in
  let rec unrec qc = match qc with [
    QLET loc bl qc ->
    QLET loc (List.map (fun (loc, qvl, cvl, qc) -> (loc, qvl, cvl, unrec qc)) bl) (unrec qc)

  | QGATEAPP _ gn _ _ _ when QGMap.mem gn genv ->
     let qc = BetaReduce.qcircuit ~{counter=counter} genv qc in
     unrec qc

  | (QGATEAPP _ _ _ _ _
     | QWIRES _ _ _
    | QBARRIER _ _
    | QBIT _ | QDISCARD _ _
    | QMEASURE _ _
    | QRESET _ _) -> qc
  ]
  in unrec qc
;

end ;

(** TODO

(1) alpha-equality

(2) raise_fresh

(3) beta-reduce

(4) unroll

(5) A-normalize


 *)

