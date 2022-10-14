open Qc_misc ;

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
  value intersect : t -> t -> t ;
  value subtract : t -> t -> t ;
  value ofList : list M.t -> t ;
  value toList : t -> list M.t ;
  value union : t -> t -> t ;
  value concat : list t -> t ;
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
  module S = Set.Make(M) ;
  type t = S.t ;
  value mt = S.empty ;
  value mem s x = S.mem x s ;
  value add s x = S.add x s ;
  value intersect l1 l2 = S.inter l1 l2 ;
  value subtract l1 l2 = S.diff l1 l2 ;
  value ofList l = S.of_list l ;
  value toList l = S.elements l ;
  value union l1 l2 = S.union l1 l2 ;
  value concat l = List.fold_left union mt l ;
  value fresh l x =
    let (s,_) = M.toID x in
    let n = S.fold (fun v acc ->
      let (s', m) = M.toID v in
      if s <> s' then acc else max acc m)
          l Int.min_int in
    if n = Int.min_int then
      M.ofID (s,-1)
    else M.ofID (s, n+1) ;

  value equal l1 l2 = S.equal l1 l2 ;

  type _t = list M.t[@@deriving (show);] ;
  value pp pps l = pp__t pps (toList l) ;
  value show l = show__t (toList l) ;
  value pp_hum pps l = Fmt.(pf pps "%a" (brackets (list ~{sep=const string "; "} M.pp_hum)) (toList l)) ;
end ;
