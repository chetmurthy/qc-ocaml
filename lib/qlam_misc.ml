open Qc_misc ;
open Pa_ppx_base ;
open Ppxutil ;

module type ENTITY_SIG = sig
  type t = 'a[@@deriving (eq, ord, show);];
  value pp_hum : Fmt.t t ;
end ;
module type VARSIG = sig
  include ENTITY_SIG ;
  value toID : t -> ID.t ;
  value ofID : ?loc:loc -> ID.t -> t ;
end ;
module type SETSIG = sig
  module M : ENTITY_SIG ;
  module S : (Set.S with type elt = M.t) ;
  type t = S.t [@@deriving (show);] ;
  value mt : t ;
  value add : t -> M.t -> t ;
  value mem : t -> M.t -> bool ;
  value intersect : t -> t -> t ;
  value subtract : t -> t -> t ;
  value ofList : list M.t -> t ;
  value toList : t -> list M.t ;
  value union : t -> t -> t ;
  value concat : list t -> t ;
  value equal : t -> t -> bool ;
  value pp_hum : Fmt.t t ;
end ;
module type VARSETSIG = sig
  module M : VARSIG ;
  include (SETSIG with module M := M) ;
  value fresh : t -> M.t -> M.t ;
end ;

module type MAPSIG = sig
  module M : ENTITY_SIG ;
  include Map.S with type key = M.t ;
  value pp_hum : Fmt.t 'a -> Fmt.t (t 'a) ;
  value ofList : list (M.t * 'a) -> t 'a ;
  value toList : t 'a -> list (M.t * 'a) ;
end ;

module EntityMap(M : ENTITY_SIG) : (MAPSIG with module M = M) = struct
  module M = M ;
  module IT = Map.Make(M) ;
  include IT ;
  value pp_hum ppval pps l =
    Fmt.(pf pps "%a" (list ~{sep=const string "; "} (parens (pair ~{sep=const string ", "} M.pp_hum ppval))) (bindings l))
  ;
  value toList = bindings ;
  value ofList l =
    List.fold_left (fun m (k,v) -> add k v m) empty l ;
end ;

module EntitySet(M : ENTITY_SIG) : (SETSIG with module M = M) = struct
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
  value equal l1 l2 = S.equal l1 l2 ;

  type _t = list M.t[@@deriving (show);] ;
  value pp pps l = pp__t pps (toList l) ;
  value show l = show__t (toList l) ;
  value pp_hum pps l = Fmt.(pf pps "%a" (brackets (list ~{sep=const string "; "} M.pp_hum)) (toList l)) ;
end ;

module VarSet(M0 : VARSIG) : (VARSETSIG with module M = M0) = struct
  module S0 = EntitySet(M0) ;
  include S0 ;
  module M = M0 ;
  value fresh l x =
    let (s,_) = M.toID x in
    let n = S.fold (fun v acc ->
      let (s', m) = M.toID v in
      if s <> s' then acc else max acc m)
          l Int.min_int in
    if n = Int.min_int then
      M.ofID (s,-1)
    else M.ofID (s, n+1) ;
end ;

module type BIJSIG = sig
  module DOM : ENTITY_SIG ;
  module RNG : ENTITY_SIG ;
  module DOMMap : (Map.S with type key = DOM.t) ;
  module RNGMap : (Map.S with type key = RNG.t) ;

  include (Misc_map.MonoMapS with type key = DOM.t and type rng = RNG.t) ;

  value insert : DOM.t -> RNG.t -> t -> t ;

  value find_rng : RNG.t -> t -> DOM.t ;
  value mem_rng : RNG.t -> t -> bool ;

  value pp_hum : Fmt.t t ;
end ;

module Bijection(M1 : ENTITY_SIG) (M2 : ENTITY_SIG) : (BIJSIG with module DOM = M1 and module RNG = M2) = struct
  module DOM = M1 ;
  module RNG = M2 ;
  module DOMMap = Map.Make(M1) ;
  module RNGMap = Map.Make(M2) ;
  type key = DOM.t ;
  type rng = RNG.t ;
  type t = (DOMMap.t RNG.t * RNGMap.t DOM.t);

  value empty    = (DOMMap.empty,RNGMap.empty) ;
  value is_empty (l, _)   = DOMMap.is_empty l;
  value mem x (l, _)   = DOMMap.mem x l;
  value mem_rng x (_, r)   = RNGMap.mem x r;
  value insert x y ((l,r) as m) = do {
    if mem x m then
      Fmt.(failwithf "Bijection.insert: %a already in domain" DOM.pp_hum x)
    else if mem_rng y m then
      Fmt.(failwithf "Bijection.insert: %a already in range" RNG.pp_hum y)
    else
      (DOMMap.add x y l,  RNGMap.add y x r)
  } ;
  value remove x (l,r) =
    let y = DOMMap.find x l in
    (DOMMap.remove x l, RNGMap.remove y r)
  ;
  value bindings (l, _) = DOMMap.bindings l ;
  value find x (l, _) = DOMMap.find x l ;
  value find_rng y (_, r) = RNGMap.find y r ;

  value pp_hum pps (l,_) =
    Fmt.(pf pps "%a" (list ~{sep=const string "; "} (parens (pair ~{sep=const string ", "} DOM.pp_hum RNG.pp_hum))) (DOMMap.bindings l))
  ;
end ;

module IDMap = EntityMap(ID) ;
module IDFVS = VarSet(ID) ;
module IntMap = EntityMap(struct
  type t = int[@@deriving (to_yojson, show, eq, ord);] ;
  value pp_hum = pp ;
end) ;
