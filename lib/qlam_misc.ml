open Qc_misc ;
open Pa_ppx_base ;
open Ppxutil ;

module type PP_HUM_SIG = sig
  type t = 'a ;
  value pp_hum : Fmt.t t ;
end ;

module type PP_HUM_POLY1_SIG = sig
  type t 'b = 'a ;
  value pp_hum : Fmt.t 'b -> Fmt.t (t 'b) ;
end ;

module type ORDERED_TYPE_WITH_PP = sig
  type t = 'a [@@deriving (to_yojson, eq, ord, show);] ;
  include (Set.OrderedType with type t := t) ;
  include (PP_HUM_SIG with type t := t) ;
end ;

module type SET_WITH_PP = sig
  type elt = 'a [@@deriving (to_yojson, eq, ord, show);] ;
  type t = 'a [@@deriving (to_yojson, eq, ord, show);] ;
  include (Set.S with type elt := elt and type t := t) ;
  include (PP_HUM_SIG with type t := t) ;
end ;

module type MAP_WITH_PP = sig
  type key = 'a [@@deriving (to_yojson, eq, ord, show);] ;
  type t !+'a = 'b [@@deriving (to_yojson, eq, ord, show);] ;
  include (Map.S with type key := key and type t 'a := t 'a) ;
  include (PP_HUM_POLY1_SIG with type t 'a := t 'a) ;
end ;

module type ENTITY_SIG = sig
  type t = 'a [@@deriving (to_yojson, eq, ord, show);];
  include (ORDERED_TYPE_WITH_PP with type t := t) ;
  include (PP_HUM_SIG with type t := t) ;
end ;

module type VARSIG = sig
  include ENTITY_SIG ;
  value toID : t -> ID.t ;
  value ofID : ?loc:loc -> ID.t -> t ;
end ;
module type SETSIG = sig
  module M : ENTITY_SIG ;
  module S : (SET_WITH_PP with type elt = M.t) ;
  type t = S.t [@@deriving (to_yojson, eq, ord, show);] ;
  include (PP_HUM_SIG with type t := t) ;
  value mt : t ;
  value add : t -> M.t -> t ;
  value mem : t -> M.t -> bool ;
  value intersect : t -> t -> t ;
  value subtract : t -> t -> t ;
  value ofList : list M.t -> t ;
  value toList : t -> list M.t ;
  value distinct : list M.t -> bool ;
  value union : t -> t -> t ;
  value subset : t -> t -> bool ;
  value concat : list t -> t ;
  value equal : t -> t -> bool ;
end ;
module type VARSETSIG = sig
  module M : VARSIG ;
  include (SETSIG with module M := M) ;
  value fresh : t -> M.t -> M.t ;
end ;

module type MAPSIG = sig
  module M : ENTITY_SIG ;
  module S : (SETSIG with module M = M) ;
  include MAP_WITH_PP with type key = M.t ;
  value swap_find: t 'a -> key -> 'a ;
  value pp_hum : Fmt.t 'a -> Fmt.t (t 'a) ;
  value ofList : list (M.t * 'a) -> t 'a ;
  value toList : t 'a -> list (M.t * 'a) ;
  value dom : t 'a -> S.t ;
  value rng : t 'a -> list 'a ;
end ;

module MapWithPP(M : ORDERED_TYPE_WITH_PP) : (MAP_WITH_PP with type key = M.t) = struct
  module S = Map.Make(M) ;
  value pp_key = M.pp ;
  value show_key = M.show ;
  value equal_key = M.equal ;
  value compare_key = M.compare ;
  value key_to_yojson = M.to_yojson ;
  type _t 'a = list (M.t * 'a) [@@deriving (to_yojson, eq, ord, show);] ;
  value pp ppval pps m = pp__t ppval pps (S.bindings m) ;
  value show ppval m = show__t ppval (S.bindings m) ;
  value to_yojson val_to_yojson m = _t_to_yojson val_to_yojson (S.bindings m) ;
  value pp_hum ppval_hum pps m =
    Fmt.(pf pps "%a" (list ~{sep=const string ", "} (pair ~{sep=const string " : "} M.pp_hum ppval_hum)) (S.bindings m))
  ;
  include S ;
end ;

module EntityMap(M : ENTITY_SIG)(S : SETSIG with module M = M) : (MAPSIG with module M = M and module S = S) = struct
  module M = M ;
  module S = S ;
  module IT = MapWithPP(M) ;
  include IT ;
  value pp_key = M.pp ;
  value show_key = M.show ;
  value compare_key = M.compare ;
  value equal_key = M.equal ;
  value key_to_yojson = M.to_yojson ;
  value breaking s pps () = Fmt.(pf pps "%s@," s) ;
  value pp_hum ppval pps l =
    Fmt.(pf pps "%a" (list ~{sep=breaking ";"} (parens (pair ~{sep=const string ", "} M.pp_hum ppval))) (bindings l))
  ;
  value swap_find m k = find k m ;
  value toList = bindings ;
  value ofList l =
    List.fold_left (fun m (k,v) -> add k v m) empty l ;
  value dom m = m |> toList |> List.map fst |> S.ofList ;
  value rng m = m |> toList |> List.map snd ;
end ;

module SetWithPP(M : ORDERED_TYPE_WITH_PP) : (SET_WITH_PP with type elt = M.t) = struct
  module S = Set.Make(M) ;
  value pp_elt = M.pp ;
  value show_elt = M.show ;
  value compare_elt = M.compare ;
  value equal_elt = M.equal ;
  value elt_to_yojson = M.to_yojson ;
  type _t = list M.t [@@deriving (to_yojson, eq, ord, show);] ;
  value pp_hum pps l =
    Fmt.(pf pps "%a" (list ~{sep=const string ", "} M.pp_hum) (S.elements l))
  ;
  value pp pps s = pp__t pps (S.elements s) ;
  value show s = show__t (S.elements s) ;
  value to_yojson s = _t_to_yojson (S.elements s);
  value compare s1 s2 = compare__t (S.elements s1) (S.elements s2) ;
  include S ;
end ;

module EntitySet(M : ENTITY_SIG) : (SETSIG with module M = M) = struct
  module M = M ;
  module S = SetWithPP(M) ;
  type t = S.t [@@deriving (to_yojson, eq, ord, show);] ;
  value mt = S.empty ;
  value mem s x = S.mem x s ;
  value add s x = S.add x s ;
  value intersect l1 l2 = S.inter l1 l2 ;
  value subtract l1 l2 = S.diff l1 l2 ;
  value ofList l = S.of_list l ;
  value toList l = S.elements l ;
  value distinct l =
    let rec drec m = fun [
          [] -> True
        | [h::t] ->
           not (mem m h) && drec (add m h) t
        ] in
    drec mt l
  ;
  value union l1 l2 = S.union l1 l2 ;
  value subset l1 l2 = S.subset l1 l2 ;
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
  module DOMS : SETSIG ;
  module RNG : ENTITY_SIG ;
  module RNGS : SETSIG ;
  module DOMMap : (Map.S with type key = DOM.t) ;
  module RNGMap : (Map.S with type key = RNG.t) ;

  type t = 'a [@@deriving (to_yojson, eq, ord, show);] ;
  include (Misc_map.MonoMapS with type t := t and type key = DOM.t and type rng = RNG.t) ;

  value insert : DOM.t -> RNG.t -> t -> t ;

  value find_rng : RNG.t -> t -> DOM.t ;
  value mem_rng : RNG.t -> t -> bool ;
  value dom : t -> DOMS.t ;
  value rng : t -> RNGS.t ;

  include (PP_HUM_SIG with type t := t) ;
end ;

module Bijection(M1 : ENTITY_SIG)(S1 : SETSIG with module M = M1)
         (M2 : ENTITY_SIG)(S2 : SETSIG with module M = M2)
       : (BIJSIG with module DOM = M1
                  and module DOMS = S1
                  and module RNG = M2
                  and module RNGS = S2
         ) = struct
  module DOM = M1 ;
  module DOMS = S1 ;
  module RNG = M2 ;
  module RNGS = S2 ;
  module DOMMap = EntityMap(M1)(S1) ;
  module RNGMap = EntityMap(M2)(S2) ;
  type key = DOM.t ;
  type rng = RNG.t ;
  type t = (DOMMap.t RNG.t * RNGMap.t DOM.t) [@@deriving (to_yojson, eq, ord, show);] ;

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
  value dom (l, _) = DOMMap.dom l ;
  value rng (_, r) = RNGMap.dom r ;
  value find x (l, _) = DOMMap.find x l ;
  value find_rng y (_, r) = RNGMap.find y r ;

  value pp_hum pps (dm, _) = DOMMap.pp_hum RNG.pp_hum pps dm ;
end ;

module IDSet = VarSet(ID) ;
module IDMap = EntityMap(ID)(IDSet) ;
module IntEntity = struct
  type t = int[@@deriving (to_yojson, show, eq, ord);] ;
  value pp_hum = pp ;
end ;
module IntSet = EntitySet(IntEntity) ;
module IntMap = EntityMap(IntEntity)(IntSet) ;
