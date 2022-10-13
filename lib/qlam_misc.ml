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
