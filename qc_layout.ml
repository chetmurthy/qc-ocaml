(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions

module Layout = struct
  (* representation of a node -- must be hashable *)
  module Node = struct
    type t = int
    let compare = Pervasives.compare
    let hash = Hashtbl.hash
    let equal = (=)
  end

  (* representation of an edge -- must be comparable *)
  module Edge = struct
    type t = string

    let compare = Pervasives.compare
    let equal = (=)
    let default = ""
  end

  (* a functional/persistent graph *)
  module G = Graph.Persistent.Graph.ConcreteLabeled(Node)(Edge)

  (* more modules available, e.g. graph traversal with depth-first-search *)
  module B = Graph.Traverse.Bfs(G)

  let cmap_to_graph cmap =
    assert (List.length cmap > 0) ;
    assert (distinct cmap) ;
    let max_vertex =
      List.fold_left (fun acc (a, b) -> max acc (max a b)) 0 cmap in
    let g = G.empty in
    let nodes = interval 0 max_vertex in
    let g = List.fold_left G.add_vertex g nodes in
    List.fold_left (fun g (src,dst) ->
        G.add_edge g src dst) g cmap
    
  let bfs g start =
    List.rev (B.fold_component (fun vertex acc -> vertex::acc) [] g start)

end
