(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Coll
open Qasmsyntax
open Qasmparser

(* The first implementation of a Multigraph DAG.

  A node is an integer, and we'll store a side-table with whatever
   information we want about nodes.

  Nodes are:

  (i) qubit (either at input or at output)
  (2) cbit (either at input or at output)

  (3) a STATEMENT, *except* gatedecl, opaquedecl, qreg, creg

  (3') [again] QOP, IF, BARRIER

  An edge is decorated with the qubit or cbit that corresponds.

 *)


(* representation of a node -- must be hashable *)
module Node = struct
   type t = int
   let compare = Pervasives.compare
   let hash = Hashtbl.hash
   let equal = (=)
end

type bit_t = 
  | Q of AST.qreg_t * int
  | C of AST.creg_t * int

let bit_to_string = function
  | Q(AST.QREG id, n) -> Printf.sprintf "%s[%d]" id n
  | C(AST.CREG id, n) -> Printf.sprintf "%s[%d]" id n

(* representation of an edge -- must be comparable *)
module Edge = struct
   type t = bit_t

   let compare = Pervasives.compare
   let equal = (=)
   let default = Q(AST.QREG "", -1)
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Edge)

(* more modules available, e.g. graph traversal with depth-first-search *)
module D = Graph.Traverse.Dfs(G)

(* module for creating dot-files *)
module Dot = Graph.Graphviz.Dot(struct
   include G (* use the graph module from above *)
   let edge_attributes (a, e, b) = [`Label (bit_to_string e); `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v = string_of_int v
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
               end)

module DAG = struct
  type node_label_t =
    | INPUT of bit_t
    | OUTPUT of bit_t
    | STMT of TA.t AST.raw_stmt_t

  type node_info_t = {
      label: node_label_t ;
    }

  type t = {
      nextid: int ;
      node_info : (int, node_info_t) LM.t ;
      g : G.t ;
      frontier : (bit_t, int) LM.t ;
    }

  let mk () = {
      nextid = 0 ;
      node_info = LM.mk() ;
      g = G.empty ;
      frontier = LM.mk() ;
    }

  let add_input dag qubit =
    let nodeid = dag.nextid in
    {
      nextid = nodeid + 1 ;
      node_info = LM.add dag.node_info (nodeid, { label = INPUT qubit }) ;
      g = G.add_vertex dag.g nodeid ;
      frontier = LM.add dag.frontier (qubit, nodeid) ;
    }

(*
 * to add a node that touches the argument [bits]:
 *
 * (1) insert this stmt as the DST node
 * for each bit:
 *   (2) find the SRC node in the frontier
 *   (3) insert the edge (SRC, QUBIT, DST)
 *   (4) remap (QUBIT->DST) in the frontier
 
 *)
  let add_node dag stmt bits =
    let nodeid = dag.nextid in
    let bits_edges =
      List.map (fun bit ->
          (bit, LM.map dag.frontier bit))
        bits in
    {
      nextid = nodeid + 1 ;
      node_info = LM.add dag.node_info (nodeid, { label = STMT stmt }) ;
      g =
        dag.g
        |> swap G.add_vertex nodeid
        |> swap (List.fold_left (fun dag (bit, srcnode) ->
                     G.add_edge_e dag (G.E.create srcnode bit nodeid)
                   )) bits_edges ;
      frontier =
        List.fold_left (fun f bit ->
            LM.remap f bit nodeid) dag.frontier bits ;
    }

  let generate_qubit_instances envs l =
    if for_all (function AST.INDEXED _ -> true | _ -> false) l then
      [l]
    else
      let regid =
        try_find (function
            | AST.IT (AST.QREG id) -> id
            | _ -> failwith "caught") l in
      let dim = TYCHK.Env.lookup_qreg envs regid in
      (interval 0 (dim-1))
       |> List.map (fun i ->
              List.map (function
                  | AST.INDEXED _ as qarg -> qarg
                  | AST.IT(AST.QREG id) -> AST.INDEXED(AST.QREG id, i)) l)

  let generate_cbit_instances envs l =
    if for_all (function AST.INDEXED _ -> true | _ -> false) l then
      [l]
    else
      let regid =
        try_find (function
            | AST.IT (AST.CREG id) -> id
            | _ -> failwith "caught") l in
      let dim = TYCHK.Env.lookup_qreg envs regid in
      (interval 0 (dim-1))
       |> List.map (fun i ->
              List.map (function
                  | AST.INDEXED _ as qarg -> qarg
                  | AST.IT(AST.CREG id) -> AST.INDEXED(AST.CREG id, i)) l)


  let make (envs, pl) =
    let rec add_stmt dag stmt =
      match stmt with
    | AST.STMT_GATEDECL _ | STMT_OPAQUEDECL _ -> dag

    | STMT_QREG(id, n) ->
       (* for each {c,qu}bit:
        * (1) create an INPUT(bit) node NID
        * (2) add it to the graph
        * (3) add a bit->NID to the frontier
        *)

       (interval 0 (n-1))
       |> List.map (fun i -> (Q(AST.QREG id, i)))
       |> List.fold_left add_input dag

    | STMT_CREG (id, n) ->
       (interval 0 (n-1))
       |> List.map (fun i -> (C(AST.CREG id, i)))
       |> List.fold_left add_input dag

       (*
        * If the qarg is a QUBIT:
        *
        * (1) find the SRC node in the frontier
        * (2) insert this stmt as the DST node
        * (3) insert the edge (SRC, QUBIT, DST)
        * (4) remap (QUBIT->DST) in the frontier
        *)
    | STMT_QOP(AST.UOP (AST.U(params, qarg))) ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       if List.length qubit_instances > 1 then
         List.fold_left (fun dag [qarg] ->
             add_stmt dag (STMT_QOP(AST.UOP (AST.U(params, qarg))))
           ) dag qubit_instances
       else
         let qargs = List.hd qubit_instances in
         let bits = List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs in
         add_node dag stmt bits

    | STMT_QOP(AST.UOP (AST.CX (qarg1, qarg2))) ->
       let qubit_instances = generate_qubit_instances envs [qarg1; qarg2] in
       if List.length qubit_instances > 1 then
         List.fold_left (fun dag [qarg1; qarg2] ->
             add_stmt dag (AST.STMT_QOP(AST.UOP (AST.CX (qarg1, qarg2)))))
           dag qubit_instances
       else
         let [qarg1; qarg2] as qargs = List.hd qubit_instances in
         let stmt = AST.STMT_QOP(AST.UOP (AST.CX (qarg1, qarg2))) in
         let bits = List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs in
         add_node dag stmt bits

    | STMT_QOP(AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, qargs))) ->
       let qubit_instances = generate_qubit_instances envs qargs in
       if List.length qubit_instances > 1 then
         List.fold_left (fun dag qargs ->
             add_stmt dag (AST.STMT_QOP(AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, qargs)))))
           dag qubit_instances
       else
         let qargs = List.hd qubit_instances in
         let stmt = AST.STMT_QOP(AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, qargs))) in
         let bits = List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs in
         add_node dag stmt bits

    | STMT_QOP(AST.MEASURE(qarg, carg)) ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       let cbit_instances = generate_cbit_instances envs [carg] in
       assert(List.length qubit_instances = List.length cbit_instances) ;
       let l = combine qubit_instances cbit_instances in
       let l = List.map (fun ([q],[c]) -> (q,c)) l in
       if List.length l > 1 then
         List.fold_left (fun dag (qarg,carg) ->
             add_stmt dag (STMT_QOP(AST.MEASURE(qarg, carg)))
           ) dag l
       else let (qarg, carg) = List.hd l in
            let stmt = AST.STMT_QOP(AST.MEASURE(qarg, carg)) in
            let bits = [
                (match qarg with
                 | AST.INDEXED (AST.QREG id, i) -> Q(AST.QREG id, i));
                (match carg with
                 | AST.INDEXED (AST.CREG id, i) -> C(AST.CREG id, i));
              ] in
            add_node dag stmt bits

    | STMT_QOP(AST.RESET qarg) ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       if List.length qubit_instances > 1 then
         List.fold_left (fun dag [qarg] ->
             add_stmt dag (AST.STMT_QOP(AST.RESET qarg))
           ) dag qubit_instances
       else let [aqrg] = List.hd qubit_instances in
            let stmt = AST.STMT_QOP(AST.RESET qarg) in
            let bits = [
                (match qarg with
                 | AST.INDEXED (AST.QREG id, i) -> Q(AST.QREG id, i))
              ] in
            add_node dag stmt bits

    | STMT_IF (creg, n, op) -> dag

    | STMT_BARRIER l -> dag

    in
    ()

end
