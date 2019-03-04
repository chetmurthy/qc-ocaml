(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Misc_functions
open Coll
open Qc_environment
open Qasmsyntax
open Qasmsyntax
open Qasmparser
open Qasmpp

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

module DAG = struct

(* representation of a node -- must be hashable *)
module Node = struct
   type t = int
   let compare (v1: t) (v2: t) = Pervasives.compare v1 v2
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

let tsort ~compare f g acc =
  let todo = ref [] in
  let indegree = Hashtbl.create 97 in

  G.iter_vertex (fun v ->
      let d = G.in_degree g v in
      if d = 0 then push todo v
      else Hashtbl.add indegree v d
    ) g;

  let rec dorec acc =
    if !todo = [] then
      if Hashtbl.length indegree = 0 then acc
      else failwith "tsort: DAG was cyclic!"
    else
    let work = !todo in
    todo := [] ;
    List.iter (fun src ->
        G.iter_succ_e (fun (_, _, dst) ->
            if not (Hashtbl.mem indegree dst) then failwith "tsort: DAG was cyclic" ;
            let d = Hashtbl.find indegree dst in
            if d = 1 then begin
                Hashtbl.remove indegree dst ;
                push todo dst
              end
            else
              Hashtbl.replace indegree dst (d-1)
          ) g src
      ) work ;
    let work = List.sort compare work in
    let acc = List.fold_left (fun acc v -> f v acc) acc work in
    dorec acc

  in dorec acc

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
    }

  let pr_node_info ~prefix info =
    match info.label with
    | INPUT bit -> [< '"<input " ; 'bit_to_string bit ; '">\n" >]
    | OUTPUT bit -> [< '"<output " ; 'bit_to_string bit ; '">\n" >]
    | STMT stmt -> ASTPP.raw_stmt stmt

  let pr_node dag (vertex, info) =
    let el = G.succ_e dag.g vertex in
    [< '"  " ;
     'string_of_int vertex
     ; '" "; pr_node_info ~prefix:"  " info ;
     prlist (fun (_, elabel, succ_vertex) ->
         [< '"    " ; 'Printf.sprintf "%s -> %d\n" (bit_to_string elabel) succ_vertex >]
       ) el ;
     >]

  let pp_dag dag =
    let canon x = List.sort Pervasives.compare x in

    [< 'Printf.sprintf "nextid: %d\n" dag.nextid ;
     '"node_info:\n" ;
     prlist (fun (n, info) ->
         pr_node dag (n, info))
       (dag.node_info |> LM.toList |> canon)
     >]

  let pp_frontier m =
    let canon x = List.sort Pervasives.compare x in
    let l = m |> LM.toList |> canon in
    if l = [] then [< >]
    else [< '"frontier:\n" ;
          prlist (fun (bit, vertex) ->
              [< '"  " ; 'string_of_int vertex ; '" -> " ; 'bit_to_string bit ; '"\n" >]
            ) l
          >]
  let pp_both (dag, frontier) =
    [< pp_dag dag ; pp_frontier frontier >]

  let mk () =
    ({
        nextid = 0 ;
        node_info = LM.mk() ;
        g = G.empty ;
      },
     LM.mk())

  let add_input (dag, frontier) qubit =
    let nodeid = dag.nextid in
    ({
        nextid = nodeid + 1 ;
        node_info = LM.add dag.node_info (nodeid, { label = INPUT qubit }) ;
        g = G.add_vertex dag.g nodeid ;
      },
     LM.add frontier (qubit, nodeid))

  let add_output (dag, frontier) qubit =
    let nodeid = dag.nextid in
    let src = LM.map frontier qubit in
    let g = dag.g in
    let g = G.add_vertex g nodeid in
    let g = G.add_edge_e g (src, qubit, nodeid) in
    ({
        nextid = nodeid + 1 ;
        node_info = LM.add dag.node_info (nodeid, { label = OUTPUT qubit }) ;
        g = g ;
      },
     LM.rmv frontier qubit)

(*
 * to add a node that touches the argument [bits]:
 *
 * (1) insert this stmt as the DST node
 * for each bit:
 *   (2) find the SRC node in the frontier
 *   (3) insert the edge (SRC, QUBIT, DST)
 *   (4) remap (QUBIT->DST) in the frontier
 
 *)
  let add_node (dag, frontier) stmt bits =
    let nodeid = dag.nextid in
    let bits_edges =
      List.map (fun bit ->
          (bit, LM.map frontier bit))
        bits in
    ({
        nextid = nodeid + 1 ;
        node_info = LM.add dag.node_info (nodeid, { label = STMT stmt }) ;
        g =
          dag.g
          |> swap G.add_vertex nodeid
          |> swap (List.fold_left (fun g (bit, srcnode) ->
                       G.add_edge_e g (G.E.create srcnode bit nodeid)
               )) bits_edges ;
      },
     List.fold_left (fun f bit ->
         LM.remap f bit nodeid) frontier bits)

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
      let dim = TYCHK.Env.lookup_creg envs regid in
      (interval 0 (dim-1))
       |> List.map (fun i ->
              List.map (function
                  | AST.INDEXED _ as carg -> carg
                  | AST.IT(AST.CREG id) -> AST.INDEXED(AST.CREG id, i)) l)


  let generate_qop_instances envs = function
    | AST.UOP (AST.U(params, qarg)) ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       List.map (fun ([qarg] as qargs) ->
           (AST.UOP (AST.U(params, qarg)),
            List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs
           )
         ) qubit_instances

    | AST.UOP (AST.CX (qarg1, qarg2)) ->
       let qubit_instances = generate_qubit_instances envs [qarg1; qarg2] in
       List.map (fun [qarg1; qarg2] as qargs ->
           (AST.UOP (AST.CX (qarg1, qarg2)),
            List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs)                 
         ) qubit_instances

    | AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, qargs)) ->
       let qubit_instances = generate_qubit_instances envs qargs in
       List.map (fun qargs ->
           (AST.UOP (AST.COMPOSITE_GATE(gateid, actual_params, qargs)),
            List.map (function AST.INDEXED (qreg, i) -> Q(qreg, i)) qargs)
         ) qubit_instances

    | AST.MEASURE(qarg, carg) ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       let cbit_instances = generate_cbit_instances envs [carg] in
       assert(List.length qubit_instances = List.length cbit_instances) ;
       let l = combine qubit_instances cbit_instances in
       let l = List.map (fun ([q],[c]) -> (q,c)) l in
       List.map (fun (qarg, carg) ->
           (AST.MEASURE(qarg, carg),
            [
              (match qarg with
               | AST.INDEXED (AST.QREG id, i) -> Q(AST.QREG id, i));
              (match carg with
               | AST.INDEXED (AST.CREG id, i) -> C(AST.CREG id, i));
            ]                 
           )
         ) l

    | AST.RESET qarg ->
       let qubit_instances = generate_qubit_instances envs [qarg] in
       List.map (fun [qarg] ->
           (AST.RESET qarg,
            [
              (match qarg with
               | AST.INDEXED (AST.QREG id, i) -> Q(AST.QREG id, i))
            ]
           )
         ) qubit_instances

  let rec add_stmt envs dag stmt =
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

    | STMT_QOP q ->
       let l = generate_qop_instances envs q in
       List.fold_left (fun dag (q, bits) ->
           add_node dag (AST.STMT_QOP q) bits
         ) dag l

    | STMT_IF (AST.CREG cregid as creg, n, qop) ->
       let dim = TYCHK.Env.lookup_creg envs cregid in
       let cbits =
         (interval 0 (dim-1))
         |> List.map (function i -> C(AST.CREG cregid, i)) in
       let l = generate_qop_instances envs qop in
       let l = List.map (fun (qop, bits) ->
                   (AST.STMT_IF(creg, n, qop),
                    cbits @ bits)
                 ) l in
       List.fold_left (fun dag (stmt, bits) ->
           add_node dag stmt bits
         ) dag l

    | STMT_BARRIER qargs ->
       let gen_qubits = function
         | AST.INDEXED(AST.QREG id, i) -> [Q(AST.QREG id, i)]
         | AST.IT(AST.QREG id) ->
            let dim = TYCHK.Env.lookup_qreg envs id in
            (interval 0 (dim-1))
            |> List.map (function i -> Q(AST.QREG id, i)) in

       let bits =
         List.fold_left (fun bits qarg ->
             bits @ (gen_qubits qarg))
           [] qargs in

       add_node dag stmt bits

  let make envs pl =
    let pl = List.map snd pl in
    let dag = mk() in
    let dag = List.fold_left (add_stmt envs) dag pl in
    let dag = List.fold_left add_output dag (LM.dom (snd dag)) in
    dag

  let rec stmt_to_label ~terse stmt =
    if not terse then pp ASTPP.raw_stmt stmt
    else match stmt with
         | AST.STMT_QOP(AST.UOP (AST.U _)) -> "U"
         | AST.STMT_QOP(AST.UOP (AST.CX _)) -> "CX"
         | AST.STMT_QOP(AST.UOP (AST.COMPOSITE_GATE(gateid, _,_))) -> gateid
         | AST.STMT_QOP(AST.MEASURE _) -> "measure"
         | AST.STMT_QOP(AST.RESET _) -> "reset"
         | AST.STMT_IF (_,_, qop) ->
            "if/"^(stmt_to_label ~terse (AST.STMT_QOP qop))
         | AST.STMT_BARRIER _ -> "barrier"

         | AST.STMT_GATEDECL _ | STMT_OPAQUEDECL _| STMT_QREG _ | STMT_CREG _ -> assert false

  let dot ?(terse=true) dag =
    let open Odot in
    let dot_vertex_0 v acc =
      let color, label = match (LM.map dag.node_info v).label with
        | INPUT bit -> ("green", bit_to_string bit)
        | OUTPUT bit -> ("red", bit_to_string bit)
        | STMT stmt -> ("lightblue", stmt_to_label ~terse stmt) in

      (Stmt_node ((Simple_id (string_of_int v), None),
                      [(Simple_id "color", Some (Simple_id "black"));
                       (Simple_id "fillcolor", Some (Simple_id color));
                       (Simple_id "label", Some (Double_quoted_id label));
                       (Simple_id "style", Some (Simple_id "filled"));
        ]) :: acc) in
    let dot_edge_0 (s, label, d) acc =
      (Stmt_edge
        (Edge_node_id (Simple_id (string_of_int s), None),
         [Edge_node_id (Simple_id (string_of_int d), None)],
         [
           (Simple_id "label", Some (Double_quoted_id (bit_to_string label)));
        ]) :: acc) in

    let l =
      []
      |> G.fold_vertex dot_vertex_0 dag.g
      |> G.fold_edges_e dot_edge_0 dag.g
      |> List.rev in
    let l = 
      (Odot.Stmt_attr
         (Odot.Attr_node
            [(Odot.Simple_id "label", Some (Odot.Double_quoted_id "\\N"))])) :: l in

    {strict = false; kind = Digraph; id = Some (Simple_id "G");
     stmt_list = l }

  let dot_to_file fname p =
    apply_to_out_channel (fun oc -> Odot.print oc p) fname

  let compare dag a b =
    let a_info = LM.map dag.node_info a in
    let b_info = LM.map dag.node_info b in
    Pervasives.compare (a_info, a) (b_info, b)

  let tsort dag =
    []
    |> tsort ~compare:(compare dag) (fun v l -> v::l) dag.g
    |> List.rev

  let to_ast envs dag =
    let canon x = List.sort Pervasives.compare x in
    let open TYCHK in
    let nodel = tsort dag in
    let stmts =
      List.fold_left (fun acc n ->
          match (LM.map dag.node_info n).label with
          | INPUT _|OUTPUT _ -> acc
          | STMT stmt -> (TA.mt, stmt)::acc) [] nodel in
    let stmts = List.rev stmts in
    let qregdecls = List.map (fun (id, dim) ->
                        (TA.mt, AST.STMT_QREG(id, dim)))
                  (LM.toList envs.Env.qregs) in
    let cregdecls = List.map (fun (id, dim) ->
                        (TA.mt, AST.STMT_CREG(id, dim)))
                  (LM.toList envs.Env.cregs) in
    (canon qregdecls) @ (canon cregdecls) @ stmts

end
