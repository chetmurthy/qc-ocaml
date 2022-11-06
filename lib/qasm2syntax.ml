(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Pa_ppx_utils
open Std
open Misc_functions
open Qc_misc

(* a string that matches 

real      := ([0-9]+\.[0-9]*|[0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?

*)

type rawtoken =
  | T_EOF
  (* special symbols *)
  | T_OPENQASM of RealNumeral.t
  | T_INCLUDE of string

  (* special characters *)
  | T_SEMICOLON
  | T_LBRACE
  | T_RBRACE
  | T_LBRACKET
  | T_RBRACKET
  | T_LPAREN
  | T_RPAREN
  | T_EQEQ
  | T_COMMA
  | T_DASH
  | T_PLUS
  | T_STAR
  | T_STARSTAR
  | T_SLASH
  | T_CARET
  | T_DASHGT

  (* reserved words *)
  | T_BARRIER
  | T_COS
  | T_CREG
  | T_CX
  | T_EXP
  | T_GATE
  | T_IF
  | T_LN
  | T_PI
  | T_QREG
  | T_SIN
  | T_SQRT
  | T_TAN
  | T_U
  | T_MEASURE
  | T_OPAQUE
  | T_RESET

  | T_INTEGER of int
  | T_REAL of RealNumeral.t
  | T_ID of string

module LexState = struct
  type t = {
      mutable at_head : bool ;
    }

  let mk () = { at_head = true }
  let is_at_head st = st.at_head

  let past_head st =
    assert(is_at_head st) ;
    st.at_head <- false
end

type token = Ploc.t * rawtoken

module CST = struct
  type expr =
    ID of string
  | REAL of RealNumeral.t
  | NNINT of int
  | PI
  | ADD of expr * expr
  | SUB of expr * expr
  | MUL of expr * expr
  | DIV of expr * expr
  | UMINUS of expr
  | POW of expr * expr
  | SIN of expr
  | COS of expr
  | TAN of expr
  | EXP of expr
  | LN of expr
  | SQRT of expr


  type id_or_indexed_t =
    | REG of string
    | BIT of string * int

  type raw_uop_t =
    | U of expr list * id_or_indexed_t
    | CX of id_or_indexed_t * id_or_indexed_t
    | COMPOSITE_GATE of string * expr list * id_or_indexed_t list

  type raw_qop_t =
    | UOP of raw_uop_t
    | MEASURE of id_or_indexed_t * id_or_indexed_t
    | RESET of id_or_indexed_t

  type 'aux qop_t =
    'aux * raw_qop_t

  type raw_gate_op_t =
    GATE_UOP of raw_uop_t
  | GATE_BARRIER of string list

  type 'aux gate_op_t =
    'aux * raw_gate_op_t

  type 'aux raw_stmt_t =
    | STMT_INCLUDE of file_type_t * string * 'aux stmt_t list
    | STMT_GATEDECL of string * string list * string list * 'aux gate_op_t list
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_QOP of raw_qop_t
    | STMT_IF of string * int * raw_qop_t
    | STMT_BARRIER of id_or_indexed_t list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

  and 'aux stmt_t = 'aux * 'aux raw_stmt_t

  type 'aux program_t = 'aux stmt_t list

  module AuxMap = struct
    type ('a, 'b) mappers_t = {
        stmt : 'a -> 'a raw_stmt_t -> 'b ;
        gop : 'a -> raw_gate_op_t -> 'b ;
      }

    let gop mappers (aux, raw_gop) =
      let aux' = mappers.gop aux raw_gop in
      (aux', raw_gop)

    let rec raw_stmt mappers = function
      | STMT_INCLUDE (ty, fname, l) -> STMT_INCLUDE(ty, fname, List.map (stmt mappers) l)
      | STMT_GATEDECL(gateid, formal_params, formal_qregs, gopl) ->
         STMT_GATEDECL(gateid, formal_params, formal_qregs,
                           List.map (gop mappers) gopl)

      | STMT_OPAQUEDECL(a, b, c) -> STMT_OPAQUEDECL(a, b, c)
      | STMT_QOP q -> STMT_QOP q
      | STMT_IF(a, b, c) -> STMT_IF(a, b, c)
      | STMT_BARRIER l -> STMT_BARRIER l
      | STMT_QREG (a,b) -> STMT_QREG (a,b)
      | STMT_CREG (a, b) -> STMT_CREG (a, b)

    and stmt mappers (aux, raw_stmt0) =
      let aux' = mappers.stmt aux raw_stmt0 in
      let raw_stmt' = raw_stmt mappers raw_stmt0 in
      (aux', raw_stmt')

    let program mappers l = List.map (stmt mappers) l
  end

end

module AST = struct
  type param_var_t =
    | CPARAM of string [@@deriving to_yojson, show, eq, ord]

  type gate_bit_t =
    | QUBIT of string [@@deriving to_yojson, show, eq, ord]

  type main_var_t =
    | CREG of string
    | QREG of string [@@deriving to_yojson, show, eq, ord]

  type 'paramvar expr =
    | ID of 'paramvar
    | REAL of RealNumeral.t
    | NNINT of int
    | PI
    | ADD of 'paramvar expr * 'paramvar expr
    | SUB of 'paramvar expr * 'paramvar expr
    | MUL of 'paramvar expr * 'paramvar expr
    | DIV of 'paramvar expr * 'paramvar expr
    | UMINUS of 'paramvar expr
    | POW of 'paramvar expr * 'paramvar expr
    | SIN of 'paramvar expr
    | COS of 'paramvar expr
    | TAN of 'paramvar expr
    | EXP of 'paramvar expr
    | LN of 'paramvar expr
    | SQRT of 'paramvar expr [@@deriving to_yojson, show, eq, ord]

  type cparamvar_t = CPARAMVAR of string [@@deriving to_yojson, show, eq, ord]
  type qubit_t = QUBIT of string [@@deriving to_yojson, show, eq, ord]
  type qreg_t = QREG of string [@@deriving to_yojson, show, eq, ord]
  type creg_t = CREG of string [@@deriving to_yojson, show, eq, ord]

  type 'a or_indexed =
    | IT of 'a
    | INDEXED of 'a * int [@@deriving to_yojson, show, eq, ord]

  type empty_t
  let compare_empty_t x y = assert false
  let equal_empty_t x y = assert false
  let pp_empty_t pps x = assert false
  let empty_t_to_yojson x = assert false

  type ('paramvar, 'qregvar) raw_uop_t =
    | U of 'paramvar expr list * 'qregvar
    | CX of 'qregvar * 'qregvar
    | COMPOSITE_GATE of string * 'paramvar expr list * 'qregvar list [@@deriving to_yojson, show, eq, ord]

  type raw_qop_t =
    | UOP of (empty_t, qreg_t or_indexed) raw_uop_t
    | MEASURE of qreg_t or_indexed * creg_t or_indexed
    | RESET of qreg_t or_indexed [@@deriving to_yojson, show, eq, ord]


  type  'aux qop_t =
    'aux * raw_qop_t [@@deriving to_yojson, show, eq, ord]

  type  raw_gate_op_t =
    GATE_UOP of (cparamvar_t, qubit_t) raw_uop_t
  | GATE_BARRIER of qubit_t list [@@deriving to_yojson, show, eq, ord]

  type 'aux gate_op_t =
    'aux * raw_gate_op_t [@@deriving to_yojson, show, eq, ord]

  type 'aux gatedecl_t = string * string list * string list * 'aux gate_op_t list [@@deriving to_yojson, show, eq, ord]

  type 'aux raw_stmt_t =
    | STMT_INCLUDE of file_type_t * string * 'aux stmt_t list option
    | STMT_GATEDECL of 'aux gatedecl_t
    | STMT_OPAQUEDECL of string * string list * string list
    | STMT_QOP of raw_qop_t
    | STMT_IF of creg_t * int * raw_qop_t
    | STMT_BARRIER of qreg_t or_indexed list
    | STMT_QREG of string * int
    | STMT_CREG of string * int

  and 'aux stmt_t = 'aux * 'aux raw_stmt_t [@@deriving to_yojson, show, eq, ord]

  type 'aux program_t = 'aux stmt_t list [@@deriving to_yojson, show, eq, ord]

  module AuxMap = struct
    type ('a, 'b) mappers_t = {
        stmt : 'a -> 'a raw_stmt_t -> 'b ;
        gop : 'a -> raw_gate_op_t -> 'b ;
      }

    let gop mappers (aux, raw_gop) =
      let aux' = mappers.gop aux raw_gop in
      (aux', raw_gop)

    let rec raw_stmt mappers = function
      | STMT_INCLUDE (ty, fname, l) -> STMT_INCLUDE(ty, fname, Option.map (List.map (stmt mappers)) l)
      | STMT_GATEDECL(gateid, formal_params, formal_qregs, gopl) ->
         STMT_GATEDECL(gateid, formal_params, formal_qregs,
                           List.map (gop mappers) gopl)

      | STMT_OPAQUEDECL(a, b, c) -> STMT_OPAQUEDECL(a, b, c)
      | STMT_QOP q -> STMT_QOP q
      | STMT_IF(a, b, c) -> STMT_IF(a, b, c)
      | STMT_BARRIER l -> STMT_BARRIER l
      | STMT_QREG (a,b) -> STMT_QREG (a,b)
      | STMT_CREG (a, b) -> STMT_CREG (a, b)

    and stmt mappers (aux, raw_stmt0) =
      let aux' = mappers.stmt aux raw_stmt0 in
      let raw_stmt' = raw_stmt mappers raw_stmt0 in
      (aux', raw_stmt')

    let program mappers l = List.map (stmt mappers) l
  end

end

module TYCHK = struct
  open Pa_ppx_utils
  open Coll

type Pa_ppx_runtime_fat.Exceptions.t +=
    TypeError of bool * string[@name "TypeError"]
[@@deriving show, sexp, yojson, eq]


  module Env = struct
    type 'a t = {
        gates: (string, 'a AST.gatedecl_t) LM.t ;
        qregs: (string, int) LM.t ;
        cregs: (string, int) LM.t ;
      }

    let equal e1 e2 =
      let canon x = List.sort Stdlib.compare x in
      (e1.gates |> LM.toList |> canon) = (e2.gates |> LM.toList |> canon) &&
      (e1.qregs |> LM.toList |> canon) = (e2.qregs |> LM.toList |> canon) &&
      (e1.cregs |> LM.toList |> canon) = (e2.cregs |> LM.toList |> canon)

    let mk () = { gates = LM.mk() ; qregs = LM.mk() ; cregs = LM.mk() }

    let add_gate envs (id, v) = { envs with gates = LM.add envs.gates (id,v) }
    let add_qreg envs (id, v) = { envs with qregs = LM.add envs.qregs (id,v) }
    let add_creg envs (id, v) = { envs with cregs = LM.add envs.cregs (id,v) }

    let has_gate envs id = LM.in_dom envs.gates id
    let find_gate envs id = LM.map envs.gates id

    let must_have_gate envs id =
      if not (has_gate envs id) then
        raise (TypeError (false, Printf.sprintf "gate %s not declared" id))

    let must_not_have_gate envs id =
      if has_gate envs id then
        raise (TypeError (false, Printf.sprintf "gate %s already declared" id))

    let has_creg envs id = LM.in_dom envs.cregs id
    let intern_creg envs id =
      if has_creg envs id then AST.CREG id
      else raise (TypeError(false, Printf.sprintf "creg %s not declared" id))
    let lookup_creg envs id =
      if has_creg envs id then LM.map envs.cregs id
      else raise (TypeError(false, Printf.sprintf "creg %s not declared" id))

    let must_not_have_creg envs id =
      if has_creg envs id then
        raise (TypeError(false, Printf.sprintf "creg %s already declared" id))

    let has_qreg envs id = LM.in_dom envs.qregs id
    let intern_qreg envs id =
      if has_qreg envs id then AST.QREG id
      else raise (TypeError(false, Printf.sprintf "qreg %s not declared" id))
    let lookup_qreg envs id =
      if has_qreg envs id then LM.map envs.qregs id
      else raise (TypeError(false, Printf.sprintf "qreg %s not declared" id))

    let must_not_have_qreg envs id =
      if has_qreg envs id then
        raise (TypeError(false, Printf.sprintf "qreg %s already declared" id))

    let auxmap mappers env =
      let map_gatedecl (a,b,c,gopl) =
        let rst = AST.STMT_GATEDECL(a,b,c,gopl) in
        match AST.AuxMap.raw_stmt mappers rst with
        | AST.STMT_GATEDECL(a,b,c,gopl) -> (a,b,c,gopl)
        | _ -> assert false in

      let gates =
        env.gates
        |> LM.toList
        |> List.map (fun (k,v) -> (k, map_gatedecl v))
        |> LM.ofList () in

      { env with gates = gates }

  end

  let _expr cparam (cst : CST.expr) =
    let rec erec = function
      | CST.ID id -> AST.ID (cparam id)
      | REAL r -> AST.REAL r
      | NNINT n -> AST.NNINT n
      | PI -> AST.PI
      | ADD(e1, e2) -> AST.ADD(erec e1, erec e2)
      | SUB(e1, e2) -> AST.SUB(erec e1, erec e2)
      | MUL(e1, e2) -> AST.MUL(erec e1, erec e2)
      | DIV(e1, e2) -> AST.DIV(erec e1, erec e2)
      | UMINUS e1 -> AST.UMINUS(erec e1)
      | POW(e1, e2) -> AST.POW(erec e1, erec e2)
      | SIN e1 -> AST.SIN(erec e1)
      | COS e1 -> AST.COS(erec e1)
      | TAN e1 -> AST.TAN(erec e1)
      | EXP e1 -> AST.EXP(erec e1)
      | LN e1 -> AST.LN(erec e1)
      | SQRT e1 -> AST.SQRT(erec e1)
    in erec cst

  let intern_or_indexed f envs arg =
    match arg with
    | CST.REG id -> AST.IT (f envs id)
    | BIT (id, n) -> AST.INDEXED(f envs id, n)

  let raw_uop envs cparam qargs = function
    | CST.U(el, qr) -> AST.U(List.map (_expr cparam) el, [qr] |> qargs |> List.hd)
    | CX(qr1, qr2) ->
       let [qr1; qr2] = qargs [qr1; qr2] in
       AST.CX(qr1, qr2)
    | COMPOSITE_GATE(gateid, cparam_actuals, qal) ->
       Env.must_have_gate envs gateid ;
       AST.COMPOSITE_GATE(gateid, List.map (_expr cparam) cparam_actuals,
                          qargs qal)

  let raw_gate_op envs cparam qargs (cst: CST.raw_gate_op_t) =
    match cst with
    | CST.GATE_UOP u ->
       AST.GATE_UOP (raw_uop envs cparam qargs u)
    | GATE_BARRIER l -> AST.GATE_BARRIER(l |> List.map (fun id -> CST.REG id) |> qargs)

  let gate_op envs cparam qargs (aux, cst) =
    try
      (aux, raw_gate_op envs cparam qargs cst)
    with TypeError (false, msg) ->
      Ploc.raise aux (TypeError(true, msg))

    (* to convert and typecheck a list of qargs (register OR bit):

       (0) check they exist in envs.qregs and convert

       (1) check that all qargs are distinct

           --> so all registers are distinct
           --> all bits are distinct

       (2) check that all registers are of identical dimension

       (3) check that all indexes of BITs are 0 <= index < {dimension of their register}

       (4) check that no register-of-a-BIT is also a register qarg

     *)
  let convert_qargs envs l =
    let _qarg x = intern_or_indexed Env.intern_qreg envs x in
    (*0*)
    let conv_qargs = List.map _qarg l in
    (*1*)
    if not (distinct conv_qargs) then
      raise (TypeError(false, "qargs are not distinct")) ;

    (*2*)
    let registers = filter (function (AST.IT _) -> true | _ -> false) conv_qargs  in
    let register_ids = List.map (fun (AST.IT (AST.QREG id)) -> id) registers in
    let register_dims = List.map (Env.lookup_qreg envs) register_ids in
    begin match register_dims with
    | [] -> ()
    | h::t ->
       if not (List.for_all ((=) h) t) then
         raise (TypeError(false, "registers with different dimensions in qargs"))
    end ;

    (*3*)
    let bits = filter (function (AST.INDEXED _) -> true | _ -> false) conv_qargs  in
    let bits_data = List.map (function (AST.INDEXED (AST.QREG id, n)) ->
                                ((id, n), Env.lookup_qreg envs id)) bits in
    List.iter (fun ((id, n), dimension) ->
        if not (0 <= n && n < dimension) then
          raise (TypeError(false, Printf.sprintf "bit %s[%d] out of dimension [0..%d)" id n dimension))
      ) bits_data ;

    (*4*)
    List.iter (fun ((id, n), dimension) ->
        if List.mem id register_ids then
          raise (TypeError(false, Printf.sprintf "bit %s[%d] conflicts with register of same name" id n)) ;
      ) bits_data ;

    conv_qargs

  let raw_qop envs (cst: CST.raw_qop_t) =
    let cparam id =
      raise (TypeError(false, Printf.sprintf "cparams not permitted here (\"%s\")" id)) in
    let carg x = intern_or_indexed Env.intern_creg envs x in
    match cst with
    | CST.UOP u ->
       AST.UOP(raw_uop envs cparam (convert_qargs envs) u)

    | MEASURE(q, c) ->
       AST.MEASURE([q] |> convert_qargs envs |> List.hd, carg c)

    | RESET q -> AST.RESET ([q] |> convert_qargs envs |> List.hd)

  let qop envs (aux, cst) =
    try
      (aux, raw_qop envs cst)
    with TypeError (false, msg) ->
      Ploc.raise aux (TypeError (true, msg))

  let rec raw_stmt envs (cst: 'aux CST.raw_stmt_t) =
    match cst with
    | CST.STMT_INCLUDE(_, _, _) ->
       assert false
    | CST.STMT_GATEDECL(gateid, param_formals, qubit_formals, gopl) ->
       Env.must_not_have_gate envs gateid ;
       let cparam id =
         if List.mem id param_formals then AST.CPARAMVAR id
         else raise (TypeError(false, Printf.sprintf "cparam %s not declared" id)) in
       let _qarg = function
         | CST.REG id ->
            if List.mem id qubit_formals then AST.QUBIT id
            else raise (TypeError(false, Printf.sprintf "qarg %s not declared" id))
         | BIT (id, n) ->
            raise (TypeError(false, Printf.sprintf "qarg %s[%n] not permitted in composite-gate definition body" id n)) in
       let qargs l =
         if not (distinct l) then
           raise (TypeError(false, "qargs are not distinct")) ;
         List.map _qarg l
       in

       AST.STMT_GATEDECL(gateid, param_formals, qubit_formals,
                         List.map (gate_op envs cparam qargs) gopl)
      
    | STMT_OPAQUEDECL(gateid, param_formals, qubit_formals) ->
       AST.STMT_OPAQUEDECL(gateid, param_formals, qubit_formals)

    | STMT_QOP q -> AST.STMT_QOP(raw_qop envs q)

    | STMT_IF(id, n, q) ->
       AST.STMT_IF(Env.intern_creg envs id, n, raw_qop envs q)

    | STMT_BARRIER l ->
       AST.STMT_BARRIER(convert_qargs envs l)

    | STMT_QREG(id, n) ->
       Env.must_not_have_qreg envs id ;
       AST.STMT_QREG(id, n)

    | STMT_CREG(id, n) ->
       Env.must_not_have_creg envs id ;
       AST.STMT_CREG(id, n)

  and stmt0 envs (aux, rst) =
    try
      (aux,raw_stmt envs rst)
    with TypeError (false, msg) ->
      Ploc.raise aux (TypeError (true, msg))

  let fold_left_map f acc l =
    let (acc, revl) = List.fold_left (fun (acc, revl) x ->
                          let (acc, x) = f acc x in
                          (acc, x::revl))
                    (acc, []) l in
    (acc, List.rev revl)

  let add1 envs stmt = match stmt with
      | _, AST.STMT_QREG(id, n) -> Env.add_qreg envs (id, n)
      | _, STMT_CREG(id, n) -> Env.add_creg envs (id, n)
      | _, STMT_GATEDECL(gateid, param_formals, qubit_formals, gopl) ->
         Env.add_gate envs (gateid, (gateid, param_formals, qubit_formals, gopl))
      | _ -> envs

  let rec stmt envs st = match st with
      aux, CST.STMT_INCLUDE(ty, fname, l) ->
      let (envs, l) = fold_left_map stmt envs l in
      (envs, (aux, AST.STMT_INCLUDE(ty, fname, Some l)))
    | x -> let st = stmt0 envs st in
           (add1 envs st, st)

  let program l =
    fold_left_map stmt (Env.mk()) l
end
