(* Copyright 2019 Chetan Murthy, All rights reserved. *)
open Sexplib0.Sexp_conv

module ExperimentConfig = struct
  type t = {
      memory_slots : int ;
      n_qubits : int ;
    } [@@deriving yojson, sexp]

end

module Instruction = struct
  type t = {
      name : string ;
      params : float list ;
      texparams : string list ;
      qubits : int list ;
      memory : int list ;
    } [@@deriving yojson, sexp]
end

module Experiment = struct

  type header_t = {
      name : (string option [@sexp.default None][@yojson.default None]) ;
      memory_slots : int ;
      n_qubits : int ;
      qreg_sizes : (string * int) list ;
      creg_sizes : (string * int) list ;
      qubit_labels : (string * int) list ;
      clbit_labels : (string * int) list ;
      compiled_circuit_qasm : (string option [@sexp.default None][@yojson.default None]) ;
    } [@@deriving yojson, sexp]

  type t = {
      config: (ExperimentConfig.t option [@sexp.default None][@yojson.default None]) ;
      header : header_t ;
      instructions : Instruction.t list ;
    } [@@deriving yojson, sexp]
end

module Qobj = struct
  type config_t = {
      max_credits : int ;
      memory_slots : int ;
      n_qubits : int ;
      seed : (int [@sexp.default 1][@yojson.default 1]) ;
      shots : int ;
      memory : bool ;
    } [@@deriving yojson, sexp]

  type experiment_type_t =
    | QASM
    | PULSE [@@deriving sexp]

  let experiment_type_t_to_yojson = function
    | QASM -> `String "QASM"
    | PULSE -> `String "PULSE"

  let experiment_type_t_of_yojson = function
    | `String "QASM" -> Result.Ok QASM
    | `String "PULSE" -> Result.Ok PULSE
    | _ -> Result.Error "Qobj_types.Qobj.experiment_type_t"

  type header_t = {
      backend_name : string ;
      backend_version : (string option [@sexp.default None][@yojson.default None]);
    } [@@deriving yojson, sexp]

  type t = {
      config : config_t ;
      experiments : Experiment.t list ;
      header : header_t ;
      qobj_id : string ;
      schema_version : string ;
      _type : experiment_type_t [@sexp.key "type"][@yojson.key "type"] ;
    } [@@deriving yojson, sexp]
end
