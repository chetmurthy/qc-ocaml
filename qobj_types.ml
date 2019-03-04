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
      name : (string option [@default None]) ;
      memory_slots : int ;
      n_qubits : int ;
      qreg_sizes : (string * int) list ;
      creg_sizes : (string * int) list ;
      qubit_labels : (string * int) list ;
      clbit_labels : (string * int) list ;
    } [@@deriving yojson, sexp]

  type t = {
      config: (ExperimentConfig.t option [@default None]) ;
      header : header_t ;
      instructions : Instruction.t list ;
    } [@@deriving yojson, sexp]
end

module Qobj = struct
  type config_t = {
      max_credits : int ;
      memory_slots : int ;
      n_qubits : int ;
      seed : (int [@default 1]) ;
      shots : int ;
      memory : bool ;
    } [@@deriving yojson, sexp]

  type experiment_type_t = QASM | PULSE [@@deriving yojson, sexp]

  type header_t = {
      backend_name : string ;
      backend_version : (string option  [@default None]);
    } [@@deriving yojson, sexp]

  type t = {
      config : config_t ;
      experiments : Experiment.t list ;
      header : header_t ;
      qobj_id : string ;
      schema_version : string ;
      _type : (experiment_type_t [@key "type"])
    } [@@deriving yojson, sexp]
end
