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
