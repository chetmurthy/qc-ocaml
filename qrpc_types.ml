open Sexplib0.Sexp_conv

module CouplingMap = struct
  type t = int list list [@@deriving yojson, sexp]
end

module Enum01 = struct
  type t = int [@@deriving yojson, sexp]
  let of_int = function
    | 0 -> 0
    | 1 -> 1
    | _ -> failwith "Enum01.of_int: invalid argument"
  let to_int n = n
end

module GateConfig = struct
  type t = {
      name: string ;
      parameters : string list ;
      qasm_def: string ;
      coupling_map : (CouplingMap.t option [@default None]) ;
      conditional: (bool [@default false]) ;
      latency_map : (Enum01.t list list option [@default None]);
      description : (string option [@default None]) ;
    } [@@deriving yojson, sexp]
end
(*
 "backend_name"
 "backend_version"
 "basis_gates"
 "conditional"
 "gates"
 "local"
 "max_shots"
 "memory"
 "n_qubits"
 "simulator"
 *)

module BackendVersion = struct
  type t = string [@@deriving yojson, sexp]

  let version_re = Pcre.regexp "[0-9]+.[0-9]+.[0-9]+$"
  let of_string s =
    if Pcre.pmatch ~rex:version_re s then
      s
    else failwith "BackendVersion.of_string: called with invalid version string"

  let to_string s = s
end

module CoreConfig = struct
  type t = {
      backend_name : string ;
      backend_version : BackendVersion.t ;
      basis_gates : string list ;
      conditional : (bool [@default false]) ;
      gates : GateConfig.t list ;
      local : bool ;
      max_shots : int ;
      memory : (bool [@default false]) ;
      n_qubits : int ;
      simulator : (bool [@default false]) ;

      open_pulse : bool ;
      url: (string option [@default None]) ;
      allow_q_object : bool ;

      sample_name : (string option [@default None]) ;
      coupling_map : (CouplingMap.t option [@default None]) ;
      max_experiments : (int [@default 1]) ;
      n_registers : (int [@default 1]) ;
      register_map : (Enum01.t list list option [@default None]) ;
      configurable : (bool [@default false]) ;
      credits_required : (bool [@default false]) ;
      online_date : (string option [@default None]) ;
      display_name : (string option [@default None]) ;
      description : (string option [@default None]) ;
      tags : (string list [@default []]);
    } [@@deriving yojson, sexp]
end

module BackendConfig = struct
  type t = CoreConfig.t list [@@deriving yojson, sexp]
end
