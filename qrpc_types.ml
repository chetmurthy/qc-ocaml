open Sexplib0.Sexp_conv
open Qobj_types


module APIError = struct
  type error_t = {
      name : (string option [@default None]) ;
      status : int ;
      message : string ;
      statusCode : (int option [@default None]) ;
      code : string ;
    } [@@deriving yojson, sexp]

  type t = {
      error : error_t ;
    } [@@deriving yojson, sexp]
end

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

module BackendDescriptor = struct
  type t = {
      id : string ;
      name : string ;
    } [@@deriving yojson, sexp]
end

module IPInfo = struct
  type t = {
      ip : string ;
      country : string ;
      continent : string ;
    } [@@deriving yojson, sexp]
end


module ResultObj = struct
  type shots_t =
    COUNT of int
  | RANGE of int * int
           
  let shots_t_to_yojson = function
    | COUNT n -> `Int n
    | RANGE(n,m) -> `List[`Int n; `Int m]

  let shots_t_of_yojson = function
    | `Int n -> Result.Ok(COUNT n)
    | `List[`Int n; `Int m] -> Result.Ok(RANGE(n,m))
    | _ -> Result.Error "ResultObj.shot_t_of_yojson"

  type data_t = {
      counts : (Yojson.Safe.json option [@default None]) ;
      snapshots : (Yojson.Safe.json option [@default None]) ;
      memory : (Yojson.Safe.json option [@default None]) ;
      statevector : (Yojson.Safe.json option [@default None]) ;
      unitary : (Yojson.Safe.json option [@default None]) ;
    } [@@deriving yojson]

  type t = {
      shots : shots_t ;
      success : bool ;
      header : Experiment.header_t ;
      data : data_t ;
      status : (string option [@default None]) ;
      job_id : (string option [@default None]) ;
      seed : (int option [@default None]) ;
      meas_return : (string option [@default None]) ;
    } [@@deriving yojson]
end

module QObjResult = struct
  type t = {
      backend_name : string ;
      backend_version : string ;
      qobj_id : string ;
      job_id : string ;
      results : ResultObj.t list ;

      status : (string option [@default None]) ;
      date : (string option [@default None]) ;
      success : bool ;
      header : Yojson.Safe.json ;
      execution_id : string ;
      
    } [@@deriving yojson]
end

module InfoQueue = struct
  type t = {
      status: string ;
      position: int ;
    } [@@deriving yojson]
end

module ShortJobStatus = struct
  type t = {
      kind: string option [@default None] ;
      status : string ;
      creationDate : string ;
      id : string ;
      infoQueue : (InfoQueue.t option [@default None]) ;
    } [@@deriving yojson]

  type list_t = t list [@@deriving yojson]
end

module JobState = struct
  type t = {
      status : string ;
      executionId : string ;
      result : (string option [@default None]) ;
    } [@@deriving yojson]
end

module JobStatus = struct

  type t = {
      qasms: JobState.t list ;
      qObject : Qobj.t ;
      qObjectResult : (QObjResult.t option [@default None]) ;
      kind: string ;
      shots : int ;
      backend : BackendDescriptor.t ;
      status : string ;
      maxCredits : int ;
      usedCredits : int ;
      creationDate : string ;
      deleted : bool ;
      ip : (IPInfo.t option [@default None]) ;
      id : string ;
      userId : string ;
      infoQueue : (InfoQueue.t option [@default None]) ;
    } [@@deriving yojson]

  type list_t = t list [@@deriving yojson]

end

module CancelResult = struct
  type t = {
      cancelled : bool ;
    } [@@deriving yojson]
end

module IBMJob = struct
  type backend_info_t = {
      name : string ;
    } [@@deriving yojson, sexp]

  type t = {
      qObject : Qobj.t ;
      backend : backend_info_t ;
    } [@@deriving yojson, sexp]

  let make_job ~backend_name qobj =
    let j = {
        qObject = qobj ;
        backend = { name = backend_name } ;
  } in
    j

end
