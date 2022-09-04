open Sexplib0.Sexp_conv
open Yojson_helpers
open Qobj_types


module APIError = struct
  type error_t = {
      name : (string option [@sexp.default None] [@yojson.default None]) ;
      status : int ;
      message : string ;
      statusCode : (int option [@sexp.default None][@yojson.default None]) ;
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
      parameters : string list option ;
      qasm_def: string option [@sexp.default None][@yojson.default None] ;
      coupling_map : (CouplingMap.t option [@sexp.default None][@yojson.default None]) ;
      conditional: (bool [@sexp.default false][@yojson.default false]) ;
      latency_map : (Enum01.t list list option [@sexp.default None][@yojson.default None]);
      description : (string option [@sexp.default None][@yojson.default None]) ;
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
  let sexp_of_json _ = Sexplib0.Sexp.Atom "<opaque>"
  type t = {
      allow_object_storage : bool ;
      allow_q_object : bool ;
      backend_name : string ;
      backend_version : BackendVersion.t ;
      basis_gates : string list ;
      gates : GateConfig.t list ;
      input_allowed : string list ;
      live_data : bool ;
      local : bool ;
      max_shots : int ;
      n_qubits : int ;
      open_pulse : bool ;
      pulse_num_channels : int ;
      pulse_num_qubits : int ;

      acquisition_latency : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      channels : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      clops : int option [@sexp.default None][@yjson.default None] ;
      conditional : (bool [@sexp.default false][@yjson.default false]) ;
      conditional_latency : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      coupling_map : (CouplingMap.t option [@sexp.default None][@yjson.default None]) ;
      credits_required : bool [@sexp.default false][@yjson.default false] ;
      default_rep_delay : int option [@sexp.default None][@yjson.default None] ;
      description : (string option [@sexp.default None][@yjson.default None]) ;
      discriminators : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      dt : float option  [@sexp.default None][@yjson.default None] ;
      dtm : float option  [@sexp.default None][@yjson.default None] ;
      dynamic_reprate_enabled : bool [@sexp.default false][@yjson.default false] ;
      hamiltonian : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      max_experiments : (int [@sexp.default 1][@yjson.default 1]) ;
      meas_kernels : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      meas_levels : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      meas_lo_range : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      meas_map : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      measure_esp_enabled : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      memory : (bool [@sexp.default false][@yjson.default false]) ;
      multi_meas_enabled : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      n_registers : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      n_uchannels : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      online_date : (string option [@sexp.default None][@yjson.default None]) ;
      parallel_compilation : (bool [@sexp.default false][@yjson.default false]) ;
      parametric_pulses : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      processor_type : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      processorType
      : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      public : (bool [@sexp.default false][@yjson.default false]) ;
      quantum_volume : int option [@sexp.default None][@yjson.default None] ;
      qubit_channel_mapping : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      qubit_lo_range : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      rep_delay_range : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      rep_times : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      revision : (string option [@sexp.default None][@yjson.default None]) ;
      sample_name : string option [@sexp.default None][@yjson.default None] ;
      simulation_method : string option [@sexp.default None][@yjson.default None] ;
      simulator : (bool [@sexp.default false][@yjson.default false]) ;
      supported_features : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      supported_instructions : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      timing_constraints : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      u_channel_lo : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      uchannels_enabled : (json [@sexp.opaque]) option [@sexp.default None][@yjson.default None] ;
      url: (string option [@sexp.default None][@yjson.default None]) ;
    } [@@deriving yojson, sexp_of]
end

module BackendConfig = struct
  type t = CoreConfig.t list [@@deriving yojson, sexp_of]
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
      counts : (json option [@yojson.default None]) ;
      snapshots : (json option [@yojson.default None]) ;
      memory : (json option [@yojson.default None]) ;
      statevector : (json option [@yojson.default None]) ;
      unitary : (json option [@yojson.default None]) ;
    } [@@deriving yojson]

  type t = {
      shots : shots_t ;
      success : bool ;
      header : Experiment.header_t ;
      data : data_t ;
      status : (string option [@yojson.default None]) ;
      job_id : (string option [@yojson.default None]) ;
      seed : (int option [@yojson.default None]) ;
      meas_return : (string option [@yojson.default None]) ;
    } [@@deriving yojson]
end

module QObjResult = struct
  type t = {
      backend_name : string ;
      backend_version : string ;
      qobj_id : string ;
      job_id : string ;
      results : ResultObj.t list ;

      status : (string option [@yojson.default None]) ;
      date : (string option [@yojson.default None]) ;
      success : bool ;
      header : (json option [@yojson.default None]);
      execution_id : (string option [@yojson.default None]) ;
      
    } [@@deriving yojson]
end

module InfoQueue = struct
  type t = {
      status: string ;
      position: (int option [@yojson.default None]) ;
    } [@@deriving yojson]
end

module ShortJobStatus = struct
  type t = {
      kind: string option [@yojson.default None] ;
      status : string ;
      creationDate : string ;
      id : string ;
      infoQueue : (InfoQueue.t option [@yojson.default None]) ;
    } [@@deriving yojson]

  type list_t = t list [@@deriving yojson]
end

module JobState = struct
  type t = {
      status : string ;
      executionId : string ;
      result : (string option [@yojson.default None]) ;
    } [@@deriving yojson]
end

module Calibration = struct
  type t = {
      id : string ;
      version : string ;
    } [@@deriving yojson]
end


module JobStatus = struct

  type t = {
      qasms: JobState.t list ;
      qObject : Qobj.t ;
      qObjectResult : (QObjResult.t option [@yojson.default None]) ;
      kind: string ;
      shots : int ;
      backend : BackendDescriptor.t ;
      status : string ;
      maxCredits : int ;
      usedCredits : int ;
      creationDate : string ;
      deleted : bool ;
      endDate : (string option [@yojson.default None]) ;
      totalTimeDevice : (float option [@yojson.default None]) ;
      ip : (IPInfo.t option [@yojson.default None]) ;
      id : string ;
      userId : string ;
      infoQueue : (InfoQueue.t option [@yojson.default None]) ;
      calibration : (Calibration.t option [@yojson.default None]) ;
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
