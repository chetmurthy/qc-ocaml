open Coll
open Misc_functions
open Qc_environment
open Qrpc_types
open Qrpc_api
open Qasm_io
open Qobj_compile

module Login = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ;
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)
    } [@@deriving cmdliner,show]

  let login p =
    Nethttp_client.Debug.enable := p.debug ;
    let rcfile = match p.rcfile with
      | Some f -> f
      | None -> Defaults._DEFAULT_QISKITRC_FILE in

    let creds = Credentials.mk() in
    Credentials.add_rcfile ~fname:rcfile creds ;
    let session = match p.key with
      | None -> Session.mk creds
      | Some key -> Session.mk ~key creds in
    Session.obtain_token session ;
    session


  let do_login p =
    let session = login p in
    let key = Session.key session in
    let url = Session.get_backends_url session in
    let token = Session.access_token session in
    Printf.printf "key: %s\n" key ;
    Printf.printf "url: %s\n" url ;
    Printf.printf "access_token: %s\n" token ;
    ()

  let cmd =
    let term = Cmdliner.Term.(const do_login $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "login" in
    (term, info)
end

module AvailableBackends = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      verbose : bool ;
      (** print backend list verbosely *)
    } [@@deriving cmdliner,show]

  let do_available_backends p =
    let { rcfile ; key ; debug; verbose } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    let l = Session.available_backends session in
    if not verbose then
      l
      |> LM.dom
      |> List.sort Pervasives.compare
      |> String.concat " "
      |> Printf.printf "backends: %s\n"
    else
      LM.app (fun k v ->
          Printf.printf "%s: " k ;
          v |> CoreConfig.to_yojson |> Yojson.Safe.pretty_to_channel stdout ;
          Printf.printf "\n" ;
        ) l

  let cmd =
    let term = Cmdliner.Term.(const do_available_backends $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "available_backends" in
    (term, info)
end

let display_response demarsh rsp =
  match rsp with
  | Result.Ok cr ->
     cr |> demarsh |> Yojson.Safe.pretty_to_channel stdout ;
     print_newline () ;
  | Result.Error apierror ->
     print_string "APIError: " ;
     apierror |> APIError.to_yojson |> Yojson.Safe.pretty_to_channel stdout;
     print_newline ()

module ShowJob = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      job_id : string ;
      (** job id to show *)

      verbose : bool ;
      (** print job verbosely (or succinctly) *)

    } [@@deriving cmdliner,show]

  let do_show_job p =
    let { rcfile ; key ; debug ; job_id ; verbose } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    if verbose then
      let j = Job.get_job job_id session in
      display_response JobStatus.to_yojson j
    else
      let j = Job.get_status_job job_id session in
      match j with
      | Result.Ok st ->
         let ShortJobStatus.{ kind ; status ; creationDate ; id } = st in
         let kind = match kind with None -> "<none>" | Some s -> s in
         Printf.printf "%s: %s\n\t%s @ %s\n" id status kind creationDate
      | Result.Error apierror ->
         print_string "APIError: " ;
         apierror |> APIError.to_yojson |> Yojson.Safe.pretty_to_channel stdout

  let cmd =
    let term = Cmdliner.Term.(const do_show_job $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "show_job" in
    (term, info)
end

module CancelJob = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      job_id : string ;
      (** job id to show *)

    } [@@deriving cmdliner,show]

  let do_cancel_job p =
    let { rcfile ; key ; debug ; job_id } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    let j = Job.cancel_job job_id session in
    display_response CancelResult.to_yojson j

  let cmd =
    let term = Cmdliner.Term.(const do_cancel_job $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "cancel_job" in
    (term, info)
end

module SubmitJob = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      qasmfile : string ;
      (** qasmfile to submit *)

      name : string ;
      (** experiment-name *)

      backend : string ;
      (** the backend to submit to *)

      shots : int ; [@default 1024]
      (** number of shots *)

      max_credits : int ; [@default 10]
      (** max credits *)

      include_path : string list ; [@default []] [@sep ':'] [@aka ["I"]]

    } [@@deriving cmdliner,show]

  let do_submit_job p =
    let { rcfile ; key ; debug ; backend ; qasmfile ; name ; shots ; max_credits ; include_path } = p in
    let session = Login.(login { rcfile ; key ; debug }) in

    let (envs, dag) = full_to_dag0_from_file ~path:include_path qasmfile in
    let (qobj: Qobj_types.Qobj.t) = Compile.circuits_to_qobj ~backend_name:backend
                                      ~shots ~max_credits
                                      ~memory:false [name, envs, dag] in

    let status = Job.submit_job backend qobj session in
    display_response JobStatus.to_yojson status

  let cmd =
    let term = Cmdliner.Term.(const do_submit_job $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "submit_job" in
    (term, info)
end

module ListJobs = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      verbose : bool ;
      (** print backend list verbosely *)

      backend : string ;
      (** the backend to list jobs for *)

      status : string option ;
      (** print only hobs with thie specified status *)

    } [@@deriving cmdliner,show]

  let do_list_jobs p =
    let { rcfile ; key ; debug; verbose ; backend; status } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    let filter = match status with
      | None -> []
      | Some s -> ["status", s] in
    let l = Job.get_status_jobs ~filter ~backend session in

    if verbose then
      List.iter ShortJobStatus.(fun { kind ; status ; creationDate ; id } ->
      let kind = match kind with None -> "<none>" | Some s -> s in
      Printf.printf "%s : %s\n\t%s @ %s\n" id status kind creationDate
        ) l
    else
      List.iter ShortJobStatus.(fun { id } ->
      Printf.printf "%s\n" id
        ) l

  let cmd =
    let term = Cmdliner.Term.(const do_list_jobs $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "list_jobs" in
    (term, info)
end

type params = {
  username: string;
  (** Your Github username *)

  api_key: string;
  (** Your Github API key *)

  command: string; [@pos 0] [@docv "CMD"]
  (** The Github API command to run *)

  dry_run: bool;
  (** Don't really run this command *)

  time_to_wait: float; [@default 0.]
  (** Just an example of another type *)
} [@@deriving cmdliner,show]

let print_params p =
  print_string (show_params p) ;
  print_newline()

let _ =
if invoked_as "qctool" then

  Cmdliner.Term.(exit @@ eval_choice Login.cmd [
                             Login.cmd;
                             AvailableBackends.cmd;
                             ListJobs.cmd;
                             ShowJob.cmd;
                             CancelJob.cmd;
                             SubmitJob.cmd;
  ])

;;
