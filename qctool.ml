open Coll
open Misc_functions
open Qc_environment
open Qrpc_types
open Qobj_types
open Qrpc_api
open Qasm_io
open Qobj_compile

module Login = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
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

  let cmd n =
    let term = Cmdliner.Term.(const do_login $ cmdliner_term ()) in
    let info = Cmdliner.Term.info n in
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

let string_list_term = Cmdliner.Arg.(value & pos_all string [] & info [] ~docv:"JOB IDS" ~doc:"job ids")


module ShowJob = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      job_ids : string list ; [@term string_list_term]
      (** job ids to show *)

      verbose : bool ;
      (** print job verbosely (or succinctly) *)

    } [@@deriving cmdliner,show]

  let print_short_job_status ~session st =
    let ShortJobStatus.{ kind ; status ; creationDate ; id } = st in
    let id_toprint =
      if LM.in_rng session.Session.diary id then
        let userkey = List.hd (LM.inv session.Session.diary id) in
        Printf.sprintf "%s [aka \"%s\"]" id (String.escaped userkey)
          else id in
    let kind = match kind with None -> "<none>" | Some s -> s in
    Printf.printf "%s: %s\n\t%s @ %s\n" id_toprint status kind creationDate ;
    do_option (fun i ->
        InfoQueue.(Printf.printf "\t[ %s%s ]\n" i.status 
                     (match i.position with
                      | None -> ""
                      | Some n ->
                         Printf.sprintf " %d" n)))
      st.ShortJobStatus.infoQueue

  let do_show_job p =
    let { rcfile ; key ; debug ; job_ids ; verbose } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    List.iter (fun job_id ->
        let job_id =
          if LM.in_dom session.Session.diary job_id then
            LM.map session.Session.diary job_id
          else job_id in
        if verbose then
          let j = Job.get_job job_id session in
          display_response JobStatus.to_yojson j
        else
          let j = Job.get_status_job job_id session in
          match j with
          | Result.Ok st ->
             print_short_job_status ~session st
          | Result.Error apierror ->
             print_string "APIError: " ;
             apierror |> APIError.to_yojson |> Yojson.Safe.pretty_to_channel stdout
      ) job_ids

  let cmd =
    let term = Cmdliner.Term.(const do_show_job $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "show_job" in
    (term, info)
end

module MonitorJob = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      visual : bool ;
      (** try to be nice visually *)

      job_ids : string list ; [@term string_list_term]
      (** job ids to show *)

    } [@@deriving cmdliner,show]

  let do_monitor_job p =
    let { rcfile ; key ; debug ; job_ids ; visual } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    let job_ids =
      List.map (fun job_id ->
          if LM.in_dom session.Session.diary job_id then
            LM.map session.Session.diary job_id
          else job_id) job_ids in
    let rec monrec job_ids cnt =
      let job_ids = List.sort Pervasives.compare job_ids in
      let statuses =
        List.map (fun job_id ->
            (job_id, Job.get_status_job job_id session)) job_ids in
      if visual then (
        ignore (Sys.command "tput clear") ;
        ignore (Sys.command "tput cup 0 0")) ;
      let job_ids =
        List.fold_left (fun acc (job_id, j) ->
            match j with
            | Result.Ok st ->
               if st.ShortJobStatus.status = "RUNNING" then (
                 Printf.printf "[%d] " cnt ;
                 ShowJob.print_short_job_status ~session st ;
                 job_id::acc
               )
               else acc
            | Result.Error apierror ->
               print_string "APIError: " ;
               apierror |> APIError.to_yojson |> Yojson.Safe.pretty_to_channel stdout ;
               job_id::acc
          ) [] statuses in
      flush stdout ;
      if [] = job_ids then () else (
        Unix.sleep 10 ;
        monrec job_ids (cnt+1)
      )
    in
    monrec job_ids 0

  let cmd =
    let term = Cmdliner.Term.(const do_monitor_job $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "monitor" in
    (term, info)
end

module ShowResult = struct
  type t = {
      rcfile : string option ; [@env "QISKITRC"]
      (** specify rcfile location *)

      key : string option ; [@env "QISKIT_IDENTITY"]
      (** specify section in rcfile *)

      debug : bool ;
      (** turn on all debugging & logging *)

      job_ids : string list ; [@term string_list_term]
      (** job id to show *)

      verbose : bool ;
      (** print job verbosely (or succinctly) *)

    } [@@deriving cmdliner,show]

  let cleanup_key width k =
    let n = int_of_string k in
    let bits = Bytes.make width '0' in
    for i = 0 to (width - 1) do
      if n land (1 lsl i) <> 0 then
        Bytes.set bits (width - i - 1) '1' ;
    done ;
    "0b"^(Bytes.to_string bits)

  let width l =
    let wbits = List.fold_left (lor) 0 l in
    let rec wrec n =
      if ((1 lsl n) -1) >= wbits then n
      else wrec (n+1)
      in wrec 1

let cleanup_counts l =
  let width = width (List.map (fun (k,_) -> int_of_string k) l) in
  List.map (fun (k, j) -> (cleanup_key width k, j)) l

  let cleanup_data_t d =
    ResultObj.{
        d with
        counts =
          match d.counts with
          | Some (`Assoc l) -> Some(`Assoc (cleanup_counts l))
          | d -> d
    }

  let do_result p =
    let { rcfile ; key ; debug ; job_ids ; verbose } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    List.iter (fun job_id ->
        let job_id =
          if LM.in_dom session.Session.diary job_id then
            LM.map session.Session.diary job_id
          else job_id in
        let rsp = Job.get_job job_id session in
        match rsp with
        | Result.Ok st -> (
          match st.JobStatus.qObjectResult with
          | None -> Printf.printf "No results yet\n"
          | Some r ->
             if verbose then
               r |> QObjResult.to_yojson |> Yojson.Safe.pretty_to_channel stdout
             else
               List.iter (fun res ->
                   let name = match res.ResultObj.header.Experiment.name with
                     | None -> "<no name>"
                     | Some s -> s in
                   Printf.printf "%s:\n\t" name ;
                   res.ResultObj.data
                   |> cleanup_data_t
                   |> ResultObj.data_t_to_yojson
                   |> Yojson.Safe.pretty_to_channel stdout
                 ) r.QObjResult.results ;
             print_newline () 
        )
        | Result.Error apierror ->
           print_string "APIError: " ;
           apierror |> APIError.to_yojson |> Yojson.Safe.pretty_to_channel stdout;
           print_newline ()
      ) job_ids

  let cmd =
    let term = Cmdliner.Term.(const do_result $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "result" in
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

      job_ids : string list ; [@term string_list_term]
      (** job id to show *)

    } [@@deriving cmdliner,show]

  let do_cancel_job p =
    let { rcfile ; key ; debug ; job_ids } = p in
    let session = Login.(login { rcfile ; key ; debug }) in
    List.iter (fun job_id ->
        let job_id =
          if LM.in_dom session.Session.diary job_id then
            LM.map session.Session.diary job_id
          else job_id in
        let j = Job.cancel_job job_id session in
        display_response CancelResult.to_yojson j
      ) job_ids

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

      user_key : (string option [@default None]) ;
      (** optional key the user can supply to be stored in diary *)

      shots : int ; [@default 1024]
      (** number of shots *)

      max_credits : int ; [@default 10]
      (** max credits *)

      include_path : string list ; [@default []] [@sep ':'] [@aka ["I"]]

    } [@@deriving cmdliner,show]

  let do_submit_job p =
    let { rcfile ; key ; debug ; backend ; qasmfile ; name ; shots ;
          max_credits ; include_path ; user_key } = p in
    let session = Login.(login { rcfile ; key ; debug }) in

    let (envs, dag) = full_to_dag0_from_file ~path:include_path qasmfile in
    let (qobj: Qobj_types.Qobj.t) = Compile.circuits_to_qobj ~backend_name:backend
                                      ~shots ~max_credits
                                      ~memory:false [name, envs, dag] in

    let status = Job.submit_job backend qobj ~user_key:user_key session in
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
      List.iter (ShowJob.print_short_job_status ~session) l
    else
      List.iter ShortJobStatus.(fun { id } ->
      Printf.printf "%s\n" id
        ) l

  let cmd =
    let term = Cmdliner.Term.(const do_list_jobs $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "list_jobs" in
    (term, info)
end

let _ =
  if invoked_as "qctool" then

    Cmdliner.Term.(exit @@ eval_choice (Login.cmd "qctool") [
                               AvailableBackends.cmd;
                               CancelJob.cmd;
                               ListJobs.cmd;
                               Login.cmd "login";
                               MonitorJob.cmd;
                               ShowJob.cmd;
                               ShowResult.cmd;
                               SubmitJob.cmd;
    ])
;;
