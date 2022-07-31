(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Coll
open Yojson_helpers
open Misc_functions
open Sexplib0.Sexp_conv
open Qc_environment
open Qrpc_types

module Configfile = struct
  open Inifiles
  let read fname = new Inifiles.inifile fname
end

module Credentials = struct
  open Defaults
  module Single = struct
                         
    let _unify_ibmq_url (url, hub, group, project) =
      try
        let [|_; _; hub; group; project|] = Pcre.extract ~rex:_REGEX_IBMQ_HUBS url in
        (url, Some hub, Some group, Some project)
      with Not_found ->
        (match (hub, group, project) with
         | (Some h, Some g, Some p) ->
            let url = Printf.sprintf "%s/Hubs/%s/Groups/%s/Projects/%s" url h g p in
            (url, hub, group, project)
         | _ -> (url, None, None, None))

    type t = {
        token : string ;
        diary : string option ;
        url : string ;
        hub : string option ;
        group : string option ;
        project : string option ;
        verify : bool ;
      }

    let unique_id t = (t.hub, t.group, t.project)

    let get ini sect attr =
      let open Inifiles in
      try
        Some (ini # getval sect attr)
      with Invalid_element _ ->
        None

    let mk ~url ?(hub=None) ?(group=None) ?(project=None) ~verify ?(diary=None) ~token =
      { token ; url ; hub ; group ; project ; verify ; diary }

    let mk_from_ini ini sect =
      let url =
        match get ini sect "url" with
        | None -> _DEFAULT_IBMQ_URL_PREFIX
        | Some url -> url in
      let hub = get ini sect "hub" in
      let group = get ini sect "group" in
      let project = get ini sect "project" in
      let (url, hub, group, project) = _unify_ibmq_url (url, hub, group, project) in
      let verify =
        match get ini sect "verify" with
        | Some ("True"|"true") -> true
        | None -> failwith (Printf.sprintf "invalid ini file section %s (no verify attribute)" sect)
        | Some s -> failwith (Printf.sprintf "invalid ini file section %s (malformed verify attribute \"%s\")" sect s) in
      let token =
        match get ini sect "token" with
        | None -> failwith (Printf.sprintf "invalid ini file section %s (no token attribute)" sect)
        | Some s -> s in
      let diary = get ini sect "diary" in
      mk ~token ~url ~hub ~group ~project ~verify ~diary

  end

  type t = (string, Single.t) MLM.t

  let mk() = (MLM.mk() : t)
  let add_rcfile ?(fname=_DEFAULT_QISKITRC_FILE) accts =
    let ini = Configfile.read fname in
    let sects = ini # sects in
    List.iter (fun sect ->
        MLM.add accts (sect, Single.mk_from_ini ini sect))
       sects

  let export accts =
    accts
    |> MLM.toList
    |> List.sort Stdlib.compare

  let add_new ~key ~url ~token accts =
    let scred = Single.mk ~url ~token in
    MLM.add accts (key, scred)
end

module RPC = struct

open Nethttp_client;;
Debug.enable := true ;;
let pipeline = new pipeline ;;
let opts = pipeline # get_options in
let opts = { opts with
      verbose_status = true ;
      verbose_request_header = true ;
      verbose_response_header = true ;
      verbose_request_contents = true ;
      verbose_response_contents = true ;
      connection_timeout = 30.0 ;
               } in
    pipeline # set_options opts
;;

let post ~headers params url =
  let call = new post url params in
  call # set_request_header (Netmime.basic_mime_header headers) ;
  pipeline # add call;
  pipeline # run() ;
  call

let post_object ~headers ~body params url =
  let url =
    if params = [] then url
    else
      let l = List.map (fun (n,v) -> n ^ "=" ^ Netencoding.Url.encode v) params in
      let s = String.concat "&" l in
      url ^ "?" ^ s in
  let call = new post_raw url body in
  call # set_request_header (Netmime.basic_mime_header headers) ;
  pipeline # add call;
  pipeline # run() ;
  call

let get ~headers params url =
  let url =
    match params with
    | [] -> url
    | l ->
       Printf.sprintf "%s?%s" url
         (String.concat "&" (List.map (fun (k,v) -> Printf.sprintf "%s=%s" k v) l)) in
  
  let call = new get url in
  call # set_request_header (Netmime.basic_mime_header headers) ;
  pipeline # add call;
  pipeline # run() ;
  call

end

module Configuration = struct
  type capabilities_t = {
      openPulse: bool
    ; maxExperiments: int option [@default None]
    } [@@deriving yojson, sexp]

  type t = {
      limit: int
    ; capabilities: capabilities_t
    } [@@deriving yojson, sexp]
end

module Device = struct
  type t = {
      priority: int
    ; name: string
    ; deleted :  bool
    ; configuration: Configuration.t option [@default None]
    } [@@deriving yojson, sexp]
end

module Dict = struct
  type 'b t = (string * 'b) list [@@deriving sexp]

  let to_yojson b_to_yojson l =
    let l = List.map (fun (k,v) -> (k,b_to_yojson v)) l in
    `Assoc l

  type 'b _list = 'b list [@@deriving yojson, sexp]
  let of_yojson b_of_yojson j =
    let open Rresult.R in
    match j with
      `Assoc l ->
      let keys = List.map fst l in
      let jvals = List.map snd l in
      (_list_of_yojson b_of_yojson (`List jvals))
        >>= (fun vals ->
        Rresult.R.ok (List.map2 (fun k v -> (k,v)) keys vals))
    | _ -> Rresult.R.error "Dict.of_yojson"

end

module Project = struct

  type t = {
      name: string
    ; title: string
    ; isDefault: bool
    ; description: string
    ; creationDate: string
    ; deleted: bool
    ; devices: Device.t Dict.t
    ; priority: int
    ; users : string list
    } [@@deriving yojson, sexp]
end

module Group = struct

type projects_map_t = (string * Project.t) list

  type t = {
      name: string
    ; title: string
    ; isDefault: bool
    ; description: string
    ; creationDate: string
    ; deleted: bool
    ; projects : Project.t Dict.t
    ; priority: int
    } [@@deriving yojson, sexp]
end

module Provider = struct

type groups_map_t = (string * Group.t) list

type t = {
    name : string
  ; title : string
  ; description : string
  ; creationDate : string
  ; deleted : bool
  ; _private: bool [@key "private"]
  ; licenseNotRequired : bool
  ; isDefault: bool
  ; analytics: bool
  ; _class: string [@key "class"]
  ; priority: int
  ; id : string
  ; ownerId : string
  ; device: string list
  ; groups : Group.t Dict.t
  } [@@deriving yojson, sexp]
end

module Session  = struct

  type api_t = {
      api_auth : string [@key "api-auth"]
    ; api_app : string [@key "api-app"]
    ; api_utils : string [@key "api-utils"]
    ; version : string
    } [@@deriving yojson, sexp]

  type login_request_t = {
      api_token : string [@key "apiToken"]
    } [@@deriving yojson, sexp]

type token_t = {
    id: string ;
    ttl: int ;
    created: string ;
    userId: string;
  } [@@deriving yojson, sexp]

type services_t = {
    quantumLab: string
  ; runtime : string
}  [@@deriving yojson, sexp]

type urls_t = {
    http: string
  ; ws: string
  ; services : services_t
  } [@@deriving yojson, sexp]

type terms_t = {
    accepted: bool
  } [@@deriving yojson, sexp]

type login_account_t = {
    provider: string
  } [@@deriving yojson, sexp]

type user_info_t = {
    email: string
  ; username : string
  ; userType: string
  ; firstName: string
  ; lastName : string
  ; institution : string
  ; roles: json list
  ; ibmQNetwork : bool
  ; qNetworkRoles : json list
  ; canScheduleBackends: bool
  ; applications : string list
  ; urls: urls_t
  ; needsRefill: bool
  ; emailVerified : bool
  ; terms: terms_t
  ; loginAccounts : login_account_t list
  ; readOnly: bool
  ; id : string
} [@@deriving yojson]

type t = {
    key : string
  ; account : Credentials.Single.t
  ; mutable token : token_t option
  ; mutable diary : (string, string) LM.t
  ; mutable api : api_t option
  ; mutable user_info : user_info_t option
  }

module Diary = struct
  type t = (string * string) list [@@deriving yojson, sexp]

  let glob_expand s =
    if starts_with ~pat:"~/" s then
      (Sys.getenv "HOME") ^ "/" ^ (String.sub s 2 (String.length s - 2))
    else s

  let load session =
    match (session.account.Credentials.Single.diary) with
      Some s ->
       let s = glob_expand s in
       if Sys.file_exists s then
         s
         |> Yojson.Safe.from_file
         |> of_yojson
         |> error_to_failure ~msg:"QC_diary.of_yojson"
       else []
    | None ->
       []

  let dump session diary =
    match diary, (session.account.Credentials.Single.diary) with
    | [], None -> ()
    | _::_, None ->
       Exc.die "no diary-file specified in qiskitrc, but we need to write one"
    | _, Some fname ->
       let fname = glob_expand fname in
       diary
       |> to_yojson
       |> Yojson.Safe.to_file fname

end

let update_diary sess user_key job_id =
  (
    if LM.in_dom sess.diary user_key then
      sess.diary <- LM.remap sess.diary user_key job_id
    else
      sess.diary <- LM.add sess.diary (user_key, job_id) ;
  ) ;
  Diary.dump sess sess.diary

let mk ?key accounts =
  if MLM.size accounts = 0 then
    failwith "Session.mk: empty accounts" ;
  let key =
    match key with
    | Some key -> key
    | None when MLM.size accounts = 1 -> List.hd (MLM.dom accounts)
    | _ -> failwith "Session.mk: msut supply key into inifile" in
  let account = MLM.map accounts key in
  let sess = {
      key
    ; account
    ; token = None
    ; diary = LM.mk ()
    ; api = None
    ; user_info = None
    } in
  let diary = Diary.load sess in
  sess.diary <- diary ;
  sess

let _get_api session =
  let url = session.account.Credentials.Single.url ^ "/version" in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
    ] in
  let call = RPC.get ~headers [] url in
  let resp_body = call # get_resp_body() in
  let api = 
    resp_body
    |> Yojson.Safe.from_string
    |> api_t_of_yojson
    |> CCResult.get_exn in
  session.api <- Some api

let _obtain_token session =
  let url = session.account.Credentials.Single.url ^ "/users/loginWithToken" in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
      ("Content-type", "application/json") ;
    ] in
  let apiToken = session.account.Credentials.Single.token in
  let login_request_s = { api_token = apiToken } |> login_request_t_to_yojson |> Yojson.Safe.to_string in
  let call = RPC.post_object ~headers ~body:login_request_s [] url in
  let resp_body = call # get_resp_body() in
  let token =
    resp_body
    |> Yojson.Safe.from_string
    |> token_t_of_yojson
    |> CCResult.get_exn in
  session.token <- Some token

let access_token session =
  match session.token with
  | None -> failwith "access_token: no token (did you forget to run obtain_token?)"
  | Some t -> t.id

let urls session =
  match session.user_info with
  | None -> failwith "urls: no user_info (did you forget to run setup?)"
  | Some ui -> ui.urls

let _get_user_info session =
  let url = session.account.Credentials.Single.url ^ "/users/me" in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
      ("X-Access-Token", access_token session) ;
    ] in
  let call = RPC.get ~headers [] url in
  let resp_body = call # get_resp_body() in
  let user_info = 
    resp_body
    |> Yojson.Safe.from_string
    |> user_info_t_of_yojson
    |> CCResult.get_exn in
  session.user_info <- Some user_info

let _get_user_hubs session =
  let url = (urls session).http ^ "/Network" in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
      ("X-Access-Token", access_token session) ;
    ] in
  let call = RPC.get ~headers [] url in
  let resp_body = call # get_resp_body() in
  resp_body
(*
  let user_info = 
    resp_body
    |> Yojson.Safe.from_string
    |> user_info_t_of_yojson
    |> CCResult.get_exn in
  session.user_info <- Some user_info
 *)
let setup session =
  _get_api session ;
  _obtain_token session ;
  _get_user_info session 

let get_backends_url session =
  let open Credentials in
  let open Single in
  let account = session.account in
  match account.hub, account.group, account.project with
  | Some hub, Some group, Some project ->
     Printf.sprintf "%s/Network/%s/Groups/%s/Projects/%s/devices/v/1"
       account.url hub group project
  | _ -> Printf.sprintf "%s/Backends/v/1" account.url

let key session = session.key

let available_backends session =
  let url = get_backends_url session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
    ] in
  let token = access_token session in
  let call = RPC.get ~headers ["access_token", token] url in
  let resp_body = call # get_resp_body() in
  resp_body
  |> Yojson.Safe.from_string
  |> BackendConfig.of_yojson
  |> error_to_failure ~msg:"BackendConfig.of_yojson"
  |> List.map (fun c -> (c.CoreConfig.backend_name, c))
  |> LM.ofList ()

end

module Job = struct
  type job_query_where_t = (string * string) list  [@@deriving sexp]

  let job_query_where_t_to_yojson l =
    `Assoc (List.map (fun (k,v) -> (k,`String v)) l)

  let job_query_where_t_of_yojson j =
    try
      (match j with
       | `Assoc l ->
          Result.Ok(List.map (function
                        | (k, `String v) -> (k, v)
                        | _ -> failwith "caught"
                      ) l)
       | _ -> Result.Error "Qrpc_api.Job.job_query_where_t")
      with Failure _ -> Result.Error "Qrpc_api.Job.job_query_where_t"

  type job_query_t = {
      order : string ;
      limit : (int [@default 10]) ;
      skip : (int [@default 0]) ;
      where : job_query_where_t ;
    }  [@@deriving yojson, sexp]

let get_status_jobs ?(filter=[]) ?(limit=10) ?(skip=0) ~backend session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs/status" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
    ] in
  let query = {
      order = "creationDate DESC";
      limit ; skip ;
      where = ("backend.name", backend)::filter ;
    } in
  let j = job_query_t_to_yojson query in
  let query_s = Yojson.Safe.to_string j in
  let call = RPC.get ~headers [("access_token", token); ("filter", query_s)] url in
  let resp_body = call # get_resp_body() in
  resp_body
  |> Yojson.Safe.from_string
  |> ShortJobStatus.list_t_of_yojson
  |> error_to_failure ~msg:"ShortJobStatus.list_t_of_yojson"

let handle_response ~rpcname ~typename demarsh f =
  let call = f () in
  let resp_body = call # get_resp_body () in
  try
    resp_body
    |> Yojson.Safe.from_string
    |> demarsh
    |> error_to_failure ~msg:(Printf.sprintf "%s.of_yojson" typename)
    |> Rresult.R.ok
  with Nethttp_client.Http_error (code, body) ->
        Exc.warn (Printf.sprintf "%s: HTTP error code %d, body=%s" rpcname code body) ;
        body
        |> Yojson.Safe.from_string
        |> APIError.of_yojson
        |> error_to_failure ~msg:(Printf.sprintf "APIError.of_yojson while demarshalling errmsg of %s" rpcname)
        |> Rresult.R.error 
     | Failure msg as exn ->
        Exc.warn (Printf.sprintf "Failure during demarshalling %s during RPC %s: resp_body was %s"
                    typename rpcname resp_body) ;
        raise exn

let get_status_job id_job session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
    ] in
  let url = url ^ "/" ^ id_job ^ "/status" in
  handle_response ~rpcname:"Job.get_status_job" ~typename:"ShortJobStatus"
    ShortJobStatus.of_yojson
    (fun () -> RPC.get ~headers [("access_token", token)] url)

let get_job id_job session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
    ] in
  let url = url ^ "/" ^ id_job in
  handle_response ~rpcname:"Job.get_job" ~typename:"JobStatus"
    JobStatus.of_yojson
    (fun () -> RPC.get ~headers [("access_token", token)] url)

let cancel_job id_job session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
      ("Content-type", "application/json") ;
    ] in
  let url = url ^ "/" ^ id_job ^ "/cancel" in
  handle_response ~rpcname:"Job.cancel_job" ~typename:"CancelResult"
    CancelResult.of_yojson
    (fun () -> RPC.post_object ~headers ~body:"" [("access_token", token)] url)

let submit_job backend_name qobj ?(user_key=None) session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.28.0") ;
      ("Accept", "*/*") ;
      ("X-Qx-Client-Application", "qiskit/0.37.1") ;
      ("Content-type", "application/json") ;
    ] in
  let job = IBMJob.make_job ~backend_name qobj in
  let job_s =
    job
    |> IBMJob.to_yojson
    |> Yojson.Safe.to_string in
  let rv =
    handle_response ~rpcname:"Job.submit_job" ~typename:"JobStatus"
      JobStatus.of_yojson
      (fun () -> RPC.post_object ~headers ~body:job_s [("access_token", token)] url) in
  (
    match user_key, rv with
      (Some user_key), Result.Ok status ->
       let job_id = status.JobStatus.id in
       let diary = session.Session.diary in
       Session.update_diary session user_key job_id
  ) ;
  rv

end
