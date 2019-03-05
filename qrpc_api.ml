(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Coll
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

    let mk ~url ?(hub=None) ?(group=None) ?(project=None) ~verify ~token =
      { token ; url ; hub ; group ; project ; verify }

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
      mk ~token ~url ~hub ~group ~project ~verify

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
    |> List.sort Pervasives.compare

  let add_new ~key ~url ~token accts =
    let scred = Single.mk ~url ~token in
    MLM.add accts (key, scred)
end

module RPC = struct

open Nethttp_client;;
Debug.enable := true ;;

let post ~headers params url =
  let call = new post url params in
  call # set_request_header (Netmime.basic_mime_header headers) ;
  let pipeline = new pipeline in
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
  let pipeline = new pipeline in
  pipeline # add call;
  pipeline # run() ;
  call

end

module Session  = struct

type token_t = {
    id: string ;
    ttl: int ;
    created: string ;
    userId: string;
  } [@@deriving yojson, sexp]

type t = {
    account : Credentials.Single.t ;
    mutable token : token_t option ;
  }

let mk ?key accounts =
  if MLM.size accounts = 0 then
    failwith "Session.mk: empty accounts" ;
  let key =
    match key with
    | Some key -> key
    | None when MLM.size accounts = 1 -> List.hd (MLM.dom accounts)
    | _ -> failwith "Session.mk: msut supply key into inifile" in
  let account = MLM.map accounts key in
  {
    account ;
    token = None ;
  }

let obtain_token session =
  let url = session.account.Credentials.Single.url ^ "/users/loginWithToken" in
  let headers = [
      ("User-Agent", "python-requests/2.21.0") ;
      ("Accept", "*/*") ;
      ("x-qx-client-application", "qiskit-api-py") ;
    ] in
  let apiToken = session.account.Credentials.Single.token in
  let call = RPC.post ~headers ["apiToken", apiToken] url in
  let resp_body = call # get_resp_body() in
  let token =
    resp_body
    |> Yojson.Safe.from_string
    |> token_t_of_yojson
    |> CCResult.get_exn in
  session.token <- Some token

let get_backends_url session =
  let open Credentials in
  let open Single in
  let account = session.account in
  match account.hub, account.group, account.project with
  | Some hub, Some group, Some project ->
     Printf.sprintf "%s/Network/%s/Groups/%s/Projects/%s/devices/v/1"
       account.url hub group project
  | _ -> Printf.sprintf "%s/Backends/v/1" account.url

let access_token session =
  match session.token with
  | None -> failwith "access_token: no token (did you forget to run obtain_token?)"
  | Some t -> t.id

let available_backends session =
  let url = get_backends_url session in
  let headers = [
      ("User-Agent", "python-requests/2.21.0") ;
      ("Accept", "*/*") ;
      ("x-qx-client-application", "qiskit-api-py") ;
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

let get_status_jobs ?(filter=[]) ?(limit=10) ?(skip=0) ~backend ~session =
  let url = session.Session.account.Credentials.Single.url ^ "/Jobs" in
  let token = Session.access_token session in
  let headers = [
      ("User-Agent", "python-requests/2.21.0") ;
      ("Accept", "*/*") ;
      ("x-qx-client-application", "qiskit-api-py") ;
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
end
