(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Coll
open Sexplib0.Sexp_conv

(* You get https support with this: *)
let () =
  Nettls_gnutls.init()

module Defaults = struct
  let _DEFAULT_QISKITRC_FILE =
    Printf.sprintf "%s/.qiskit/qiskitrc" (Sys.getenv "HOME")

    let _REGEX_IBMQ_HUBS = Pcre.regexp ~flags:[`CASELESS] (
                               "(http[s]://.+/api)"^
                                 "/Hubs/([^/]+)/Groups/([^/]+)/Projects/([^/]+)"
                             )
    let _DEFAULT_IBMQ_URL_PREFIX = "https://quantumexperience.ng.bluemix.net/api"

end

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

module Cookie = struct

(*

set-cookie-header = "Set-Cookie:" SP set-cookie-string
 set-cookie-string = cookie-pair *( ";" SP cookie-av )
 cookie-pair       = cookie-name "=" cookie-value
 cookie-name       = token
 cookie-value      = *cookie-octet / ( DQUOTE *cookie-octet DQUOTE )
 cookie-octet      = %x21 / %x23-2B / %x2D-3A / %x3C-5B / %x5D-7E
                       ; US-ASCII characters excluding CTLs,
                       ; whitespace DQUOTE, comma, semicolon,
                       ; and backslash
 token             = <token, defined in [RFC2616], Section 2.2>

 cookie-av         = expires-av / max-age-av / domain-av /
                     path-av / secure-av / httponly-av /
                     extension-av
 expires-av        = "Expires=" sane-cookie-date
 sane-cookie-date  = <rfc1123-date, defined in [RFC2616], Section 3.3.1>
 max-age-av        = "Max-Age=" non-zero-digit *DIGIT
                       ; In practice, both expires-av and max-age-av
                       ; are limited to dates representable by the
                       ; user agent.
 non-zero-digit    = %x31-39
                       ; digits 1 through 9
 domain-av         = "Domain=" domain-value
 domain-value      = <subdomain>
                       ; defined in [RFC1034], Section 3.5, as
                       ; enhanced by [RFC1123], Section 2.1
 path-av           = "Path=" path-value
 path-value        = <any CHAR except CTLs or ";">
 secure-av         = "Secure"
 httponly-av       = "HttpOnly"
 extension-av      = <any CHAR except CTLs or ";">


   cookie-header = "Cookie:" OWS cookie-string OWS
   cookie-string = cookie-pair *( ";" SP cookie-pair )

   token          = 1*<any CHAR except CTLs or separators>
       separators     = "(" | ")" | "<" | ">" | "@"
                      | "," | ";" | ":" | "\" | <">
                      | "/" | "[" | "]" | "?" | "="
                      | "{" | "}" | SP | HT

       CTL            = <any US-ASCII control character
                        (octets 0 - 31) and DEL (127)>


   OWS            = *( [ obs-fold ] WSP )
                    ; "optional" whitespace
   obs-fold       = CRLF


 *)

let semi_space = Pcre.regexp "; "
               
let cookie_string_re = Pcre.regexp "([^=]+)=([^\"]*|\"[^\"]+\")"

let _OWS_left_re = Pcre.regexp ~flags:[`DOTALL] "^(?:(?:\\r\\n)? )*(\\S.*)$"
let _OWS_right_re = Pcre.regexp ~flags:[`DOTALL] "^(.*\\S)(?:(?:\\r\\n)? )*$"

let clean_ows s =
  let rv1 = Pcre.extract ~rex:_OWS_left_re s in
  let s = rv1.(1) in
  let rv2 = Pcre.extract ~rex:_OWS_right_re s in
  rv2.(1)

let parse_cookie_pair pair =
  let a = Pcre.extract ~rex:cookie_string_re pair in
  (a.(1), a.(2))

let parse_cookie s =
  let s = clean_ows s in
  let l = Pcre.split ~rex:semi_space s in
  List.map parse_cookie_pair l

type cookie_attribute_t =
  | Expires of string
  | MaxAge of string
  | Domain of string
  | Path of string
  | Secure
  | HttpOnly
  | Extension of string

let split_av_re = Pcre.regexp "(Expires|Max-Age|Domain|Path)=(.*)"

let parse_cookie_av s =
  try
    let p = Pcre.extract ~rex:split_av_re s in
    let k = p.(1) in
    let v = p.(2) in
    match k with
  | "Expires" -> Expires v
  | "Max-Age" -> MaxAge v
  | "Domain" -> Domain v
  | "Path" -> Path v
  | _ -> Extension s
  with Not_found ->
        if s = "Secure" then Secure
        else if s = "HttpOnly" then HttpOnly
        else Extension s

let parse_set_cookie s =
  let cookie_pair::avl = Pcre.split ~rex:semi_space s in
  let (k,v) = parse_cookie_pair cookie_pair in
  (k,v,List.map parse_cookie_av avl)
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

let mk ~key accounts =
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

end

(*
DEBUG:qiskit.providers.ibmq.api.ibmqconnector:response body: b'{"id":"VuZbTHbbJIKvGMnvwkO67gK6Ni8uXsdrziQp68L3OdHHIgiaeyGiEsZFvdB0zWp1","ttl":1209600,"created":"2019-03-02T20:33:44.525Z","userId":"5c7773071c6030005246cf46"}'  
 *)
