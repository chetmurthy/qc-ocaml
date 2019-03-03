open Coll

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

    type t ={
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

    let mk ini sect =
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
      { token ; url ; hub ; group ; project ; verify }

  end

    let mk ?(fname=_DEFAULT_QISKITRC_FILE) () =
      let ini = Configfile.read fname in
      let sects = ini # sects in
      List.fold_left (fun cmap sect ->
          LM.add cmap (sect, Single.mk ini sect))
      (LM.mk()) sects

  type t = (string option * string option * string option, Single.t) LM.t

end

type token_t = {
    id: string ;
    ttl: int ;
    created: string ;
    userId: string;
  } [@@deriving yojson]

(*
DEBUG:qiskit.providers.ibmq.api.ibmqconnector:response body: b'{"id":"VuZbTHbbJIKvGMnvwkO67gK6Ni8uXsdrziQp68L3OdHHIgiaeyGiEsZFvdB0zWp1","ttl":1209600,"created":"2019-03-02T20:33:44.525Z","userId":"5c7773071c6030005246cf46"}'  
 *)
