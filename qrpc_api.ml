
module Configfile = struct
  open Inifiles
  let mk() = new Inifiles.inifile "/home/chet/.qiskit/qiskitrc"
end

module Credentials = struct
  let _REGEX_IBMQ_HUBS = Pcre.regexp ~flags:[`CASELESS] (
      "(http[s]://.+/api)"^
        "/Hubs/([^/]+)/Groups/([^/]+)/Projects/([^/]+)"
    )
        
  let _unify_ibmq_url ?hub ?group ?project url =
    try
      let [|_; _; hub; group; project|] = Pcre.extract ~rex:_REGEX_IBMQ_HUBS url in
      (url, Some hub, Some group, Some project)
    with Not_found ->
      (match (hub, group, project) with
       | (Some h, Some g, Some p) ->
          let url = Printf.sprintf "%s/Hubs/%s/Groups/%s/Projects/%s" url h g p in
          (url, hub, group, project)
       | _ -> (url, None, None, None))
    

  type single_t ={
      token : string ;
      url : string ;
      hub : string option ;
      group : string option ;
      project : string option ;
      verify : bool ;
    }
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
