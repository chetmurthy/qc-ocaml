open Misc_functions
open Qc_environment
open Qrpc_api

module Login = struct
  type t = {
      rcfile : string option ;
      (** specify rcfile location *)

      key : string option ;
      (** specify section in rcfile *)
    } [@@deriving cmdliner,show]

  let login p =
    let rcfile = match p.rcfile with
      | Some f -> f
      | None -> Defaults._DEFAULT_QISKITRC_FILE in

    let creds = Credentials.mk() in
    Credentials.add_rcfile ~fname:rcfile creds ;
    let session = match p.key with
      | None -> Session.mk creds
      | Some key -> Session.mk ~key creds in
    Session.obtain_token session ;
    let url = Session.get_backends_url session in
    let token = Session.access_token session in
    Printf.printf "url: %s\n" url ;
    Printf.printf "access_token: %s\n" token ;
    ()

  let cmd =
    let term = Cmdliner.Term.(const login $ cmdliner_term ()) in
    let info = Cmdliner.Term.info "login" in
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

  let cmd1_term = Cmdliner.Term.(const print_params $ params_cmdliner_term ()) in
  let cmd1_info = Cmdliner.Term.info "cmd1" in

  let cmd2_term = Cmdliner.Term.(const print_params $ params_cmdliner_term ()) in
  let cmd2_info = Cmdliner.Term.info "cmd2" in

  let cmd3_term = Cmdliner.Term.(const print_params $ params_cmdliner_term ()) in
  let cmd3_info = Cmdliner.Term.info "cmd3" in

  Cmdliner.Term.(exit @@ eval_choice (cmd1_term, cmd1_info) [Login.cmd; (cmd1_term, cmd1_info); (cmd2_term, cmd2_info); (cmd3_term, cmd3_info)])

;;
