let rec split_args cmd = function
  | "--" :: files -> List.rev cmd, files
  | [file] -> List.rev cmd, [file]
  | arg :: args -> split_args (arg :: cmd) args
  | [] -> failwith "please supply input arguments"
let split_args = split_args []

let comment_pattern = Re.Perl.compile_pat "^\\(\\*\\*(.*?)\\*\\)"
let envvar_pattern = Re.Perl.compile_pat {|(?:\$\(([^)]+)\)|\$\{([^}]+)\})|}

let envsubst s =
  let envlookup vname =
    match Sys.getenv_opt vname with
      Some v -> v
    | None -> failwith (Printf.sprintf "ya_wrap_ocamlfind: environment variable <<%s>> not found" vname) in
  let f g =
    match (Re.Group.get_opt g 1, Re.Group.get_opt g 2) with
      (None, None) -> s
    | (Some v, _) -> envlookup v
    | (None, Some v) -> envlookup v in
  Re.replace ~all:true envvar_pattern ~f s

let discover_args f =
  let f' = open_in f in
  let line1 = input_line f' in
  close_in f';
  match Re.exec_opt comment_pattern line1 with
  | None -> ""
  | Some group -> envsubst (Re.Group.get group 1)

let () = 
  let cmd, files =
    Array.to_list Sys.argv |> List.tl |> split_args in
  let cmd = Filename.quote_command (List.hd cmd) (List.tl cmd) in

  List.iter (fun f ->
      let extra = discover_args f in
      let cmd = Printf.sprintf "%s %s %s" cmd extra f in
      Printf.fprintf stderr "%s\n%!" cmd;
      ignore (Sys.command cmd))
    files
