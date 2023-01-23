let rec split_args cmd = function
  | "--" :: files -> List.rev cmd, files
  | [file] -> List.rev cmd, [file]
  | arg :: args -> split_args (arg :: cmd) args
  | [] -> failwith "please supply input arguments"
let split_args = split_args []

let comment_pattern = Re.Perl.compile_pat "^\\(\\*\\*(.*?)\\*\\)"

let discover_args f =
  let f' = open_in f in
  let line1 = input_line f' in
  close_in f';
  match Re.exec_opt comment_pattern line1 with
  | None -> ""
  | Some group -> Re.Group.get group 1

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
