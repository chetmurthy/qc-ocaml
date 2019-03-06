exception Die of int * string;;

let die ?rc:(rc=0) msg = raise (Die (rc,msg))

let warn msg = flush stdout; Printf.eprintf "[WARNING: %s]\n" msg; flush stderr

let inform msg = flush stdout; Printf.eprintf "[INFO: %s]\n" msg; flush stderr

let pp = function
    Die(rc,s) as x ->
      Printf.fprintf stderr "Uncaught exception: Die(%d, %s)\n" rc s

  | Assert_failure(f,s,e) as x ->
      Printf.fprintf stderr "Uncaught exception: Assert_failure(\"%s\",%d,%d)\n" f s e

  | Unix.Unix_error (e,s1,s2) ->
     Printf.fprintf stderr "Unix_error %s: %s, %s\n" (Unix.error_message e) s1 s2

  | x ->
      Printf.fprintf stderr "%s\n" (Printexc.to_string x)

let print ?die:(die=false) fct arg =
flush stdout; flush stderr;
  try
    fct arg
  with x ->
    Printexc.print_backtrace stderr ; flush stderr ;
    pp x ;
    flush stdout; flush stderr;
    if not die then
      raise x
    else match x with
	Die(rc,_) -> exit rc
      | _ -> raise x
