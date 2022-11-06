open Pa_ppx_utils
open Qc_misc

let dot_to_file fp p =
  let fname = Fpath.to_string fp in
  Std.apply_to_out_channel (fun oc -> Odot.print oc p) fname

module Exec = struct
  open Rresult
  open Bos
  let ( let* ) x f = Rresult.(>>=) x f

let generate dir dotobj =
  let dotf = Fpath.(dir // v "graph.dot") in
  let _ = dot_to_file dotf dotobj in
  Ok dotf

let xdot_cmd = Cmd.(v "xdot" % "-f" % "fdp")

let display dotf =
  let doit = Cmd.(xdot_cmd % (Fpath.to_string dotf)) in
  let* _ = OS.Cmd.(run_out doit |> to_string) in
  Ok ()

let generate_and_display dir dotobj =
  let* dotf = generate dir dotobj in
  let* () = display dotf in
  Ok dotf


let xdot ?(preserve=false) ?(display=true) dotobj =
  in_tmp_dir ~preserve (if display then generate_and_display else generate) dotobj


end
