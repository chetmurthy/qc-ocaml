(* Copyright 2019 Chetan Murthy, All rights reserved. *)

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

    let _DEFAULT_DAG0_BASIS = [
        ("CX", (2, 0, 0));
        ("U", (1, 0, 3));
        ("barrier", (-1, 0, 0));
        ("measure", (1, 1, 0));
        ("reset", (1, 0, 0))
      ]

    let _QOBJ_VERSION = "1.0.0"

    let _qelib_gates = [
        "u3"; "u2"; "u1"; "cx"; "id"; "x"; "y"; "z"; "h";
        "s"; "sdg"; "t"; "tdg"; "cz"; "cy"; "ccx"; "cu1";
        "cu3"; "swap"; "cswap"; "u0"; "rx"; "ry"; "rz";
        "ch"; "crz"; "rzz"]

end
