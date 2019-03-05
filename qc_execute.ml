(* Copyright 2019 Chetan Murthy, All rights reserved. *)

open Sexplib0.Sexp_conv

open Misc_functions
open Coll
open Qc_environment
open Qasmsyntax
open Qasmsyntax
open Qasmparser
open Qasmdag0
open Qasmpp
open Qc_symbolic
open Qobj_types

module Job = struct

  type backend_info_t = {
      name : string ;
    } [@@deriving yojson, sexp]

  type t = {
      qObject : Qobj.t ;
      backend : backend_info_t ;
    } [@@deriving yojson, sexp]

  let make_job ~backend_name qobj =
    let j = {
        qObject = qobj ;
        backend = { name = backend_name } ;
  } in
    j

end
