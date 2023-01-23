(** -syntax camlp5o *)
open Pa_ppx_utils
open Std

let stream_of_lexer_eof_function eof_function lexer lexbuf =
let rec strec () =
    let tok = lexer lexbuf
    in if (eof_function tok) then [< >]
       else [< 'tok; strec() >]
in [< strec () >]

let list_of_stream_eof eofp strm =
let rec listrec acc = parser
  [< 't when  not (eofp t); strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let ploc_encl_with_comments loc1 loc2 =
  let loc = Ploc.encl loc1 loc2 in
  let com1 = Ploc.comment loc1 in
  let com2 = Ploc.comment loc2 in
  Ploc.with_comment loc (com1^com2)



(*
let stream_of_lexer_eof eoftok lexer lexbuf =
let rec strec () =
    let tok = lexer lexbuf
    in if (eoftok = tok) then [< >]
       else [< 'tok; strec() >]
in [< strec () >]

let stream_of_lexer lexer lexbuf =
let rec strec () =
    try
    let tok = lexer lexbuf
    in [< 'tok; strec() >]
    with Failure _ -> [< >]
in [< strec () >]

let list_of_stream strm =
let rec listrec acc = parser
  [< 't ; strm >] -> listrec (t::acc) strm
| [< >] -> List.rev acc
in listrec [] strm

let plist elem = 
  let rec plist_rec accum = parser
     [< e = elem; strm >] -> plist_rec (e::accum) strm
   | [< >]                         -> (List.rev accum)
  in plist_rec []
 *)
let ne_plist elem = parser
  [< e = elem; l = (plist elem) >] -> (e,l)
(*
let ne_plist_with_sep sep elem = 
 let rec do_rec = parser
  [< e = elem; l = (parser [< () = sep; l = do_rec >] -> l | [< >] -> []) >] -> e::l
 in do_rec

let plist_with_sep sep elem = parser
    [< l = (ne_plist_with_sep sep elem) >] -> l
  | [< >] -> []

 *)
let ne_plist_with_sep_function sep elem = 
 let rec do_rec = parser
  [< e = elem; rv = (parser [< f = sep; l = do_rec >] -> f e l | [< >] -> e) >] -> rv
 in do_rec

let clean_hws_left_re = Pcre.regexp ~flags:[`DOTALL] "^\\h*(\\H.*)?$"
let clean_hws_right_re = Pcre.regexp ~flags:[`DOTALL] "^(.*\\H)?\\h*$"
let nl_re = Pcre.regexp "\r"
let lf_re = Pcre.regexp "\n"
let cleanws ?(lf=false) s =
  let s = Pcre.substitute ~rex:nl_re ~subst:(fun s -> "") s in
  let s = if lf then Pcre.substitute ~rex:lf_re ~subst:(fun s -> "") s else s in
  let rv1 = Pcre.extract ~rex:clean_hws_left_re s in
  let s = rv1.(1) in
  let rv2 = Pcre.extract ~rex:clean_hws_right_re s in
  rv2.(1)

let lfs_re = Pcre.regexp "\n+"
let ws_re = Pcre.regexp " +"
let comma_ws_re = Pcre.regexp ", +"
let clean_all_left_re = Pcre.regexp ~flags:[`DOTALL] "^[\\h\\v]*([^\\h\\v].*)?$"
let clean_all_right_re = Pcre.regexp ~flags:[`DOTALL] "^(.*[^\\h\\v])?[\\h\\v]*$"
let collapse_ws s =
  let s = Pcre.substitute ~rex:lfs_re ~subst:(fun s -> "\n") s in
  let s = Pcre.substitute ~rex:ws_re ~subst:(fun s -> " ") s in
  let s = Pcre.substitute ~rex:comma_ws_re ~subst:(fun s -> ",") s in
  let rv1 = Pcre.extract ~rex:clean_all_left_re s in
  let s = rv1.(1) in
  let rv2 = Pcre.extract ~rex:clean_all_right_re s in
  rv2.(1)

(*
let fst (a, _) = a
let snd (_, b) = b

let rec sep_last = function
    [] -> failwith "sep_last"
  | hd::[] -> (hd,[])
  | hd::tl ->
      let (l,tl) = sep_last tl in (l,hd::tl)

let invoked_as name =
  let l = [name; name^".byte"; name^".native"] in
  let argv0 = Sys.argv.(0) in
  let path = Pcre.split ~rex:(Pcre.regexp "/") argv0 in
  let fname, _ = sep_last path in
  List.mem fname l
 *)
let rec prlist elem l = match l with 
    []   -> [< >]
  | h::t -> let e = elem h and r = prlist elem t in [< e; r >];;

let rec prlist_with_sep sep elem l = match l with
    []   -> [< >]
  | [h]  -> elem h
  | h::t ->
      let e = elem h and s = sep()
      in [< e; s; prlist_with_sep sep elem t >];;
(*
type ('a,'b) union = Inl of 'a | Inr of 'b

let finally f arg finf =
  let rv = try Inl(f arg) with e -> Inr e
  in (try finf arg (match rv with Inl v -> Some v | Inr _ -> None) with e -> ());
	match rv with
		Inl v -> v
	  | Inr e -> raise e

let apply_to_in_channel f fna =
  let ic = open_in fna in
    finally
      f
      ic
      (fun _ _ -> close_in ic)

let apply_to_out_channel f fna =
  let oc = try open_out fna with Failure _ -> failwith (Printf.sprintf "Cannot open file %s for write" fna) in
	try
	  let rv = f oc
	  in close_out oc; rv
	with exc -> (close_out oc; raise exc)
 *)
let file_contents fna =
  apply_to_in_channel
    (fun ic ->
       let len = in_channel_length ic in
       let cbuf = Bytes.create len in
	 really_input ic cbuf 0 len;
	 Bytes.to_string cbuf)
    fna
(*
let subset l1 l2 =
  let t2 = Hashtbl.create 151 in
    List.iter (fun x-> Hashtbl.add t2 x ()) l2;
    let rec look = function
    [] -> true
      | x::ll -> try Hashtbl.find t2 x; look ll
        with Not_found -> false
    in look l1

let same_members s1 s2 = subset s1 s2 && subset s2 s1

let rec uniquize = function
    [] -> []
  | (h::t) -> if List.mem h t then uniquize t else h::(uniquize t)
let make_set = uniquize

let add_set a fs = if List.mem a fs then fs else (a::fs)

let rec rmv_set a ls =
  match ls with
      (h::t) -> if h = a then t else h::(rmv_set a t)
    | _ -> failwith "listset__rmv"

let try_find f = 
 let rec try_find_f = function
     [] -> failwith "try_find"
   | h::t -> try f h with Failure _ -> try_find_f t
 in try_find_f

let filter p =
  let rec filter_aux = function
      [] -> []
    | x::l -> if p x then x::filter_aux l else filter_aux l
  in filter_aux

let rec last = function
    [] -> failwith "last"
  | x::[] -> x
  | x::l -> last l

let for_all p = 
 let rec for_all_p = function
     [] -> true | a::l -> p a && for_all_p l
 in for_all_p

let exists p l = not(for_all (function x -> not(p x)) l)

let push l x = (l := x :: !l)
let pop l =
    match !l with
    h::tl -> l := tl
  | [] -> invalid_arg "pop"
let top l = List.hd !l

let map_option f = function
    None -> None
  | Some x -> Some(f x)

let do_option f = function
    Some x -> f x
  | None -> ()

let isSome = function Some _ -> true | None -> false
let outSome (Some n) = n

let comp f g x = f (g x)

let rec distinct = function
    h::t -> (not (List.mem h t)) && distinct t
  | _ -> true

let interval n m = 
  let rec interval_n (l,m) =
    if n > m then l else interval_n (m::l,pred m)
  in interval_n ([],m)

let range = interval 1
 *)
let swap f a b = f b a
(*
let combine l1 l2 = List.map2 (fun a b -> (a,b)) l1 l2
let split l = (List.map fst l,List.map snd l)

let pp ppfun arg =
  let strm = ppfun arg in
  let l = list_of_stream strm in
  String.concat "" l

let intersect l1 l2 = filter (fun x -> List.mem x l2) l1

let diff_set l1 l2 =
  if l2 = [] then l1 else filter (fun x -> not (List.mem x l2)) l1
let subtract = diff_set

let starts_with ?(improper=false) ~pat s =
  let slen = String.length s in
  let patlen = String.length pat in
    if slen < patlen || (slen = patlen && not improper) then
      false
    else
      (String.sub s 0 patlen) = pat

let ends_with ~pat s =
  let slen = String.length s in
  let patlen = String.length pat in
    if slen < patlen then
      false
    else
      (String.sub s (slen-patlen) patlen) = pat

let pr_option f = function
    None -> [< >]
  | Some v -> f v
 *)
let error_to_failure ?(msg="") = function
  | Result.Error s -> 
    failwith (msg^": "^s)
  | Ok s -> s
