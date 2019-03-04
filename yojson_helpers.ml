open Misc_functions

type user_comparator_t = cmp0:(Yojson.Safe.json -> Yojson.Safe.json -> bool) -> Yojson.Safe.json -> Yojson.Safe.json -> bool

let (default_usercmp : user_comparator_t) = fun ~cmp0 f1 f2 -> false

let rec canon (j : Yojson.Safe.json) =
  match j with
  | `Assoc l ->
     let l = List.map (fun (k,j) -> (k,canon j)) l in
     `Assoc (List.sort Pervasives.compare l)
     
  | `List l ->
     let l = List.map canon l in
     `List l
     
  | `Tuple l ->
     let l = List.map canon l in
     `Tuple l
     
  | `Variant(s1,j) -> `Variant(s1,map_option canon j)
  | j -> j

let compare ?(explain=false) ?(usercmp=default_usercmp) (j1: Yojson.Safe.json) (j2: Yojson.Safe.json) =
let rec cmprec l1 l2 =
  if usercmp ~cmp0:cmprec0 l1 l2 then true
  else cmprec0  l1  l2

  and cmprec0 l1  l2 =
    let eq =
      match l1, l2 with
      | `Null, `Null -> true
      | `Bool b1, `Bool b2 -> b1 = b2
      | `Int n1, `Int n2 -> n1=n2
      | `Intlit n1, `Intlit n2 -> n1=n2
      | `Float f1,`Float f2 -> f1=f2
      | `String s1,`String s2 -> s1=s2
      | `Assoc l1, `Assoc l2 ->
         List.length l1 = List.length l2 &&
           List.for_all2 (fun (ka,va) (kb,vb) -> ka=kb && cmprec va vb) l1 l2
        
      | `List l1, `List l2 ->
         List.length l1 = List.length l2 &&
           List.for_all2 (fun a b -> cmprec a b) l1 l2
        
      | `Tuple l1, `Tuple l2 ->
         List.length l1 = List.length l2 &&
           List.for_all2 (fun a b -> cmprec a b) l1 l2
        
      | `Variant(s1,jopt1), `Variant(s2, jopt2) ->
         s1=s2 &&
           (match jopt1, jopt2 with
            | Some j1, Some j2 -> cmprec j1 j2
            | _ -> false)
      | _ -> false
    in
    if not eq && explain then
      Printf.fprintf stderr "\nJSON NOT EQUAL\n================\n%s\n================\n%s\n================\n"
        (Yojson.Safe.pretty_to_string l1) (Yojson.Safe.pretty_to_string l2) ;
    eq
in
cmprec j1 j2

