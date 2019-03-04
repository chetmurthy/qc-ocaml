
let default_floatcmp (f1: float) (f2: float) = Pervasives.compare f1 f2

let compare ?(floatcmp=default_floatcmp) (j1: Yojson.Safe.json) (j2: Yojson.Safe.json) =
let canon l = List.sort Pervasives.compare l in

let rec cmprec = function
    | `Null, `Null -> true
    | `Bool b1, `Bool b2 -> b1 = b2
    | `Int n1, `Int n2 -> n1=n2
    | `Intlit n1, `Intlit n2 -> n1=n2
    | `Float f1,`Float f2 -> floatcmp f1 f2 = 0
    | `String s1,`String s2 -> s1=s2
    | `Assoc l1, `Assoc l2 ->
       let l1 = canon l1 in
       let l2 = canon l2 in
       List.length l1 = List.length l2 &&
         List.for_all2 (fun (ka,va) (kb,vb) -> ka=kb && cmprec (va,vb)) l1 l2

    | `List l1, `List l2 ->
       List.length l1 = List.length l2 &&
         List.for_all2 (fun a b -> cmprec (a,b)) l1 l2

    | `Tuple l1, `Tuple l2 ->
       List.length l1 = List.length l2 &&
         List.for_all2 (fun a b -> cmprec (a,b)) l1 l2

    | `Variant(s1,jopt1), `Variant(s2, jopt2) ->
       s1=s2 &&
         (match jopt1, jopt2 with
          | Some j1, Some j2 -> cmprec (j1, j2)
          | _ -> false)
    | _ -> false
in
cmprec (j1, j2)

