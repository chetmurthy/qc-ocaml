include "qelib1.qli";

gate bell2 () p q =
let p = h p in
let (p, q) = CX p q in
(p, q)
;

let q0 = qubit() and q1 = qubit() in
let (q0, q1) = bell2 q0 q1 in
(q0, q1)
