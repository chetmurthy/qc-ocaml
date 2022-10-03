include "qelib1.qli";

let q0 = qubit() and q1 = qubit() in
let q0 = h q0 in
let (q0, q1) = CX q0 q1 in
(q0, q1)
