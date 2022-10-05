OPENQASM 2.0;
// 1
include "qelib1.inc";
// 2
qreg q[2];

// 3
h q[0];
// 4
cx q[0],q[1];
