OPENQASM 2.0;
include "qelib1.inc";
qreg q[2];

h q[0];
CX q[1],q[0];
