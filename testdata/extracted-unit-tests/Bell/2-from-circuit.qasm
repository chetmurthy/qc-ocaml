OPENQASM 2.0;
include "qelib1.inc";
qreg q[5];
creg c[5];
h q[2];
cx q[2],q[1];
measure q[1] -> c[1];
measure q[2] -> c[0];