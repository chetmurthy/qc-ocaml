OPENQASM 2.0;
include "qelib1.inc";
qreg q[14];
creg c[2];
u2(0,pi) q[1];
cx q[1],q[0];
barrier q[1],q[0];
measure q[1] -> c[1];
measure q[0] -> c[0];
