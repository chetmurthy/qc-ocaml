OPENQASM 2.0;
include "qelib1.inc";
qreg q[6];

h q[0];
h q[1];
cx q[2], q[3];
cx q[0], q[1];
h q[0];
h q[1];
swap q[1], q[2];
swap q[2], q[3];
cx q[3], q[4];
swap q[2], q[3];
swap q[3], q[4];
cx q[5], q[4];
swap q[3], q[4];
swap q[2], q[3];
cx q[1], q[2];
