OPENQASM 2.0;
include "qelib1.inc";
qreg q[6];

cx q[2], q[3];
cx q[1], q[0];
swap q[2], q[3];
swap q[0], q[4];
cx q[1], q[0];
cx q[5], q[2];
h q[2];
h q[3];
cx q[2], q[3];
h q[2];
h q[3];
