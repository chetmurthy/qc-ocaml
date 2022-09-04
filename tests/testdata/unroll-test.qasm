OPENQASM 2.0;
include "qelib1.inc";
gate g1 a, b { CX a, b; }
gate g2 a, b { g1 a, b; }
qreg q[2];
g2 q[0], q[1];
