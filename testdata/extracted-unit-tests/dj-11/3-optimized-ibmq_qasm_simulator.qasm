OPENQASM 2.0;
include "qelib1.inc";
qreg q0[14];
creg c0[13];
u3(pi,0,pi) q0[13];
u2(0,pi) q0[13];
u2(0,pi) q0[12];
u2(0,pi) q0[11];
u2(0,pi) q0[10];
u2(0,pi) q0[9];
u2(0,pi) q0[8];
u2(0,pi) q0[7];
u2(0,pi) q0[6];
u2(0,pi) q0[5];
u2(0,pi) q0[4];
u2(0,pi) q0[3];
u2(0,pi) q0[2];
u2(0,pi) q0[1];
u2(0,pi) q0[0];
barrier q0[0],q0[1],q0[2],q0[3],q0[4],q0[5],q0[6],q0[7],q0[8],q0[9],q0[10],q0[11],q0[12],q0[13];
cx q0[3],q0[13];
cx q0[12],q0[13];
barrier q0[0],q0[1],q0[2],q0[3],q0[4],q0[5],q0[6],q0[7],q0[8],q0[9],q0[10],q0[11],q0[12],q0[13];
u2(0,pi) q0[0];
u2(0,pi) q0[1];
u2(0,pi) q0[2];
u2(0,pi) q0[3];
u2(0,pi) q0[4];
u2(0,pi) q0[5];
u2(0,pi) q0[6];
u2(0,pi) q0[7];
u2(0,pi) q0[8];
u2(0,pi) q0[9];
u2(0,pi) q0[10];
u2(0,pi) q0[11];
u2(0,pi) q0[12];
barrier q0[0],q0[1],q0[2],q0[3],q0[4],q0[5],q0[6],q0[7],q0[8],q0[9],q0[10],q0[11],q0[12],q0[13];
measure q0[0] -> c0[0];
measure q0[1] -> c0[1];
measure q0[2] -> c0[2];
measure q0[3] -> c0[3];
measure q0[4] -> c0[4];
measure q0[5] -> c0[5];
measure q0[6] -> c0[6];
measure q0[7] -> c0[7];
measure q0[8] -> c0[8];
measure q0[9] -> c0[9];
measure q0[10] -> c0[10];
measure q0[11] -> c0[11];
measure q0[12] -> c0[12];
