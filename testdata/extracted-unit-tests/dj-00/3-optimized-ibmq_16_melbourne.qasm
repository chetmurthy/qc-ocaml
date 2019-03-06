OPENQASM 2.0;
include "qelib1.inc";
qreg q[14];
creg c0[13];
u2(0,pi) q[13];
u2(0,pi) q[12];
u2(0,pi) q[11];
u2(0,pi) q[10];
u2(0,pi) q[9];
u2(0,pi) q[8];
u3(1.57079632679490,3.14159265358979,3.14159265358979) q[7];
u2(0,pi) q[6];
u2(0,pi) q[5];
u2(0,pi) q[4];
u2(0,pi) q[3];
u2(0,pi) q[2];
u2(0,pi) q[1];
u2(0,pi) q[0];
barrier q[0],q[1],q[2],q[13],q[3],q[12],q[4],q[11],q[10],q[5],q[9],q[6],q[8],q[7];
id q[7];
barrier q[0],q[1],q[2],q[13],q[3],q[12],q[4],q[11],q[10],q[5],q[9],q[6],q[8],q[7];
u2(0,pi) q[13];
u2(0,pi) q[12];
u2(0,pi) q[11];
u2(0,pi) q[10];
u2(0,pi) q[9];
u2(0,pi) q[8];
u2(0,pi) q[6];
u2(0,pi) q[5];
u2(0,pi) q[4];
u2(0,pi) q[3];
u2(0,pi) q[2];
u2(0,pi) q[1];
u2(0,pi) q[0];
barrier q[13],q[2],q[1],q[0],q[8],q[6],q[9],q[5],q[10],q[11],q[4],q[12],q[3];
measure q[13] -> c0[3];
measure q[12] -> c0[5];
measure q[11] -> c0[7];
measure q[10] -> c0[8];
measure q[9] -> c0[10];
measure q[8] -> c0[12];
measure q[6] -> c0[11];
measure q[5] -> c0[9];
measure q[4] -> c0[6];
measure q[3] -> c0[4];
measure q[2] -> c0[2];
measure q[1] -> c0[1];
measure q[0] -> c0[0];
