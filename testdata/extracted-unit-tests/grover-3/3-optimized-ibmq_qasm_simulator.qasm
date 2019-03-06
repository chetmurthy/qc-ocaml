OPENQASM 2.0;
include "qelib1.inc";
qreg q2[3];
qreg q3[1];
qreg q4[4];
creg c0[3];
u2(0,pi) q4[3];
u3(pi,0,pi) q3[0];
u2(0,pi) q3[0];
u2(0,pi) q3[0];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u2(0,pi) q2[1];
u2(0,pi) q2[0];
cx q2[0],q4[0];
cx q2[1],q4[0];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
cx q2[2],q4[0];
u2(0,pi) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
u2(0,pi) q4[0];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[1];
u3(pi,0,pi) q2[1];
cx q2[1],q4[1];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
u3(pi,0,pi) q2[2];
cx q2[2],q4[1];
u2(0,pi) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
u2(0,pi) q4[1];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u3(pi,0,pi) q2[0];
cx q2[0],q4[2];
u3(pi,0,pi) q2[1];
cx q2[1],q4[2];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[2];
u2(0,pi) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
u2(0,pi) q4[2];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
cx q4[3],q3[0];
u1(-pi/4) q3[0];
cx q4[2],q3[0];
u1(pi/4) q3[0];
cx q4[3],q3[0];
u1(-pi/4) q3[0];
cx q4[2],q3[0];
u1(pi/4) q3[0];
u2(0,pi) q3[0];
u2(0,pi) q3[0];
u1(pi/4) q4[3];
cx q4[2],q4[3];
u1(pi/4) q4[2];
u1(-pi/4) q4[3];
cx q4[2],q4[3];
u2(0,pi) q4[3];
u1(pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q4[0];
u1(-pi/4) q4[1];
cx q4[0],q4[1];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q4[0];
u1(-pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[0];
cx q2[1],q4[0];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[0];
u2(0,pi) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
u2(0,pi) q4[0];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[1];
u3(pi,0,pi) q2[1];
cx q2[1],q4[1];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
u3(pi,0,pi) q2[2];
cx q2[2],q4[1];
u2(0,pi) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
u2(0,pi) q4[1];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u3(pi,0,pi) q2[0];
cx q2[0],q4[2];
u3(pi,0,pi) q2[1];
cx q2[1],q4[2];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[2];
u2(0,pi) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
u2(0,pi) q4[2];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[0];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[1];
u3(pi,0,pi) q2[1];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u2(0,pi) q2[2];
u2(0,pi) q2[2];
cx q2[1],q2[2];
u1(-pi/4) q2[2];
cx q2[0],q2[2];
u1(pi/4) q2[2];
cx q2[1],q2[2];
u1(-pi/4) q2[2];
cx q2[0],q2[2];
u1(pi/4) q2[2];
u2(0,pi) q2[2];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[0];
cx q2[0],q4[0];
u3(pi,0,pi) q2[1];
u2(0,pi) q2[1];
cx q2[1],q4[0];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
cx q2[2],q4[0];
u2(0,pi) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
u2(0,pi) q4[0];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[1];
u3(pi,0,pi) q2[1];
cx q2[1],q4[1];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
u3(pi,0,pi) q2[2];
cx q2[2],q4[1];
u2(0,pi) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
u2(0,pi) q4[1];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u3(pi,0,pi) q2[0];
cx q2[0],q4[2];
u3(pi,0,pi) q2[1];
cx q2[1],q4[2];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[2];
u2(0,pi) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
u2(0,pi) q4[2];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
cx q4[3],q3[0];
u1(-pi/4) q3[0];
cx q4[2],q3[0];
u1(pi/4) q3[0];
cx q4[3],q3[0];
u1(-pi/4) q3[0];
cx q4[2],q3[0];
u1(pi/4) q3[0];
u2(0,pi) q3[0];
u1(pi/4) q4[3];
cx q4[2],q4[3];
u1(pi/4) q4[2];
u1(-pi/4) q4[3];
cx q4[2],q4[3];
u2(0,pi) q4[3];
u1(pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q4[0];
u1(-pi/4) q4[1];
cx q4[0],q4[1];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
cx q4[1],q4[3];
u1(-pi/4) q4[3];
cx q4[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q4[0];
u1(-pi/4) q4[1];
cx q4[0],q4[1];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[0];
cx q2[1],q4[0];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[0];
u2(0,pi) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
cx q4[3],q4[0];
u1(-pi/4) q4[0];
cx q2[2],q4[0];
u1(pi/4) q4[0];
u2(0,pi) q4[0];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
cx q2[0],q4[1];
u3(pi,0,pi) q2[1];
cx q2[1],q4[1];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
u3(pi,0,pi) q2[2];
cx q2[2],q4[1];
u2(0,pi) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
cx q4[3],q4[1];
u1(-pi/4) q4[1];
cx q2[2],q4[1];
u1(pi/4) q4[1];
u2(0,pi) q4[1];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u3(pi,0,pi) q2[0];
cx q2[0],q4[2];
u3(pi,0,pi) q2[1];
cx q2[1],q4[2];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[2];
cx q2[2],q4[2];
u2(0,pi) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
cx q4[3],q4[2];
u1(-pi/4) q4[2];
cx q2[2],q4[2];
u1(pi/4) q4[2];
u2(0,pi) q4[2];
u1(pi/4) q4[3];
cx q2[2],q4[3];
u1(pi/4) q2[2];
u1(-pi/4) q4[3];
cx q2[2],q4[3];
u2(0,pi) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
cx q2[1],q4[3];
u1(-pi/4) q4[3];
cx q2[0],q4[3];
u1(pi/4) q4[3];
u2(0,pi) q4[3];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[0];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[1];
u3(pi,0,pi) q2[1];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u2(0,pi) q2[2];
u2(0,pi) q2[2];
cx q2[1],q2[2];
u1(-pi/4) q2[2];
cx q2[0],q2[2];
u1(pi/4) q2[2];
cx q2[1],q2[2];
u1(-pi/4) q2[2];
cx q2[0],q2[2];
u1(pi/4) q2[2];
u2(0,pi) q2[2];
u2(0,pi) q2[2];
u3(pi,0,pi) q2[2];
u2(0,pi) q2[2];
measure q2[2] -> c0[2];
u1(pi/4) q2[1];
cx q2[0],q2[1];
u1(pi/4) q2[0];
u1(-pi/4) q2[1];
cx q2[0],q2[1];
u3(pi,0,pi) q2[0];
u2(0,pi) q2[0];
measure q2[0] -> c0[0];
u3(pi,0,pi) q2[1];
u2(0,pi) q2[1];
measure q2[1] -> c0[1];
