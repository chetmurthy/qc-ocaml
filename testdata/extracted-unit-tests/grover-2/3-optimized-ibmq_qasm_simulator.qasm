OPENQASM 2.0;
include "qelib1.inc";
qreg q1[3];
u2(0,pi) q1[2];
u3(pi,0,pi) q1[2];
u2(0,pi) q1[2];
u2(0,pi) q1[2];
u2(0,pi) q1[1];
u3(pi,0,pi) q1[1];
cx q1[1],q1[2];
u1(-pi/4) q1[2];
u2(0,pi) q1[0];
u3(pi,0,pi) q1[0];
cx q1[0],q1[2];
u1(pi/4) q1[2];
cx q1[1],q1[2];
u1(-pi/4) q1[2];
cx q1[0],q1[2];
u1(pi/4) q1[2];
u2(0,pi) q1[2];
u2(0,pi) q1[2];
u3(pi,0,pi) q1[2];
u2(0,pi) q1[2];
u1(pi/4) q1[1];
cx q1[0],q1[1];
u1(pi/4) q1[0];
u1(-pi/4) q1[1];
cx q1[0],q1[1];
u3(pi,0,pi) q1[0];
u2(0,pi) q1[0];
u3(pi,0,pi) q1[1];
u2(0,pi) q1[1];
