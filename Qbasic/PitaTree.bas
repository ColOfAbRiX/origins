10 REM *** Lopsided Pythagora's Tree ***
20 REM *** USING BINARY NUMBER SYSTEM ***
30 REM *** NAME:PYTHT2 ***
40 SCREEN 11: CLS : PI = 3.141593
50 WINDOW (-2.5, -2)-(5.5, 4)
60 DIM X(4096), Y(4096)
70 REM *** CHOICE OF ANGLE ***
71 INPUT "SCEGLI 3 PER UN ALBERO ASIMMETRICO, 4 PER UNO SIMMETRICO: ", T
72 INPUT "INSERISCI IL NUMERO DI ITERAZIONI (MASSIMO 9): ", IT
73 IF IT > 10 THEN GOTO 72
74 CLS
80 F = PI / T: C = COS(F): S = SIN(F)
90 A1 = -C * S: A2 = C * C: B1 = A1 + A2: B2 = -A1 + A2
100 C1 = B2: C2 = 1 - B1: D1 = 1 - A1: D2 = 1 - A2
110 X(2) = 0: Y(2) = 0: X(3) = 1: Y(3) = 0
120 LINE (0, 0)-(1, 0): LINE -(1, -1): LINE -(0, -1): LINE -(0, 0)
130 FOR M = 1 TO IT
140 FOR J = 0 TO 2 ^ (M - 1) - 1
150 X0 = X(2 ^ M + 2 * J): Y0 = Y(2 ^ M + 2 * J)
160 X1 = X(2 ^ M + 2 * J + 1): Y1 = Y(2 ^ M + 2 * J + 1)
170 U = X1 - X0: V = Y1 - Y0
180 XA = X0 + A1 * U - A2 * V: YA = Y0 + A2 * U + A1 * V
190 XB = X0 + B1 * U - B2 * V: YB = Y0 + B2 * U + B1 * V
200 XC = X0 + C1 * U - C2 * V: YC = Y0 + C2 * U + C1 * V
210 XD = X0 + D1 * U - D2 * V: YD = Y0 + D2 * U + D1 * V
220 X(2 ^ (M + 1) + 4 * J) = XA: Y(2 ^ (M + 1) + 4 * J) = YA
230 X(2 ^ (M + 1) + 4 * J + 1) = XB: Y(2 ^ (M + 1) + 4 * J + 1) = YB
240 X(2 ^ (M + 1) + 4 * J + 2) = XC: Y(2 ^ (M + 1) + 4 * J + 2) = YC
250 X(2 ^ (M + 1) + 4 * J + 3) = XD: Y(2 ^ (M + 1) + 4 * J + 3) = YD
260 LINE (X0, Y0)-(XA, YA): LINE -(XB, YB)
270 LINE -(X1, Y1): LINE -(XD, YD)
280 LINE -(XC, YC): LINE -(X0, Y0)
290 NEXT J: NEXT M: BEEP
300 A$ = INPUT$(1): END

