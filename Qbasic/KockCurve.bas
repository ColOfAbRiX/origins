10 REM ***Koch Curve***
20 REM ***NAME:KOCH***
30 SCREEN 11: CLS : PI = 3.141593
40 WINDOW (-.1, -.4)-(1.1, .5)
45 INPUT "Numero di iterazioni(consigliato 4)? ", a
50 P = a: DIM T(P): REM ***ORDER***
60 H = 3 ^ (-P): PSET (0, 0)
70 FOR N = 0 TO 4 ^ (P - 1)
80 REM ***QUATERNARY NOTATION OF N***
90 M = N: FOR L = 0 TO P - 1
100 T(L) = M MOD 4: M = M \ 4: NEXT L
110 REM ***DETERMINATION SLOPE OF NTH LINE SEGMENT***
120 S = 0: FOR K = 0 TO P - 1
130 S = S + (T(K) + 1) MOD 3 - 1
140 NEXT K
150 REM ***GRAPH OF NTH LINE SEGMENT***
160 X = X + COS(PI * S / 3) * H
170 Y = Y + SIN(PI * S / 3) * H
180 LINE -(X, Y)
190 NEXT N: BEEP: a$ = INPUT$(1): END