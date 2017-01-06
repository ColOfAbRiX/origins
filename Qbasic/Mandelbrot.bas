REM ***Mandelbrot Set***
REM ***NAME:MANDEL***
SCREEN 11: CLS
WINDOW (-2.2, -1.4)-(1.1, 1.4)
N1 = 640: N2 = 400: REM ***RESOLUTION***
FOR I = -N1 TO N1: A = -.55 + 1.65 * I / N1
FOR J = 0 TO N2: B = 1.4 * J / N2
U = 4 * (A * A + B * B): V = U - 2 * A + 1 / 4
IF U + 8 * A + 15 / 4 < 0 THEN K = 1: GOTO 170
IF V - SQR(V) + 2 * A - 1 / 2 < 0 THEN K = 1: GOTO 170
X = A: Y = B
FOR K = 1 TO 50
U = X * X: V = Y * Y: W = 2 * X * Y
X = U - V + A: Y = W + B
IF U + V > 16 THEN GOTO 170
NEXT K
170 L = K MOD 2: PSET (A, B), L: PSET (A, -B), L
NEXT J: NEXT I
BEEP: A$ = INPUT$(1): END

