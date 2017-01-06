REM ***ARCHIMEDES SPIRAL***
REM ***NAME:ARCHI***
SCREEN 11: CLS : PI = 3.141592654#
WINDOW (-12, -9)-(12, 9)
A = .1: PSET (0, 0)
FOR T = 0 TO 20 * PI STEP .1: R = A * T
X = R * COS(T): Y = R * SIN(T)
LINE -(X, Y)
NEXT T
A$ = INPUT$(1): END

