DECLARE SUB Finestra2 (Larg!, Alt!, c!, Text$)
DECLARE SUB Pagina1 ()
DECLARE SUB Center (Alt!, Text$)
DECLARE SUB Finestra (Larg!, Alt!, c!, Text$)
SCREEN 0
COLOR 7, 1
CLS
ON ERROR GOTO Errori

'$STATIC
DIM SHARED Scelta(10)
DIM SHARED a(10)
DIM SHARED b(10)
DIM SHARED SubW$(1)
DIM SHARED Info$(16)

'$DYNAMIC

COLOR 0, 7
Finestra 40, 10, 7, "S e t u p  LSC  2.1"
COLOR 0, 7
Center 8, "Benvenuti nel setup di L.s.c. V2.1"
Center 10, "Configurerete il vostro programma"
Center 12, "per il vostro computer"
Center 14, "Premete un tasto per continuare"
DO: LOOP WHILE INKEY$ = ""
Scelta(1) = -1
Scelta(2) = -1
Config:
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    Finestra2 18, 4, 7, ""
    Center 8, "Scegli che tipo di configurazione vuoi"
    Center 10, " 1-Automatica [ ]"
    Center 11, " 2-Manuale    [ ]"
    IF Scelta(1) = 0 THEN
        LOCATE 10, 47
        PRINT "*"
    END IF
    IF Scelta(2) = 0 THEN
        LOCATE 11, 47
        PRINT "*"
    END IF
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(49)
            Scelta(1) = NOT (SGN(Scelta(1)))
            Scelta(2) = -1
        CASE CHR$(50)
            Scelta(2) = NOT (SGN(Scelta(2)))
            Scelta(1) = -1
        CASE CHR$(13)
            IF Scelta(1) = 0 OR Scelta(2) = 0 THEN Pagina1
        CASE ELSE
            BEEP
            GOTO Config
    END SELECT
    GOTO Config

Errori:
    SELECT CASE ERR
        CASE 64
            a(1) = a(1) - 1
        CASE ELSE
            COLOR 12, 0
            CLS
            Center 1, STRING$(80, "Ü")
            Center 2, "Û                                                                              Û"
            Center 3, "Û               The program is failed. Owerflow of the memory.                 Û"
            Center 4, "Û               Error 924. Block number 24CCH345F3 is failed.                  Û"
            Center 5, "Û                  Wait 60 seconds to restart the system.                      Û"
            Center 6, "Û                    Push any key to next errors page.                         Û"
            Center 7, "Û                                                                              Û"
            Center 8, STRING$(80, "ß")
            WHILE INKEY$ = ""
                a = 0: b = 1
                FOR x = 1 TO 5
                    a = a + 1
                    Center 12 + a, "   "
                NEXT
                Center 19, "   "
                SLEEP 1
                a = 0: b = 1
                FOR x = 1 TO 5
                    a = a + 1
                    Center 12 + a, "ÛÛÛ"
                NEXT
                Center 19, "ÛÛÛ"
                SLEEP 1
            WEND
            a = 0
            FOR x = 1 TO 23 * (80 / (LEN("Errore") + 2))
                a = a + 1
                LOCATE a, b
                PRINT "Errors  "
                IF a > 22 THEN b = b + LEN("Errore") + 2: a = 0
            NEXT
            END
    END SELECT
    RESUME

REM $STATIC
SUB Center (Alt, Text$)
    a = 40 - LEN(Text$) / 2 + 1
    LOCATE Alt, a
    PRINT Text$
END SUB

SUB Finestra (Larg, Alt, c, Text$)
    a = 40 - (Larg / 2)
    b = 11 - (Alt / 2)
    COLOR c, c
    FOR x = 1 TO Alt + 3
        d = d + 1
        LOCATE b + d - 2, a - 2
        PRINT STRING$(Larg + 6, " ")
    NEXT
    d = 0
    COLOR 15, c
    LOCATE b, a
    PRINT "Ú" + STRING$(Larg, "Ä") + "¿"
    FOR x = 1 TO Alt
        d = d + 1
        LOCATE b + d, a
        PRINT "³" + STRING$(Larg, " ")
    NEXT
    COLOR 0, c
    d = 0
    FOR x = 1 TO Alt
        d = d + 1
        LOCATE b + d, a + 1
        PRINT STRING$(Larg, " ") + "³"
    NEXT
    LOCATE b + d, a
    PRINT "À" + STRING$(Larg, "Ä") + "Ù"
    COLOR 0, c
    LOCATE b - 1, 40 - LEN(Text$) / 2
    PRINT Text$
    COLOR 0, 1
    d = 0
    FOR x = 1 TO Alt + 2
        d = d + 1
        LOCATE b + d - 1, a + Larg + 4
        PRINT "Û"
    NEXT
    LOCATE b + d, a + 1
    PRINT STRING$(Larg + 4, "ß")
END SUB

SUB Finestra2 (Larg, Alt, c, Text$)
    a = 40 - (Larg / 2)
    b = 11 - (Alt / 2)
    COLOR c, c
    FOR x = 1 TO Alt + 3
        d = d + 1
        LOCATE b + d - 2, a - 2
        PRINT STRING$(Larg + 6, " ")
    NEXT
    d = 0
    COLOR 0, c
    LOCATE b, a
    PRINT "Ú" + STRING$(Larg, "Ä") + "¿"
    FOR x = 1 TO Alt
        d = d + 1
        LOCATE b + d, a
        PRINT "³" + STRING$(Larg, " ")
    NEXT
    COLOR 15, c
    d = 0
    FOR x = 1 TO Alt
        d = d + 1
        LOCATE b + d, a + 1
        PRINT STRING$(Larg, " ") + "³"
    NEXT
    LOCATE b + d, a
    PRINT "À" + STRING$(Larg, "Ä") + "Ù"
    COLOR 0, c
    LOCATE b - 1, 40 - LEN(Text$) / 2
    PRINT Text$
END SUB

SUB Pagina1
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    IF Scelta(1) = 0 THEN
        GOSUB Auto
    ELSE
        GOSUB Man
    END IF

Auto:
    Center 8, "Configurazione della velocit…"
    Finestra2 22, 2, 7, ""
    Center 9, "Test..."
    Center 15, "Test 1"
    aa = TIMER
    FOR x = 1 TO UBOUND(a) - 5
        a = a + 1
        b = TIMER
        FOR y = 1 TO 100000: NEXT
        c = TIMER
        b(a) = c - b: a(a) = 1 / (c - b)
    NEXT: ab = TIMER
    a = 0
    FOR x = 1 TO UBOUND(a) - 5
        a = a + 1: t = t + a(a): t1 = t1 + b(a)
    NEXT
    TM = t1 / (UBOUND(b) - 5): FC = t / (UBOUND(a) - 5)
    SELECT CASE TM
        CASE IS <= .1: Vel = 75
        CASE IS > .1 AND TM <= 0: Vel = 110
        CASE IS > 0 AND TM <= 2: Vel = 150
        CASE IS > 2 AND TM <= 3.5: Vel = 300
        CASE IS > 3.5 AND TM <= 5: Vel = 600
        CASE IS > 5 AND TM <= 6: Vel = 1200
        CASE IS > 6 AND TM <= 7.5: Vel = 2400
        CASE IS > 7.5 AND TM <= 9: Vel = 4800
        CASE ELSE: Vel = 9600
    END SELECT
    Info$(5) = STR$(Vel)
    Center 9, "         "
    F1$ = "Velocit… scelta:" + STR$(Vel): Center 11, F1$
    SLEEP 3
    Center 8, STRING$(30, " ")
    Center 8, "Configurazione della Porta"
    Finestra2 24, 2, 7, ""
    Center 9, "Test..."
    Center 15, "Test 2"
    a(1) = 11
    a(1) = a(1) - 1
    OPEN "COM" + LTRIM$(STR$(a(1))) + ":300,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #1
    CLOSE
    SLEEP 1
    Center 9, "         "
    F1$ = "Porta seriale scelta: " + STR$(a(1)): Center 11, F1$
    Info$(9) = LTRIM$(STR$(a(1)))
    SLEEP 3
    Center 8, "Configurazione Tempo di chiamata"
    Finestra2 24, 2, 7, ""
    Center 9, "Test..."
    Center 15, "Test 3"
    Center 9, "         "
    SLEEP 1
    Center 11, "Tempo chiamata: 10sec"
    Info$(4) = "10"
    SLEEP 3
    GOSUB Compensazione
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    Center 7, "  Configurazioni secondarie  "
    Center 9, "Invio file         "
    Info$(11) = "S": Info$(12) = "S": Info$(13) = "S": Info$(14) = "N": Info$(15) = "S":
    Center 10, "Directory di arrivo"
    Info$(10) = "C:\"
    Center 11, "Opzioni colore     "
    Info$(2) = "COLORE"
    Center 12, "Guida              "
    COLOR 14, 7: SLEEP 1: LOCATE 9, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 9, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 10, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 10, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 11, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 11, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 12, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 12, 28: PRINT "OK"
    SLEEP 3
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    Center 7, "                          "
    Center 7, "  Configurazione Uente  "
    Center 8, "Scrivi il tuo nome e cognome"
    Finestra2 28, 2, 7, ""
    LOCATE 11, 27
    INPUT Info$(1)
    LOCATE 11, 27: PRINT Info$(1) + "   "
    GOSUB Salva
    SLEEP 5
    RUN "c:comuni21"

Man:
    Finestra2 10, 2, 7, ""
    Center 8, "Scrivi la velocit… della porta"
    Center 13, "Scelte:75,110,150,300,600,1200,"
    Center 14, "2400,4800,9600"
    Center 11, "____"
Immissione:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(13)
            IF a$ <> "75" AND a$ <> "110" AND a$ <> "150" AND a$ <> "300" AND a$ <> "600" AND a$ <> "1200" AND a$ <> "2400" AND a$ <> "4800" AND a$ <> "9600" THEN
                BEEP
                GOTO Immissione
            ELSE
                Info$(5) = a$
                a$ = ""
                Finestra 40, 10, 7, "S e t u p  LSC  2.1"
                Finestra2 6, 2, 7, ""
                Center 8, "Scrivi il numero della seriale libera"
                Center 11, "__"
                GOTO Immissione2
            END IF
        CASE CHR$(8)
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE ELSE
            a$ = a$ + Tasti$
    END SELECT
    b$ = a$ + STRING$(4 - LEN(a$), "_")
    Center 11, b$
    GOTO Immissione

Immissione2:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(13)
            Info$(9) = LTRIM$(RTRIM$(a$))
            a$ = ""
            Finestra 40, 10, 7, "S e t u p  LSC  2.1"
            Finestra2 6, 2, 7, ""
            Center 8, "Scrivi il tempo di chiamata"
            Center 11, "____"
            GOTO Immissione3
        CASE CHR$(8)
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE ELSE
            a$ = a$ + Tasti$
    END SELECT
    b$ = a$ + STRING$(2 - LEN(a$), "_")
    Center 11, b$
    GOTO Immissione2

Immissione3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(13)
            Info$(4) = LTRIM$(RTRIM$(a$))
            a$ = ""
            Finestra 40, 10, 7, "S e t u p  LSC  2.1"
            Finestra2 26, 2, 7, ""
            Center 8, "Scrivi la dir. di arrivo dei file"
            Center 11, STRING$(24, "_")
            GOTO Immissione4
        CASE CHR$(8)
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE ELSE
            a$ = a$ + Tasti$
    END SELECT
    b$ = a$ + STRING$(4 - LEN(a$), "_")
    Center 11, b$
    GOTO Immissione3

Immissione4:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(13)
            IF RIGHT$(a$, 1) <> "\" THEN a$ = a$ + "\"
            a$ = UCASE$(a$)
            Info$(10) = LTRIM$(RTRIM$(a$))
            aa = 1
            GOSUB Compensazione
        CASE CHR$(8)
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE ELSE
            a$ = a$ + Tasti$
    END SELECT
    b$ = a$ + STRING$(24 - LEN(a$), "_")
    Center 11, b$
    GOTO Immissione4


Compensazione:
    Center 8, STRING$(34, " ")
    Center 8, "Configurazione velocit… PC"
    Finestra2 24, 2, 7, ""
    Center 9, "Test..."
    Center 15, "Test 4"
    a = 0
    aa = TIMER
    FOR x = 1 TO UBOUND(a)
        a = a + 1
        b = TIMER
        FOR y = 1 TO 100000: NEXT
        c = TIMER
        a(a) = 1 / (c - b)
    NEXT: ab = TIMER
    a = 0
    FOR x = 1 TO UBOUND(a)
        a = a + 1
        t = t + a(a)
    NEXT
    FC = t / UBOUND(a)
    Center 9, "         "
    F1$ = "Compensazione:" + STR$(FC): Center 11, F1$
    Info$(3) = STR$(FC)
    SLEEP 3
    IF aa = 0 THEN RETURN
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    Center 7, "  Configurazioni secondarie  "
    Center 9, "Invio file         "
    Info$(11) = "S": Info$(12) = "S": Info$(13) = "S": Info$(14) = "N": Info$(15) = "S":
    Center 10, "Directory di arrivo"
    Info$(10) = "C:\"
    Center 11, "Opzioni colore     "
    Info$(2) = "COLORE"
    Center 12, "Guida              "
    COLOR 14, 7: SLEEP 1: LOCATE 9, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 9, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 10, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 10, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 11, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 11, 28: PRINT "OK"
    COLOR 14, 7: SLEEP 1: LOCATE 12, 29: PRINT "¯"
    COLOR 10, 7: SLEEP 1: LOCATE 12, 28: PRINT "OK"
    SLEEP 3
    Finestra 40, 10, 7, "S e t u p  LSC  2.1"
    COLOR 0, 7
    Center 7, "                          "
    Center 7, "  Configurazione Uente  "
    Center 8, "Scrivi il tuo nome e cognome"
    Finestra2 28, 2, 7, ""
    LOCATE 11, 27
    INPUT Info$(1)
    LOCATE 11, 27: PRINT Info$(1) + "   "
    GOSUB Salva
    SLEEP 5
    RUN "comuni21.bas"
    

Salva:
    Info$(6) = "N"
    Info$(7) = "8"
    Info$(8) = "1"
    Info$(16) = "N"
    OPEN "info2.lsc" FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
            PRINT #1, Info$(a)
    NEXT
    CLOSE
    RETURN
END SUB

