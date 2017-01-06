DECLARE SUB Vittoria (Giocatore$, Vin!)
DECLARE SUB Tabella (a$, b$)
DECLARE SUB Invio (xy$)
DECLARE SUB Gioco ()
DECLARE SUB Disposizione ()
DECLARE SUB ScambioI ()
DECLARE SUB Pagina2 ()
DECLARE SUB Pagina1 ()
DECLARE SUB Titolo ()
DECLARE SUB Colori ()
DECLARE SUB PInfor ()
SCREEN 0
CLS

'$STATIC
DIM SHARED Info$(16)
DIM SHARED Avv$(1)
DIM SHARED Porta$(2)
DIM SHARED Navi(5)
DIM SHARED Mossa$(2)
DIM SHARED Turno(1)
DIM SHARED a(1)
DIM SHARED Punti(2)
DIM SHARED Inizio(2)
'$DYNAMIC
DIM SHARED a$(5, 5)
DIM SHARED Col(8, 2)
DIM SHARED Intra$(50, 50)
 'Costanti
CONST Info$ = "info2.lsc"

     'Titolo  Men—  Pagina  Stato  Errore  Attivo  MSel  Punto Guida
Colorato:
DATA   4,7,   0,3,    7,1,  15,1,   14,4,  15,1,   14,3,  12,7, 10,7
BlackW:
DATA   0,7,   7,0,    7,0,   7,0,   15,0,  15,0,   15,0,   0,7, 15,7
PInfor
Porta$(2) = "COM" + Info$(9) + ":4800," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
OPEN Porta$(2) FOR RANDOM AS #2
COM(VAL(Info$(9))) ON
Colori
Titolo
ScambioI
Pagina1
Pagina2
Disposizione

REM $STATIC
SUB Colori
    IF Info$(2) = "COLORE" THEN
        RESTORE Colorato
    ELSE
        RESTORE BlackW
    END IF
    FOR x = 1 TO UBOUND(Col, 1)
        a = a + 1
        FOR y = 1 TO 2
            b = b + 1
            READ Col(a, b)
        NEXT
        b = 0
    NEXT
    COLOR Col(3, 2), Col(3, 2): CLS
END SUB

SUB Disposizione
    Navi(1) = 5
    Navi(2) = 4
    Navi(3) = 3
    Navi(4) = 3
    Navi(5) = 2
    b = 0
Immissione:
    LOCATE 19, 2: PRINT "Scrivi le coordinate di Nave"; b + 1; "(lung"; Navi(b + 1); ")"
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8)
            IF a = 0 THEN
                BEEP
                GOTO Immissione
            END IF
            a = a - 1
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE CHR$(13)
            b = b + 1
            a$(b, 1) = LEFT$(a$, 2)
            a$(b, 2) = RIGHT$(a$, 2)
            a1$ = LEFT$(a$(b, 1), 1)
            a2$ = RIGHT$(a$(b, 1), 1)
            b1$ = LEFT$(a$(b, 2), 1)
            b2$ = RIGHT$(a$(b, 2), 1)
            FOR x = 1 TO Navi(b) - 2
                ab = ab + 1
                IF a1$ = b1$ THEN
                    IF ABS(VAL(a2$) - VAL(b2$)) <> Navi(b) - 1 THEN
                        BEEP
                        LOCATE 18, 2: PRINT "Nave troppo lunga o troppo corta"
                        LOCATE 20, 2: PRINT "                    "
                        a$ = "": a = 0: ab = 0: b = b - 1
                        GOTO Immissione
                    END IF
                    IF a2$ < b2$ THEN
                        Intra$(b, ab) = a1$ + LTRIM$(STR$(VAL(RIGHT$(a$(b, 1), 1)) + ab))
                    ELSE
                        Intra$(b, ab) = a1$ + LTRIM$(STR$(VAL(RIGHT$(a$(b, Navi(b)), 1)) - ab))
                    END IF
                ELSEIF a2$ = b2$ THEN
                    IF ABS(ASC(a1$) - ASC(b1$)) <> Navi(b) - 1 THEN
                        BEEP
                        LOCATE 18, 2: PRINT "Nave troppo lunga o troppo corta"
                        LOCATE 20, 2: PRINT "                    "
                        a$ = "": a = 0: ab = 0: b = b - 1
                        GOTO Immissione
                    END IF
                    IF a1$ < b1$ THEN
                        Intra$(b, ab) = LTRIM$(CHR$(ASC(LEFT$(a$(b, 1), 1)) + ab)) + a2$
                    ELSE
                        Intra$(b, ab) = LTRIM$(CHR$(ASC(LEFT$(a$(b, Navi(b)), 1)) - ab)) + a2$
                    END IF
                ELSE
                    BEEP
                    LOCATE 18, 2: PRINT "Mossa non consentita"
                    LOCATE 20, 2: PRINT "                    "
                    a$ = "": a = 0: ab = 0: b = b - 1
                    GOTO Immissione
                END IF
            NEXT
            a$ = "": a = 0: ab = 0
            GOSUB Immagine
            COLOR Col(6, 1), Col(6, 2)
            LOCATE 20, 2: PRINT STRING$(20, " ")
            IF b > 4 THEN Gioco
        CASE ELSE
            IF (ASC(UCASE$(Tasti$)) >= 49 AND ASC(UCASE$(Tasti$)) <= 56) OR (ASC(UCASE$(Tasti$)) >= 65 AND ASC(UCASE$(Tasti$)) <= 72) THEN
                IF a > 4 THEN
                    BEEP
                    GOTO Immissione
                END IF
                a = a + 1
                a$ = a$ + UCASE$(Tasti$)
                IF a = 2 THEN a = a + 1: a$ = a$ + "-"
            ELSE
                BEEP
                GOTO Immissione
            END IF
    END SELECT
    LOCATE 18, 2: PRINT SPACE$(40)
    LOCATE 20, 2
    PRINT a$ + "      "
    GOTO Immissione

Immagine:
    a1$ = LEFT$(a$(b, 1), 1)
    a2$ = RIGHT$(a$(b, 1), 1)
    b1$ = LEFT$(a$(b, 2), 1)
    b2$ = RIGHT$(a$(b, 2), 1)
    SELECT CASE a1$
        CASE "A": L11 = 7
        CASE "B": L11 = 8
        CASE "C": L11 = 9
        CASE "D": L11 = 10
        CASE "E": L11 = 11
        CASE "F": L11 = 12
        CASE "G": L11 = 13
        CASE "H": L11 = 14
    END SELECT
    SELECT CASE a2$
        CASE "1": L12 = 12
        CASE "2": L12 = 15
        CASE "3": L12 = 18
        CASE "4": L12 = 21
        CASE "5": L12 = 24
        CASE "6": L12 = 27
        CASE "7": L12 = 30
        CASE "8": L12 = 33
    END SELECT
    SELECT CASE b1$
        CASE "A": L21 = 7
        CASE "B": L21 = 8
        CASE "C": L21 = 9
        CASE "D": L21 = 10
        CASE "E": L21 = 11
        CASE "F": L21 = 12
        CASE "G": L21 = 13
        CASE "H": L21 = 14
    END SELECT
    SELECT CASE b2$
        CASE "1": L22 = 12
        CASE "2": L22 = 15
        CASE "3": L22 = 18
        CASE "4": L22 = 21
        CASE "5": L22 = 24
        CASE "6": L22 = 27
        CASE "7": L22 = 30
        CASE "8": L22 = 33
    END SELECT
    COLOR 15, Navi(b)
    LOCATE L11, L12 - 1: PRINT b
    LOCATE L21, L22 - 1: PRINT b
    FOR x = 1 TO Navi(b) - 1
        IF L11 = L21 THEN
            IF L12 < L22 THEN
                aa = aa + 3
            ELSE
                aa = aa - 3
            END IF
            LOCATE L11, L12 + aa - 1
            PRINT b
        END IF
        IF L12 = L22 THEN
            IF L11 < L21 THEN
                aa = aa + 1
            ELSE
                aa = aa - 1
            END IF
            LOCATE L11 + aa, L12 - 1
            PRINT b
        END IF
    NEXT
    aa = 0
    RETURN
END SUB

SUB Gioco
TIMER ON
b$ = "": a = 0
OPEN "temp.lsc" FOR INPUT AS #1: INPUT #1, a$: CLOSE #1
IF a$ = "0" THEN
    LOCATE 19, 2
    PRINT "Attendi che l'altro giocatore faccia la sua mossa ..."
    PRINT #2, "Fatto"
    INPUT #2, Mossa$(1)
    INPUT #2, Mossa$(2)
    IF Mossa$(1) = "Disconnetti" THEN KILL "temp.lsc": RUN "COMUNI22"
    Turno(1) = -1
    Tabella Mossa$(1), Mossa$(2)
    LOCATE 19, 2
    PRINT Info$(1); " fai la tua mossa                       "
    Inizio(1) = TIMER
    LOCATE 20, 2
    PRINT SPACE$(10)
    GOSUB Immissione2
ELSE
    LOCATE 19, 2: PRINT "Attendi che l'avversario finisca di disporre le navi"
    INPUT #2, aa$
    LOCATE 19, 2: PRINT SPACE$(50)
    Turno(1) = NOT (SGN(Turno(1)))
    Inizio(1) = TIMER
    LOCATE 19, 2
    PRINT Info$(1); " fai la tua mossa                       "
    LOCATE 20, 2
    PRINT SPACE$(10)
Immissione2:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(27)
            PRINT #2, "Disconnetti"
            PRINT #2, "Disconnetti"
            KILL "temp.lsc"
            RUN "COMUNI22"
        CASE CHR$(8)
            IF a = 0 THEN
                BEEP
                GOTO Immissione2
            END IF
            a = a - 1
            b$ = LEFT$(b$, LEN(b$) - 1)
        CASE CHR$(13)
            Invio b$
            b$ = "": a = 0
            IF Turno(1) = 0 OR a(1) = 1 THEN Turno(1) = -1: Tabella Mossa$(1), Mossa$(2)
            LOCATE 19, 2
            PRINT Info$(1); " fai la tua mossa                       "
            LOCATE 20, 2
            PRINT SPACE$(10)
            Turno(1) = -1
        CASE ELSE
            IF (ASC(UCASE$(Tasti$)) >= 49 AND ASC(UCASE$(Tasti$)) <= 56) OR (ASC(UCASE$(Tasti$)) >= 65 AND ASC(UCASE$(Tasti$)) <= 72) THEN
                IF a > 1 THEN
                    BEEP
                    GOTO Immissione2
                END IF
                a = a + 1
                b$ = b$ + UCASE$(Tasti$)
            ELSE
                BEEP
                GOTO Immissione2
            END IF
    END SELECT
    LOCATE 20, 2
    PRINT b$ + " "
    GOTO Immissione2

END IF
Gioco
END SUB

SUB Invio (xy$)
    Turno(1) = NOT (SGN(Turno(1)))
    a1$ = LEFT$(xy$, 1)
    a2$ = RIGHT$(xy$, 1)
    PRINT #2, a1$
    PRINT #2, a2$
    Tabella a1$, a2$
    LOCATE 19, 2
    PRINT "Attendi che l'altro giocatore faccia la sua mossa ..."
    INPUT #2, Mossa$(1)
    INPUT #2, Mossa$(2)
    IF Mossa$(1) = "Disconnetti" THEN KILL "temp.lsc": RUN "COMUNI22"
    Turno(1) = NOT (SGN(Turno(1)))
    a(1) = 1
    EXIT SUB
    
END SUB

SUB Pagina1
COLOR Col(6, 1), Col(6, 2)
LOCATE 5, 23 - (LEN(Info$(1)) / 2)
PRINT Info$(1)
COLOR Col(6, 1), 0
FOR x = 1 TO 4
    a = a + 2
    LOCATE 5 + a, 11
    PRINT "ЫЫЫ   ЫЫЫ   ЫЫЫ   ЫЫЫ   "
    LOCATE 5 + a + 1, 11
    PRINT "   ЫЫЫ   ЫЫЫ   ЫЫЫ   ЫЫЫ"
NEXT
a = 0
COLOR Col(6, 1), Col(6, 2)
FOR x = 1 TO 8
    a = a + 3
    b = b + 1
    LOCATE 15, 8 + a
    PRINT b
NEXT
a = 0
FOR x = 1 TO 8
    a = a + 1
    LOCATE 6 + a, 9
    PRINT CHR$(64 + a)
NEXT
END SUB

SUB Pagina2
COLOR Col(6, 1), Col(6, 2)
LOCATE 5, 60 - (LEN(Avv$(1)) / 2)
PRINT Avv$(1)
COLOR Col(6, 1), Col(5, 2)
FOR x = 1 TO 4
    a = a + 2
    LOCATE 5 + a, 48
    PRINT "ЫЫЫ   ЫЫЫ   ЫЫЫ   ЫЫЫ   "
    LOCATE 5 + a + 1, 48
    PRINT "   ЫЫЫ   ЫЫЫ   ЫЫЫ   ЫЫЫ"
NEXT
a = 0
COLOR Col(5, 2), Col(6, 2)
FOR x = 1 TO 8
    a = a + 3
    b = b + 1
    LOCATE 15, 45 + a
    PRINT b
NEXT
a = 0
FOR x = 1 TO 8
    a = a + 1
    LOCATE 6 + a, 46
    PRINT CHR$(64 + a)
NEXT
END SUB

SUB PInfor
    OPEN Info$ FOR INPUT AS #1
    WHILE EOF(1) = 0
        a = a + 1
        LINE INPUT #1, Info$(a)
    WEND
    CLOSE
    IF Info$(1) = "" THEN RUN "c:setup.bas"
END SUB

SUB ScambioI
    OPEN "temp.lsc" FOR INPUT AS #1: INPUT #1, a$: CLOSE #1
    IF a$ <> "1" THEN
        INPUT #2, Avv$(1)
        SLEEP 1
        PRINT #2, Info$(1)
    ELSE
        SLEEP 1
        PRINT #2, Info$(1)
        INPUT #2, Avv$(1)
    END IF
END SUB

SUB Tabella (x$, y$)
OPEN "temp.lsc" FOR INPUT AS #1: INPUT #1, a$: CLOSE #1
IF Turno(1) = -1 THEN
    FOR x = 1 TO 5
        a = a + 1
        a1$ = LEFT$(a$(a, 1), 1)
        a2$ = RIGHT$(a$(a, 1), 1)
        b1$ = LEFT$(a$(a, 2), 1)
        b2$ = RIGHT$(a$(a, 2), 1)
        IF a$(a, 1) = x$ + y$ THEN
            Ast$ = " C "
            Punti(2) = Punti(2) + 1
            PRINT #2, "Col"
            EXIT FOR
        ELSE
            Ast$ = " X "
        END IF
        IF a$(a, 2) = x$ + y$ THEN
            Ast$ = " C "
            Punti(2) = Punti(2) + 1
            PRINT #2, "Col"
            EXIT FOR
        ELSE
            Ast$ = " X "
        END IF
        FOR y = 1 TO Navi(a)
            ab = ab + 1
            IF Intra$(a, ab) = x$ + y$ THEN
                Ast$ = " C "
                Punti(2) = Punti(2) + 1
                PRINT #2, "Col"
                EXIT FOR
            ELSE
                Ast$ = " X "
            END IF
            LOCATE 1
        NEXT
        ab = 0
        IF Ast$ = " C " THEN EXIT FOR
        IF a = 5 THEN PRINT #2, "H2O"
    NEXT
    SELECT CASE x$
        CASE "A": L11 = 7
        CASE "B": L11 = 8
        CASE "C": L11 = 9
        CASE "D": L11 = 10
        CASE "E": L11 = 11
        CASE "F": L11 = 12
        CASE "G": L11 = 13
        CASE "H": L11 = 14
    END SELECT
    SELECT CASE y$
        CASE "1": L12 = 12
        CASE "2": L12 = 15
        CASE "3": L12 = 18
        CASE "4": L12 = 21
        CASE "5": L12 = 24
        CASE "6": L12 = 27
        CASE "7": L12 = 30
        CASE "8": L12 = 33
    END SELECT
    LOCATE L11, L12 - 1
    PRINT Ast$
END IF
IF Turno(1) = 0 THEN
    SELECT CASE x$
        CASE "A": L11 = 7
        CASE "B": L11 = 8
        CASE "C": L11 = 9
        CASE "D": L11 = 10
        CASE "E": L11 = 11
        CASE "F": L11 = 12
        CASE "G": L11 = 13
        CASE "H": L11 = 14
    END SELECT
    SELECT CASE y$
        CASE "1": L12 = 49
        CASE "2": L12 = 52
        CASE "3": L12 = 55
        CASE "4": L12 = 58
        CASE "5": L12 = 61
        CASE "6": L12 = 64
        CASE "7": L12 = 67
        CASE "8": L12 = 70
    END SELECT
    INPUT #2, SN$
    LOCATE L11, L12 - 1
    IF SN$ = "Col" THEN
        PRINT " C "
        Punti(1) = Punti(1) + 1
    ELSE
        PRINT " X "
    END IF
    Turno(1) = NOT (SGN(Turno(1)))
END IF
IF Punti(1) = 17 THEN
    Inizio(2) = TIMER
    Vittoria Info$(1), 1
END IF
IF Punti(2) = 17 THEN
    Inizio(2) = TIMER
    Vittoria Avv$(1), 2
END IF
END SUB

SUB Titolo
    COLOR Col(1, 1), Col(1, 2)
    LOCATE 1
    PRINT SPACE$(80)
    Tit$ = "  < LSC  2 . 1 >   B a t t a g l i a   n a v a l e   1 . 0  "
    LOCATE 1, 40 - (LEN(Tit$)) / 2
    PRINT Tit$
    LOCATE 23
    PRINT SPACE$(80)
    LOCATE 23, 5
    PRINT "INVIO=Inizio   ESC=Esci"
END SUB

SUB Vittoria (Giocatore$, Vin)
    COLOR 1, 1: CLS
    COLOR 1, 7
    FOR x = 1 TO 11
        a = a + 1
        LOCATE 5 + a, 20
        PRINT SPACE$(42)
    NEXT
    a = 0
    COLOR 15, 7
    LOCATE 7, 21
    PRINT "Ъ" + STRING$(38, "Д") + "ї"
    COLOR 0, 7
    LOCATE 16, 21
    PRINT "А" + STRING$(38, "Д") + "Щ"
    FOR x = 1 TO 8
        a = a + 1
        LOCATE 7 + a, 60
        PRINT "і"
    NEXT
    a = 0
    COLOR 15, 7
    FOR x = 1 TO 8
        a = a + 1
        LOCATE 7 + a, 21
        PRINT "і"
    NEXT
    a = 0
    COLOR 0, 1
    FOR x = 1 TO 10
        a = a + 1
        LOCATE 6 + a, 62
        PRINT "Ы"
    NEXT
    LOCATE 7 + a, 23
    PRINT STRING$(40, "Я")
    COLOR 14, 7
    LOCATE 6, 33
    PRINT "Fine del gioco"
    a = INT(Inizio(2) - Inizio(1)): M$ = " secondi"
    IF a >= 60 THEN a = INT(a / 60): M$ = " minuti"
    IF a >= 60 THEN a = a / 60: M$ = " ore"
    IF Vin = 1 THEN
        F1$ = "Complimenti " + Giocatore$
        F2$ = "Hai vinto !"
        F3$ = "Hai affondato le navi di" + Avv$(1)
        F4$ = "in" + STR$(a) + M$
    ELSE
        F1$ = Info$(1) + " hai perso"
        F2$ = Giocatore$ + " ti ha affondato"
        F3$ = "tutte le navi in" + STR$(a) + M$
        F4$ = "Riprova a sfidarlo !"
    END IF
    F5$ = "Premi un tasto per proseguire"
    LOCATE 8, 41 - (LEN(F1$) / 2): PRINT F1$
    LOCATE 10, 41 - (LEN(F2$) / 2): PRINT F2$
    LOCATE 11, 41 - (LEN(F3$) / 2): PRINT F3$
    LOCATE 12, 41 - (LEN(F4$) / 2): PRINT F4$
    LOCATE 14, 41 - (LEN(F5$) / 2): PRINT F5$
    DO: LOOP WHILE INKEY$ = ""
    KILL "temp.lsc"
    RUN "COMUNI22"
END SUB

