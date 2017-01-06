DECLARE SUB Arrivo ()
DECLARE SUB Connessione ()
DECLARE SUB InvioFile ()
DECLARE SUB Strumenti ()
DECLARE SUB Opzioni ()
DECLARE SUB Info ()
DECLARE SUB Comunicazione ()
DECLARE SUB Menu ()

SCREEN 0
COLOR 7, 1: CLS
COLOR 4, 7: LOCATE 1: PRINT STRING$(80, " ")
LOCATE 1, 14
PRINT "L i n k   b y   S e r i a l   C o n n e c t o r         (L.S.C.)"
COLOR 0, 3
OPEN "Info.lsc" FOR INPUT AS #1
    LINE INPUT #1, Utente$
    LINE INPUT #1, Velocita$
CLOSE
LOCATE 2: PRINT STRING$(80, " ")
LOCATE 2, 8
PRINT "<Strumenti>            <Opzioni>                                      <?>"
LOCATE 3: COLOR 15, 1: PRINT "    Collegamento:"
LOCATE 3, 18: COLOR 14, 1: PRINT "         "
LOCATE 3, 24: COLOR 15, 1: PRINT STRING$(57, " ")
LOCATE 3, 26: PRINT "     Stato: Disconnesso             VelocitÖ: "; Velocita$
COLOR 0, 7: LOCATE 23: PRINT "  F10=Menó" + STRING$(69, " ")
COLOR 7, 1

LOCATE 4, 1: PRINT "…" + STRING$(38, "Õ") + "ª" + STRING$(38, "ƒ") + "ø"
a = 4
FOR x = 1 TO 18
    a = a + 1
    LOCATE a
    PRINT "∫" + STRING$(38, " ") + "∫" + STRING$(38, " ") + "≥"
NEXT
LOCATE a, 1: PRINT "»" + STRING$(38, "Õ") + "º" + STRING$(38, "ƒ") + "Ÿ"
LOCATE 4, 3: PRINT " " + Utente$ + " "
4 OPEN "trans.lsc" FOR INPUT AS #1
    INPUT #1, Tipo$
    INPUT #1, Tipo2$
CLOSE #1
IF Tipo2$ = "Connesso" THEN
    OPEN "trans.lsc" FOR OUTPUT AS #1
         PRINT #1, Tipo$
         PRINT #1, "N"
    CLOSE #1
    CALL Comunicazione
END IF
OPEN "trans.lsc" FOR OUTPUT AS #1
     PRINT #1, "N"
     PRINT #1, "N"
CLOSE #1
ON COM(1) GOSUB a
ON TIMER(30) GOSUB Prova
OPEN "COM2:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
COM(1) ON

3
Selezione:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(0) + "D": CALL Menu
        CASE CHR$(0) + "<"
            Conn$ = "Chiamata"
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, Conn$
                PRINT #1, "N"
            CLOSE #1
            CALL Connessione
        CASE CHR$(0) + "="
            Conn$ = "Dis"
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, Conn$
                PRINT #1, "N"
            CLOSE #1
            CALL Connessione
        CASE CHR$(0) + ";": CALL Info
        CASE CHR$(0) + ">": CALL Opzioni
        CASE CHR$(0) + "A": CALL InvioFile
        CASE CHR$(27)
            OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "N"
            PRINT #1, "N": CLOSE
            STOP
        CASE ELSE
            BEEP
            GOTO Selezione
    END SELECT
    PRINT Tasti$
    RUN

a: CALL Arrivo

Prova:
    RETURN

SUB Arrivo
    OPEN "trans.lsc" FOR INPUT AS #1
        INPUT #1, Tipo$
    CLOSE
    IF Tipo$ = "N" THEN
        OPEN "COM1:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
        a$ = ""
        WHILE LEN(a$) <> 3
            INPUT #3, a$
        WEND
        IF a$ = "$$$" THEN
            SLEEP 1
            PRINT #3, a$
            COLOR 7, 1
            LOCATE 3, 38
            PRINT "Connesso          "
            PRINT #3, a$
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "Risposta"
                PRINT #1, "Connesso"
            CLOSE #1
            GOTO Si
        ELSE
            COLOR 7, 1
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "N"
                PRINT #1, "N"
            CLOSE #1
            LOCATE 3, 38
            PRINT "Disconnesso        "
            CLOSE #1
            GOTO No
        END IF
    END IF
    IF Tipo$ = "Chiamata" THEN
        OPEN "trans.lsc" FOR INPUT AS #1
            INPUT #1, Tipo$
            INPUT #1, Codice$
        CLOSE #1
        OPEN "COM1:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
        a$ = ""
        WHILE LEN(a$) <> 3
           INPUT #3, a$
        WEND
        IF a$ = Codice$ THEN
            COLOR 7, 1
            LOCATE 3, 38
            PRINT "Connesso          "
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "Chiamata"
                PRINT #1, "Connesso"
            CLOSE #1
            GOTO Si
        ELSE
            COLOR 7, 1
            LOCATE 3, 38
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "N"
                PRINT #1, "N"
            CLOSE #1
            PRINT "Disconnesso        "
            GOTO No
        END IF
    END IF
    IF Tipo$ = "Conversazione" THEN
        END
    END IF
    IF Tipo$ <> "Chiamata" AND Tipo$ <> "N" AND Tipo$ <> "Conversazione" THEN
        OPEN "COM1:1200,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
            INPUT #3, Sped$
            INPUT #3, File$
        OPEN File$ FOR OUTPUT AS #1
        WHILE a$ <> "Fine"
            INPUT #3, a$
            PRINT #1, a$
        WEND
        END
    END IF
    RUN

Si:
    COLOR 15, 1: LOCATE 3, 38: PRINT "Connesso          "
    COLOR 0, 7: LOCATE 23, 20: PRINT "                     "
    Song$ = "MFT255O5L1F"
    PLAY Song$
    RUN 4
    CALL Comunicazione

No:
    RUN

END SUB

SUB Comunicazione
    OPEN "trans.lsc" FOR INPUT AS #1
        INPUT #1, Tipo$
    CLOSE #1
    OPEN "trans.lsc" FOR OUTPUT AS #1
        PRINT #1, Tipo$
        PRINT #1, "N"
    CLOSE #1
    RESET
    OPEN "info.lsc" FOR INPUT AS #1
        LINE INPUT #1, Nome$
        LINE INPUT #1, Velocita$
        LINE INPUT #1, Parita$
        LINE INPUT #1, Dati$
        LINE INPUT #1, Stop$
        LINE INPUT #1, CD$
        LINE INPUT #1, CS$
        LINE INPUT #1, DS$
        LINE INPUT #1, LF$
        LINE INPUT #1, OP$
        LINE INPUT #1, RB$
        LINE INPUT #1, RS$
        LINE INPUT #1, TB$
        LINE INPUT #1, NPorta$
        LINE INPUT #1, Tempo$
        LINE INPUT #1, Sped$
    CLOSE #1
    OPEN "COM1:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
    IF Tipo$ = "Chiamata" THEN
        SLEEP 3
        FOR x = 1 TO 1
           PRINT #3, Velocita$
        NEXT
        Vel2$ = ""
        WHILE Vel2$ = ""
            INPUT #3, Vel2$
        WEND
    ELSE
        Vel2$ = ""
        WHILE Vel2$ = ""
            INPUT #3, Vel2$
        WEND
        SLEEP 3
        FOR x = 1 TO 1
            PRINT #3, Velocita$
        NEXT
    END IF
     
    IF VAL(Vel2$) < VAL(Velocita$) THEN
        Velocita$ = Vel2$ + "  "
    ELSE
        Velocita$ = Velocita$ + "  "
    END IF
    COLOR 15, 1: LOCATE 3, 72: PRINT Velocita$
           
    IF Tipo$ = "Chiamata" THEN
        SLEEP 1
        FOR x = 1 TO 1
            PRINT #3, Nome$
        NEXT
        Connesso$ = ""
        WHILE Connesso$ = ""
            INPUT #3, Connesso$
        WEND
    ELSE
        Connesso$ = ""
        WHILE Connesso$ = ""
            INPUT #3, Connesso$
        WEND
        SLEEP 2
        FOR x = 1 TO 1
            PRINT #3, Nome$
        NEXT
    END IF
    COLOR 15, 1
    LOCATE 4, 1: PRINT "…" + STRING$(38, "Õ") + "À" + STRING$(38, "Õ") + "ª"
    a = 4
    FOR x = 1 TO 18
        a = a + 1
        LOCATE a
        PRINT "∫" + STRING$(38, " ") + "∫" + STRING$(38, " ") + "∫"
    NEXT
    LOCATE a, 1: PRINT "»" + STRING$(38, "Õ") + "º" + STRING$(38, "Õ") + "º"
    LOCATE 4, 3: PRINT " " + Nome$ + " "
    LOCATE 4, 43: PRINT " " + Connesso$ + " "
      
    IF Tipo$ = "Chiamata" THEN
        SLEEP 3
        FOR x = 1 TO 5
            PRINT #3, "010101"
        NEXT
    ELSE
        CC$ = ""
        WHILE CC$ = ""
            INPUT #3, CC$
        WEND
        COLOR 15, 1: LOCATE 3, 19: PRINT CC$
    END IF
    Porta$ = "COM" + NPorta$ + ":" + Velocita$ + "," + Parita$ + "," + Dati$ + "," + Stop$ + ",CD" + CD$ + ",CS" + CS$ + ",DS" + DS$
    Porta$ = Porta$ + ",OP" + OP$
    IF LF$ = "1" THEN Porta$ = Porta$ + ",LF"
    IF RS$ = "1" THEN Porta$ = Porta$ + ",RS"
    Porta$ = Porta$ + ",TB" + TB$ + ",RB" + RB$
    OPEN "trans.lsc" FOR OUTPUT AS #1
        PRINT #1, "Conversazione"
        PRINT #1, "Connesso"
    CLOSE #1
    CLOSE #3
    OPEN Porta$ FOR RANDOM AS #3
    a = 0
    b = 0

Conversazione:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(0) + ";" THEN CALL Info
    IF Tasti$ = CHR$(0) + "A" THEN CALL InvioFile
    IF Tasti$ = CHR$(27) THEN
        OPEN "trans.lsc" FOR OUTPUT AS #1
        PRINT #1, "N"
        PRINT #1, "N": CLOSE
        STOP
    END IF
    IF Tasti$ = CHR$(0) + "<" THEN
        OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "Conversazione"
            PRINT #1, "Connesso"
        CLOSE #1
        CALL Connessione
    END IF
    IF Tasti$ = CHR$(0) + "=" THEN
        Conn = 0
        OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "N"
            PRINT #1, "N"
        CLOSE #1
        CALL Connessione
    END IF
    IF Tasti$ = CHR$(27) THEN
        OPEN "trans.lsc" FOR OUTPUT AS #1
        PRINT #1, "N"
        PRINT #1, "N": CLOSE
        STOP
    END IF
    IF Tasti$ = CHR$(8) THEN
        a = a - 1
        Scritta$ = LEFT$(Scritta$, a * b)
        GOTO Conversazione
    END IF
    IF Tasti$ = CHR$(13) THEN GOSUB InvioMessaggio
    IF Tasti$ = CHR$(32) THEN Tasti$ = CHR$(250)
    a = a + 1
    Scritta$ = Scritta$ + Tasti$
    IF a > 36 THEN
        b = b + 1
        a = 0
    END IF
    IF b >= 17 THEN b = 0
    LOCATE 5 + b, 3
    PRINT RIGHT$(Scritta$, a)
    GOTO Conversazione

InvioMessaggio:
    IF Sped$ = "P" THEN
        PRINT #3, Scritta$
    END IF
    b = b + 1
    IF Sped$ = "L" THEN
        FOR x = 1 TO LEN(Scritta$)
            aa = aa + 1
            Scritta1$ = RIGHT$(LEFT$(Scritta$, aa), 1)
            PRINT #3, Scritta1$
        NEXT
    END IF
    RETURN
    
END SUB

SUB Connessione
OPEN "trans.lsc" FOR INPUT AS #1
    INPUT #1, Conn$
CLOSE #1
SELECT CASE Conn$
    CASE IS = "Chiamata": GOSUB Connetti
    CASE IS = "Dis": GOSUB Disconnetti
END SELECT

Disconnetti:
    IF Conn = 1 THEN
        Conn = 0
        CLOSE #1
        LOCATE 3, 38
        PRINT "Disconnesso"
        RUN
    ELSE
       COLOR 15, 4
       LOCATE 10, 31
       PRINT "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
       LOCATE 11, 31
       PRINT "€    Disconnesso    €"
       LOCATE 12, 31
       PRINT "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
       SLEEP 4
       RUN
    END IF

Connetti:
   
    OPEN "info.lsc" FOR INPUT AS #1
        LINE INPUT #1, Nome$
        LINE INPUT #1, Velocita$
        LINE INPUT #1, Parita$
        LINE INPUT #1, Dati$
        LINE INPUT #1, Stop$
        LINE INPUT #1, CD$
        LINE INPUT #1, CS$
        LINE INPUT #1, DS$
        LINE INPUT #1, LF$
        LINE INPUT #1, OP$
        LINE INPUT #1, RB$
        LINE INPUT #1, RS$
        LINE INPUT #1, TB$
        LINE INPUT #1, NPorta$
        LINE INPUT #1, Tempo$
    CLOSE #1
    COLOR 15, 1: LOCATE 3, 19: PRINT CHR$(26)
    Tasti$ = "": a = 0
    WHILE a < 6
        Tasti$ = ""
        WHILE Tasti$ = ""
            Tasti$ = INKEY$
        WEND
            IF Tasti$ = CHR$(27) THEN
                OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "N"
                PRINT #1, "N": CLOSE
                STOP
            END IF
            IF Tasti$ = CHR$(8) THEN
                LOCATE 3, 18: COLOR 14, 1: PRINT "   .     "
                RUN 3
            END IF
                 a = a + 1: Cod$ = Cod$ + Tasti$
        LOCATE 3, 19: PRINT Cod$
    WEND
    CLOSE #3
    OPEN "COM1:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
    COLOR 7, 1: LOCATE 3, 38: PRINT "Chiamata in corso"
    LOCATE 23, 20: COLOR 0, 7: PRINT "A     B": LOCATE 23, 21: COLOR 14, 7: PRINT "√ƒƒƒ¥"
    COLOR 12, 7: LOCATE 23, 22: PRINT "˛": aa = 1
    a = 0
    FOR x = 1 TO 3
        a = a + 2
        Cod1$ = LEFT$(RIGHT$(Cod$, a), 2)
        Codice$ = CHR$(VAL(Cod1$) + 35) + Codice$
    NEXT
    OPEN "trans.lsc" FOR OUTPUT AS #1
        PRINT #1, "Chiamata"
        PRINT #1, Codice$
    CLOSE #1
    GOSUB Chiamata

Chiamata:
    IF Conn = 0 THEN
        Tempo = (VAL(Tempo$) * 2) * 1000
        a = 0
        WHILE Tempo > 0
            PRINT #3, Codice$
            Tempo = INT((Tempo / 2))
            FOR x = 1 TO Tempo
                a = a + 1
                IF a MOD 750 = 0 THEN
                    aa = aa + 1
                    IF aa > 3 THEN aa = 1
                    COLOR 14, 7: LOCATE 23, 21: PRINT "√ƒƒƒ¥"
                    COLOR 12, 7: LOCATE 23, 21 + aa: PRINT "˛"
                END IF
            NEXT
        WEND
       RUN
    ELSE
       COLOR 15, 4
       LOCATE 10, 31
       PRINT "€ﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂﬂ€"
       LOCATE 11, 31
       PRINT "€   Gia  connesso   €"
       LOCATE 12, 31
       PRINT "€‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹‹€"
       SLEEP 4
       RUN
    END IF
    CLOSE #1
    RUN

END SUB

SUB Info
COLOR 0, 7
Larg = 50
Lung = 8
DisB = (80 / 2) - (Larg / 2)
DisH = (22 / 2) - (Lung / 2)
LOCATE DisH, DisB
PRINT "⁄" + STRING$(Larg, "ƒ") + "ø"
a = 0
FOR x = 1 TO Lung
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "≥" + STRING$(Larg, " ") + "≥€"
NEXT
LOCATE DisH + a, DisB
PRINT "¿" + STRING$(Larg, "ƒ") + "Ÿ€"
LOCATE DisH + a + 1, DisB + 2: COLOR 0, 1: PRINT STRING$(Larg + 1, "ﬂ")
COLOR 0, 7
LOCATE DisH, DisB + 3: PRINT "Informazioni su L.S.C."
LOCATE DisH + 1, DisB + 14
PRINT "Link by Serial Connector"
LOCATE DisH + 2, DisB + 11
PRINT "Copyright (C) Col S.p.A. 2000"
LOCATE DisH + 3, DisB + 5
PRINT "Svilupato interamente su Software QBasic"
LOCATE DisH + 4, DisB + 9
PRINT "Copyright (C) Microsoft 1987-1993"
LOCATE DisH + 5, DisB + 14
PRINT "Prodotto dalla Col S.p.A."
LOCATE DisH + 6, DisB + 20
PRINT "Versione 1.0"
LOCATE DisH + 7, DisB + 22
COLOR 15, 3: PRINT CHR$(16) + "  OK  " + CHR$(17)

Tasti$ = ""
WHILE Tasti$ = ""
    Tasti$ = INKEY$
WEND
RUN
END SUB

SUB InvioFile
Larg = 44
Lung = 6
DisB = (80 / 2) - (Larg / 2)
DisH = (22 / 2) - (Lung / 2)
COLOR 0, 7
LOCATE DisH, DisB
PRINT "⁄" + STRING$(Larg, "ƒ") + "ø"
a = 0
FOR x = 1 TO Lung
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "≥" + STRING$(Larg, " ") + "≥€"
NEXT
LOCATE DisH + a, DisB
PRINT "¿" + STRING$(Larg, "ƒ") + "Ÿ€"
LOCATE DisH + a + 1, DisB + 2: COLOR 0, 1: PRINT STRING$(Larg + 1, "ﬂ")
COLOR 0, 7
LOCATE DisH, DisB + 3
PRINT "Invio File "
LOCATE DisH + 1, DisB + 3: PRINT "File da inviare: "
LOCATE DisH + 2, DisB + 3: PRINT "Percorso file: "
LOCATE DisH + 3, DisB + 3: PRINT "Grandezza file:"
LOCATE DisH + 4, DisB + 3: PRINT "Sistema di invio:"
LOCATE DisH + 5, DisB + ((Larg / 2) - 5)
PRINT "A" + STRING$(10, " ") + "B"
COLOR 14, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4): PRINT STRING$(10, "ƒ")
COLOR 0, 7
LOCATE DisH + 1, DisB + 19: INPUT File$
LOCATE DisH + 1, DisB + 19: PRINT File$ + "    "
IF File$ = "" THEN GOSUB Esci
LOCATE DisH + 2, DisB + 17: INPUT Percorso$
LOCATE DisH + 2, DisB + 17: PRINT Percorso$ + "    "
IF RIGHT$(Percorso$, 1) = "\" THEN
    FileIn$ = Percorso$ + File$
ELSE
    FileIn$ = Percorso$ + "\" + File$
END IF
OPEN FileIn$ FOR INPUT AS #1
Grand = LOF(1)
CLOSE #1
LOCATE DisH + 3, DisB + 3
PRINT "Grandezza file:"; Grand; "byte"
IF Grand > 1024 THEN
    IF Grand > 1048576 THEN
        IF Grand > 6291456 THEN Sped$ = "LN"
    ELSE
        Sped$ = "P"
    END IF
ELSE
    Sped$ = "L"
END IF
IF Sped$ = "LN" THEN SSped$ = "Linea"
IF Sped$ = "L" THEN SSped$ = "Lettera"
IF Sped$ = "P" THEN SSped$ = "Parola"
LOCATE DisH + 4, DisB + 21: PRINT SSped$
COLOR 12, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4): PRINT "˛"
GOSUB InvioDelFile
GOSUB Esci

Esci:
    a = -1
    COLOR 7, 1
    FOR x = 1 TO Lung + 2
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT STRING$(Larg + 3, " ")
    NEXT
    a = -1
    FOR x = 1 TO Lung + 2
        a = a + 1
        LOCATE DisH + a, 40: PRINT "∫"
    NEXT
    RUN 3

InvioDelFile:
    CLOSE #3
    OPEN "COM1:2400,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #3
        PRINT #3, "File"
        PRINT #3, "File"
        PRINT #3, Sped$
        PRINT #3, File$
    OPEN FileIn$ FOR INPUT AS #1
    IF Sped$ = "L" THEN
        WHILE EOF(1) = 0
            a = a + 1
                LINE INPUT #1, LFile$
                PRINT #3, LFile$
            LFile = LEN(LFile$)
            Grand = Grand - LFile
            LOCATE DisH + 3, DisB + 3
            PRINT "Grandezza file:"; Grand; "byte        "
            IF aa >= 9 THEN aa = 0
            IF a MOD 1 = 0 THEN
                aa = aa + 1
                COLOR 14, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4): PRINT STRING$(10, "ƒ")
                COLOR 12, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4) + aa: PRINT "˛"
                COLOR 0, 7
            END IF
        WEND
        CLOSE #1
    END IF
   
    IF Sped$ = "P" THEN
        WHILE EOF(1) = 0
            a = a + 1
                LINE INPUT #1, LFile$
                PRINT #3, LFile$
            LFile = LEN(LFile$)
            Grand = Grand - LFile
            LOCATE DisH + 3, DisB + 3
            PRINT "Grandezza file:"; Grand; "byte        "
            IF aa >= 9 THEN aa = 0
            IF a MOD 1 = 0 THEN
                aa = aa + 1
                COLOR 14, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4): PRINT STRING$(10, "ƒ")
                COLOR 12, 7: LOCATE DisH + 5, DisB + ((Larg / 2) - 4) + aa: PRINT "˛"
                COLOR 0, 7
            END IF
        WEND
        CLOSE #1
    END IF
   
    IF Sped$ = "LN" THEN
    END IF
    RETURN
END SUB

SUB Menu
DIM dMenu(10)
COLOR 14, 3
LOCATE 2, 8
PRINT CHR$(16)
dMenu(1) = 8
dMenu(2) = 31
dMenu(3) = 78
MenuS = 1
Selezione1:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): RUN
        CASE CHR$(27)
            OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "N"
            PRINT #1, "N": CLOSE
            STOP
        CASE CHR$(0) + "K": GOSUB Sinistra
        CASE CHR$(0) + "M": GOSUB Destra
        CASE CHR$(13): GOSUB Invio
        CASE ELSE:
            BEEP
            GOSUB Selezione1
    END SELECT
    CALL Menu

Sinistra:
    IF MenuS = 1 THEN
        BEEP
        GOSUB Selezione1
    END IF
    IF MenuS = 2 THEN
        MenuS = MenuS - 1
        COLOR 14, 3
        LOCATE 2, dMenu(MenuS)
        PRINT CHR$(16)
        COLOR 0, 3
        LOCATE 2, dMenu(MenuS + 1)
        PRINT "<"
    END IF
    IF MenuS = 3 THEN
        MenuS = MenuS - 1
        COLOR 14, 3
        LOCATE 2, dMenu(MenuS)
        PRINT CHR$(16)
        COLOR 0, 3
        LOCATE 2, dMenu(MenuS + 1)
        PRINT "<"
END IF
    GOSUB Selezione1

Destra:
    IF MenuS = 3 THEN
        BEEP
        GOSUB Selezione1
    END IF
    IF MenuS = 2 THEN
        MenuS = MenuS + 1
        COLOR 14, 3
        LOCATE 2, dMenu(MenuS)
        PRINT CHR$(16)
        COLOR 0, 3
        LOCATE 2, dMenu(MenuS - 1)
        PRINT "<"
    END IF
    IF MenuS = 1 THEN
        MenuS = MenuS + 1
        COLOR 14, 3
        LOCATE 2, dMenu(MenuS)
        PRINT CHR$(16)
        COLOR 0, 3
        LOCATE 2, dMenu(MenuS - 1)
        PRINT "<"
    END IF
    GOSUB Selezione1

Invio:
    IF MenuS = 1 THEN CALL Strumenti
    IF MenuS = 2 THEN CALL Opzioni
    IF MenuS = 3 THEN CALL Info

END SUB

SUB Opzioni
DIM MenuItem$(20)
COLOR 0, 7
OPEN "info.lsc" FOR INPUT AS #1
    LINE INPUT #1, Nome$
    LINE INPUT #1, Velocita$
    LINE INPUT #1, Parita$
    LINE INPUT #1, Dati$
    LINE INPUT #1, Stop$
    LINE INPUT #1, CD$
    LINE INPUT #1, CS$
    LINE INPUT #1, DS$
    LINE INPUT #1, LF$
    LINE INPUT #1, OP$
    LINE INPUT #1, RB$
    LINE INPUT #1, RS$
    LINE INPUT #1, TB$
    LINE INPUT #1, NPorta$
    LINE INPUT #1, Tempo$
    LINE INPUT #1, Sped$
CLOSE #1
IF Sped$ = "L" THEN Spedizione$ = "Lettera"
IF Sped$ = "P" THEN Spedizione$ = "Parola"

MenuItem$(1) = "VelocitÖ:                  " + Velocita$
MenuItem$(2) = "Bit dati:                  " + Dati$
MenuItem$(3) = "Tempo di prova chiamata:   " + Tempo$
MenuItem$(4) = "Nome Utente: " + Nome$
MenuItem$(5) = "Buffer di arrivo:          " + RB$
MenuItem$(6) = "Buffer del trasmittente:   " + TB$
MenuItem$(7) = "Tipo di trasmissione:   " + Spedizione$

                                          
Larg = 36
Lung = 8
DisB = (80 / 2) - (Larg / 2)
DisH = (22 / 2) - (Lung / 2)
LOCATE DisH, DisB
PRINT "⁄" + STRING$(Larg, "ƒ") + "ø"
a = 0
FOR x = 1 TO Lung
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "≥" + STRING$(Larg, " ") + "≥€"
NEXT
LOCATE DisH + a, DisB
PRINT "¿" + STRING$(Larg, "ƒ") + "Ÿ€"
LOCATE DisH + a + 1, DisB + 2: COLOR 0, 1: PRINT STRING$(Larg + 1, "ﬂ")
COLOR 0, 7
LOCATE DisH, DisB + 3: PRINT "Opzioni"
a = 0
FOR x = 1 TO Lung - 1
    a = a + 1
    LOCATE DisH + a, DisB + 1: PRINT STR$(a) + "-" + MenuItem$(a)
NEXT
Selezione3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(49): GOSUB SelVelocita
        CASE CHR$(50): GOSUB SelDati
        CASE CHR$(51): GOSUB SelTempo
        CASE CHR$(52): GOSUB SelNome
        CASE CHR$(53): GOSUB SelBuffArr
        CASE CHR$(54): GOSUB SelBuffTra
        CASE CHR$(55): GOSUB SelSped
        CASE CHR$(8): RUN
        CASE CHR$(0) + "D": CALL Menu
        CASE CHR$(0) + "<"
            Conn = 1
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, Conn
                PRINT #1, "Connesso"
            CLOSE #1
            CALL Connessione
        CASE CHR$(0) + "="
            Conn = 0
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "N"
                PRINT #1, "N"
            CLOSE #1
            CALL Connessione
        CASE CHR$(27)
            OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "N"
            PRINT #1, "N": CLOSE
            STOP
        CASE CHR$(0) + ";": CALL Info
        CASE ELSE
            BEEP
            GOTO Selezione3
    END SELECT
    GOTO Selezione3


SelVelocita:
    LOCATE 8, 53: PRINT "____"
    LOCATE 8, 51: INPUT ; V$
    IF V$ <> "75" AND V$ <> "110" AND V$ <> "150" AND V$ <> "300" AND V$ <> "600" AND V$ <> "1200" AND V$ <> "2400" AND V$ <> "4800" AND V$ <> "9600" THEN
        BEEP
        GOTO SelVelocita
    END IF
    Velocita$ = V$
    LOCATE 8, 51: PRINT "  " + Velocita$
    GOSUB Salva
    RETURN

SelDati:
    LOCATE 9, 53: PRINT "_"
    LOCATE 9, 51: INPUT ; Dati$
    IF VAL(Dati$) > 8 THEN Dati$ = "8"
    IF VAL(Dati$) < 7 THEN Dati$ = "7"
    LOCATE 9, 51: PRINT "  " + Dati$
    GOSUB Salva
    RETURN

SelTempo:
    LOCATE 10, 53: PRINT "___"
    LOCATE 10, 51: INPUT ; Tempo$
    LOCATE 10, 51: PRINT "  " + Tempo$
    GOSUB Salva
    RETURN

SelNome:
    LOCATE 11, 40: PRINT "__________________"
    LOCATE 11, 38: INPUT ; Nome$
    LOCATE 11, 38: PRINT "  " + Nome$
    GOSUB Salva
    RETURN

SelBuffArr:
    LOCATE 12, 53: PRINT "____"
    LOCATE 12, 51: INPUT ; RB$
    LOCATE 12, 51: PRINT "  " + RB$
    GOSUB Salva
    RETURN

SelBuffTra:
    LOCATE 13, 53: PRINT "____"
    LOCATE 13, 51: INPUT ; TB$
    LOCATE 13, 51: PRINT "  " + TB$
    GOSUB Salva
    RETURN

SelSped:
    LOCATE 14, 50: PRINT "_______"
    LOCATE 14, 48: INPUT Sped$
    IF Sped$ = "Lettera" OR Sped$ = "Parola" THEN
        Sped$ = LEFT$(Sped$, 1)
    ELSEIF Sped$ = "Linea" THEN
        Sped$ = "LN"
    ELSE
        BEEP
        GOSUB SelSped
    END IF
    LOCATE 14, 48: PRINT "  " + Sped$
    GOSUB Salva
    RETURN

END
Salva:
    OPEN "info.lsc" FOR OUTPUT AS #1
        PRINT #1, Nome$
        PRINT #1, Velocita$
        PRINT #1, Parita$
        PRINT #1, Dati$
        PRINT #1, Stop$
        PRINT #1, CD$
        PRINT #1, CS$
        PRINT #1, DS$
        PRINT #1, LF$
        PRINT #1, OP$
        PRINT #1, RB$
        PRINT #1, RS$
        PRINT #1, TB$
        PRINT #1, NPorta$
        PRINT #1, Tempo$
        PRINT #1, Sped$
    CLOSE #1
    RETURN


END SUB

SUB Strumenti
DIM MenuItem$(20)
COLOR 0, 7
MenuItem$(1) = "Connetti      F2"
MenuItem$(2) = "Disconnetti   F3"
MenuItem$(3) = "Invia file    F7"
Larg = 20
Lung = 4
DisB = (80 / 2) - (Larg / 2)
DisH = (22 / 2) - (Lung / 2)
LOCATE DisH, DisB
PRINT "⁄" + STRING$(Larg, "ƒ") + "ø"
a = 0
FOR x = 1 TO Lung
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "≥" + STRING$(Larg, " ") + "≥€"
NEXT
LOCATE DisH + a, DisB
PRINT "¿" + STRING$(Larg, "ƒ") + "Ÿ€"
LOCATE DisH, DisB + 3: PRINT "Strumenti"
LOCATE DisH + a + 1, DisB + 2: COLOR 0, 1: PRINT STRING$(Larg + 1, "ﬂ")
COLOR 0, 7
a = 0
FOR x = 1 TO Lung - 1
    a = a + 1
    LOCATE DisH + a, DisB + 1: PRINT STR$(a) + "-" + MenuItem$(a)
NEXT
Selezione2:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(49)
            GOSUB Cancella
            Conn = 1
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "Chiamata"
                PRINT #1, "Connesso"
            CLOSE #1
            CALL Connessione
        CASE CHR$(50)
            GOSUB Cancella
            Conn = 0
            OPEN "trans.lsc" FOR OUTPUT AS #1
                PRINT #1, "N"
                PRINT #1, "N"
            CLOSE #1
            CALL Connessione
        CASE CHR$(51)
            CALL InvioFile
        CASE CHR$(27)
            OPEN "trans.lsc" FOR OUTPUT AS #1
            PRINT #1, "N"
            PRINT #1, "N": CLOSE
            STOP
        CASE CHR$(8): RUN
        CASE ELSE
            BEEP
            GOSUB Selezione2
    END SELECT
    GOSUB Selezione2

Cancella:
    a = -1
    COLOR 7, 1
    FOR x = 1 TO Lung + 2
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT STRING$(Larg + 2, " ")
    NEXT
    a = -1
    FOR x = 1 TO Lung + 2
        a = a + 1
        LOCATE DisH + a, 40: PRINT "∫"
    NEXT
    RETURN
END SUB

