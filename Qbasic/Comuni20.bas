DECLARE SUB InvioOp ()
DECLARE SUB Page1Up (Text$, Riga!)
DECLARE SUB IScritte (aa!)
DECLARE SUB Sinistra (Alt!, Text$, DisB!)
DECLARE SUB Scritte ()
DECLARE SUB Risposta ()
DECLARE SUB Connessione ()
DECLARE SUB PInfor ()
DECLARE SUB Tempo ()
DECLARE SUB Informazioni ()
DECLARE SUB Connetti ()
DECLARE SUB Disconnetti ()
DECLARE SUB Invio ()
DECLARE SUB Opzioni ()
DECLARE SUB Utente ()
DECLARE SUB MenuDef ()
DECLARE SUB Center (Alt!, Text$, Fin!)
DECLARE SUB Finestra (Alt!, Larg!, Text$)
DECLARE SUB Esecuzione ()
DECLARE SUB Menu (Selezione)
DECLARE SUB Pagina1 ()
DECLARE SUB Pagina2 ()
DECLARE SUB Preparativi ()
DECLARE SUB MenuVuoto ()
DECLARE SUB Titolo ()
DECLARE SUB Colore ()

REM Link by Serial Connector 2.0

SCREEN 0
CLS

 'Dimensionamento Matrici
'$STATIC
DIM SHARED Info$(16)
DIM SHARED NMenu(3)
DIM SHARED NMenu$(3)
DIM SHARED Conn(1)
DIM SHARED MSel(1)
DIM SHARED TipChi(1)
DIM SHARED NSoft$(2)
DIM SHARED USoft$(1)
DIM SHARED VSoft$(1)
DIM SHARED Tasti$(1)
DIM SHARED a$(2)
DIM SHARED Porta$(3)
DIM SHARED Frase$(200)
DIM SHARED b(20)
DIM SHARED c(1)
'$DYNAMIC
DIM SHARED Col(8, 2)
DIM SHARED TipMenu$(3, 6)
 'Costanti
CONST Info$ = "Info2.lsc"
CONST Soft$ = "0003"
     'Titolo  Men—  Pagina  Stato  Errore  Attivo  MSel  Punto
Colorato:
DATA   4,7,   0,3,    7,1,  15,1,   14,4,  15,1,   14,3,  12,7
BlackW:
DATA   0,7,   7,0,    7,0,   7,0,   15,0,  15,0,   15,0,   0,7
PInfor
Colore
MenuDef
ON COM(VAL(Info$(9))) GOSUB Chiamata
Esecuzione

Chiamata:
    IF TipChi(1) = 1 THEN
        Connessione
    ELSE
        Risposta
    END IF

RETURN

REM $STATIC
SUB Center (Alt, Text$, Fin)
    DisB = 40 - Fin / 2 + 1
    LOCATE Alt, DisB
    PRINT Text$
END SUB

SUB Colore
    IF Info$(2) = "COLORE" THEN RESTORE Colorato:  ELSE RESTORE BlackW
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

SUB Connessione
    IF Conn(1) = 2 AND c(1) = 0 THEN Scritte
    IF Conn(1) = 1 THEN
        IF c(1) = 1 THEN Conn(1) = 2
        EXIT SUB
    END IF
    IF Conn(1) = 2 THEN
        IF c(1) = 1 THEN c(1) = 0
        EXIT SUB
    END IF
    IF Conn(1) > 1 THEN Scritte
    INPUT #2, a$
    IF a$ = NSoft$(1) THEN
        Conn(1) = Conn(1) + 1
        INPUT #2, USoft$(1)
        INPUT #2, VSoft$(1)
        IF VAL(Info$(5)) > VAL(VSoft$(1)) THEN Info$(5) = VSoft$(1)
        SLEEP 1
        PRINT #2, Info$(5)
        SLEEP 1
        PRINT #2, Info$(1)
        SLEEP 1
        PRINT #2, Soft$
        SLEEP 1
        PRINT #2, Soft$
    END IF
END SUB

SUB Connetti
Immissione:
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    IF Tasti$(1) = CHR$(27) THEN STOP
    IF Tasti$(1) = CHR$(8) THEN
        IF a < 1 THEN
            BEEP
            GOTO Immissione
        END IF
        a = a - 1
        a$ = LEFT$(a$, LEN(a$) - 1)
    ELSE
        a = a + 1
        a$ = a$ + Tasti$(1)
    END IF
    LOCATE 3, 22
    PRINT a$ + " "
    IF a < 4 THEN GOTO Immissione
    NSoft$(1) = a$
    LOCATE 3, 37: PRINT "Chiamata in corso"
    CLOSE
    Porta$(1) = "COM" + Info$(9) + ":75," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
    OPEN Porta$(1) FOR RANDOM AS #2
    COM(VAL(Info$(9))) ON
    TipChi(1) = 1
    LOCATE 3, 22
    a = 1
    TIMER ON
    FOR y = 1 TO VAL(Info$(4)) / a
        b = TIMER
        PRINT #2, NSoft$(1)
        FOR x = 1 TO (VAL(Info$(4)) / 10) * 100000 * VAL(Info$(3)) / a
            IF Conn(1) = 1 THEN EXIT SUB
            IF b MOD 100 * VAL(Info$(3)) = 0 THEN GOSUB Punto2
            b = b + 1
        NEXT
        a = a * 2
        c = TIMER
        IF c - b = 0 THEN EXIT FOR
        IF Conn(1) = 1 THEN EXIT SUB
    NEXT
    RUN

Punto2:
    COLOR Col(5, 1), Col(1, 2)
    aa = aa + 1
    IF aa > 9 THEN aa = 0
    F1$ = "AÃ" + STRING$(10, "Ä") + "´B"
    Center 23, F1$, 50
    LOCATE 23, 18 + aa
    COLOR Col(8, 1), Col(8, 2)
    PRINT CHR$(254)
    COLOR Col(5, 1), Col(1, 2)
RETURN

END SUB

SUB Disconnetti
    IF Conn(1) = 0 THEN
        COLOR Col(7, 1), Col(7, 2)
        Finestra 2, 20, ""
        COLOR Col(7, 1), Col(7, 2)
        Center 11, "Gia Disconnesso", 16
        SLEEP 5
        RUN
    ELSE
        PRINT #2, "Disconnetti"
        PRINT #2, "Disconnetti"
        Conn(1) = 0
    END IF
END SUB

SUB Esecuzione
    Titolo
    Preparativi
    Pagina1
    Pagina2
    CLOSE
    SLEEP 2
    IF Conn(1) = 0 THEN
        CLOSE
        Porta$(1) = "COM" + Info$(9) + ":75," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
    ELSE
        Porta$(1) = "COM" + Info$(9) + ":" + Info$(5) + "," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
        Conn(1) = 2
        c(1) = 0
    END IF
    OPEN Porta$(1) FOR RANDOM AS #2
    COM(VAL(Info$(9))) ON
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27): STOP
        CASE CHR$(0) + ";": Informazioni
        CASE CHR$(0) + "<": Connetti
        CASE CHR$(0) + "=": Disconnetti
        CASE CHR$(0) + ">": Tempo
        CASE CHR$(0) + "?": Opzioni
        CASE CHR$(0) + "A": Invio
        CASE CHR$(0) + "D": Menu 1
        CASE CHR$(13)
            aa = 1
            GOSUB ScritteA
        CASE ELSE
            BEEP
            Esecuzione
    END SELECT
    SELECT CASE MSel(1)
        CASE 1: Connetti
        CASE 2: Disconnetti
        CASE 3: Invio
        CASE 4: STOP
        CASE 5: Utente
        CASE 6: Tempo
        CASE 7: Opzioni
        CASE 8: InvioOp
        CASE 9: Informazioni
    END SELECT
    IF Conn(1) = 1 THEN Esecuzione
    RUN

ScritteA:
    c(1) = 1
    LOCATE 23, 5
    COLOR 4, 7
    PRINT "INVIO=Scivere ESC+F10=Menu"
    COLOR Col(6, 1), Col(6, 2)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27)
            Esecuzione
        CASE CHR$(44)
            Frase$(aa) = Frase$(aa) + "ú"
        CASE CHR$(8)
            IF a < 1 THEN
                BEEP: BEEP
                GOTO ScritteA
            END IF
            a = a - 1
            Frase$(aa) = LEFT$(Frase$(aa), LEN(Frase$(aa)) - 1)
        CASE CHR$(13)
            IScritte aa
            aa = aa + 1
            Frase$(aa - 1) = ""
            GOTO ScritteA
        CASE ELSE
            IF aa > 16 THEN aa = 1
            a = a + 1
            Frase$(aa) = Frase$(aa) + Tasti$(1)
    END SELECT
    Frase$(aa - 1) = ""
    Sinistra 4 + aa, Frase$(aa), 3
    IF a > 33 THEN a = 0: aa = aa + 1: IScritte aa - 1
    GOTO ScritteA
END SUB

SUB Finestra (Alt, Larg, Text$)
    DisH = 11 - Alt / 2
    DisB = 40 - (Larg / 2)
    LOCATE DisH, DisB
    PRINT "Ú" + STRING$(Larg, "Ä") + "¿"
    LOCATE DisH, DisB + Larg / 6
    PRINT Text$
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "³" + STRING$(Larg, " ") + "³"
    NEXT
    LOCATE DisH + a, DisB
    PRINT "À" + STRING$(Larg, "Ä") + "Ù"
    COLOR Col(2, 1), Col(3, 2)
    a = 0
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB + Larg + 2
        PRINT "Û"
    NEXT
    LOCATE DisH + a + 1, DisB + 2
    PRINT STRING$(Larg + 1, "ß")
END SUB

SUB Informazioni
    COLOR Col(7, 1), Col(7, 2)
    Finestra 8, 40, "Informazioni su L.S.C."
    COLOR Col(7, 1), Col(7, 2)
    Center 8, "Link by Serial Connector", 26
    Center 9, "Copyright (C) Col S.p.A. 2000", 32
    Center 10, "Svilupato interamente su Software QBasic", 40
    Center 11, "Copyright (C) Microsoft 1987-1993", 34
    Center 12, "Prodotto dalla Col S.p.A.", 26
    Center 13, "Versione 2.0", 14
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    RUN
END SUB

SUB Invio
    IF Conn(1) = 0 THEN
        COLOR Col(7, 1), Col(7, 2)
        Finestra 4, 30, "Messaggio di sistema"
        COLOR Col(7, 1), Col(7, 2)
        Center 10, "Impossibile inviare file", 26
        Center 11, "Non si Š collegati a", 20
        Center 12, "nessun computer", 16
        SLEEP 10
        RUN
    END IF
    COLOR Col(7, 1), Col(7, 2)
    Finestra 8, 30, "Invia file"
    COLOR Col(7, 1), Col(7, 2)
    Center 8, "Scrivi il percorso del file", 28
    Center 10, "Scrivi il nome del file", 24
    Center 14, "AÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´B", 20
    COLOR Col(1, 1), Col(1, 2)
    Center 9, SPACE$(27), 28
    Center 11, SPACE$(27), 28
    CLOSE
    LOCATE 9, 27: INPUT Perc$
    IF Perc$ = "" THEN
        Perc$ = "C:"
    ELSE
        IF RIGHT$(Perc$, 1) <> "\" THEN Perc$ = Perc$ + "\"
    END IF
    LOCATE 9, 27: PRINT Perc$ + "  "
    LOCATE 11, 27: INPUT File$
    LOCATE 11, 27: PRINT File$ + "  "
    COLOR Col(7, 1), Col(7, 2)
    Porta$(1) = "COM" + Info$(9) + ":" + Info$(5) + "," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
    OPEN Porta$(1) FOR RANDOM AS #2
    PRINT #2, "File"
    SLEEP 2
    PRINT #2, File$
    SLEEP 2
    CLOSE
    Porta$(2) = "COM" + Info$(9) + ":9600," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
    OPEN Perc$ + File$ FOR INPUT AS #1
    OPEN Porta$(2) FOR RANDOM AS #2
    IF Info$(11) = "S" THEN
        IF Info$(13) = "S" THEN
            PRINT #2, "ASCIIR"
            GOSUB ASCIIR2
        END IF
        PRINT #2, "ASCII"
        GOSUB ASCII2
    ELSE
        PRINT #2, "Norm"
    END IF
    SLEEP 1
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
            PRINT #2, a$
        FOR x = 1 TO 700 * VAL(Info$(3)): NEXT
        g = g + LEN(a$)
        Grand$ = "Grandezza:" + STR$(g)
        Center 12, Grand$, 18
        GOSUB Punto
    WEND
    PRINT #2, "Fine"
    CLOSE
    EXIT SUB

Punto:
    IF a > 14 THEN a = 0
    a = a + 1
    Center 14, "AÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´B", 20
    LOCATE 14, 32 + a
    COLOR Col(8, 1), Col(8, 2)
    PRINT CHR$(254)
    COLOR Col(7, 1), Col(7, 2)
RETURN

ASCII2:
    b = 0
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
            FOR x = 1 TO LEN(a$)
                b = b + 1
                b$ = RIGHT$(LEFT$(a$, b), 1)
                PRINT #2, ASC(b$)
            FOR y = 1 TO 300 * VAL(Info$(3)): NEXT
            NEXT
            b = 0
            b$ = ""
            PRINT #2, "013"
        g = g + LEN(a$)
        Grand$ = "Grandezza:" + STR$(g)
        Center 12, Grand$, 18
        GOSUB Punto
    WEND
    PRINT #2, "Fine"
    CLOSE
    EXIT SUB

ASCIIR2:
    b = 0
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
        FOR x = 1 TO LEN(a$)
            b = b + 1
            b$ = RIGHT$(LEFT$(a$, b), 1)
            IF b$ = "," THEN
                    PRINT #2, "ASCII"
                    PRINT #2, ASC(b$)
            ELSEIF b$ = CHR$(32) THEN
                    PRINT #2, "ASCII"
                    PRINT #2, ASC(" ")
            ELSEIF b$ = CHR$(34) THEN
                    PRINT #2, "ASCII"
                    PRINT #2, "34"
            ELSE
                    PRINT #2, b$
            END IF
            'FOR y = 1 TO 90 * VAL(Info$(3)): NEXT
        NEXT
        SLEEP 1
        PRINT #2, "013"
        b$ = "": b = 0
        g = g + LEN(a$)
        Grand$ = "Grandezza:" + STR$(g)
        Center 12, Grand$, 18
        GOSUB Punto
    WEND
    PRINT #2, "Fine"
    CLOSE
    EXIT SUB

RETURN
END SUB

SUB InvioOp
    COLOR Col(7, 1), Col(7, 2)
    Finestra 6, 30, "Opzioni di invio file"
    COLOR Col(7, 1), Col(7, 2)
    F1$ = "1-Directory di arrivo: "
    F2$ = "2-Invio ASCII: " + Info$(11)
    F3$ = "3-Ricezione aut.ASCII: " + Info$(12)
    F4$ = "4-Invio ASCII rapido:" + Info$(13)
    Center 9, F1$, 28
    Center 11, F2$, 28
    Center 12, F3$, 28
    Center 13, F4$, 28
    COLOR Col(1, 1), Col(1, 2)
    Center 10, SPACE$(28), 28
    Center 10, Info$(10), 28
    COLOR Col(7, 1), Col(7, 2)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27)
            MSel(1) = 0
            Esecuzione
        CASE CHR$(49)
            a$ = Info$(10)
            GOSUB Dir
        CASE CHR$(50)
            LOCATE 11, 41: PRINT SPACE$(10)
            LOCATE 11, 41: INPUT Info$(11)
            IF UCASE$(Info$(11)) <> "N" AND UCASE$(Info$(11)) <> "S" THEN RUN
            LOCATE 11, 41: PRINT Info$(11) + "  "
            Info$(11) = UCASE$(Info$(11))
            GOSUB Salva3
        CASE CHR$(51)
            LOCATE 12, 50: PRINT SPACE$(5)
            LOCATE 12, 50: INPUT Info$(12)
            IF UCASE$(Info$(12)) <> "N" AND UCASE$(Info$(12)) <> "S" THEN RUN
            LOCATE 12, 50: PRINT Info$(12) + "  "
            Info$(12) = UCASE$(Info$(12))
            GOSUB Salva3
        CASE CHR$(52)
            LOCATE 13, 48: PRINT SPACE$(5)
            LOCATE 13, 48: INPUT Info$(13)
            IF UCASE$(Info$(13)) <> "N" AND UCASE$(Info$(13)) <> "S" THEN RUN
            LOCATE 13, 48: PRINT Info$(13) + "  "
            Info$(13) = UCASE$(Info$(13))
            GOSUB Salva3
    END SELECT
    InvioOp

Salva3:
    OPEN Info$ FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
        PRINT #1, Info$(a)
    NEXT
    CLOSE #1
    RETURN

Dir:
    ab = LEN(a$)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(13)
            Info$(10) = UCASE$(a$)
            GOSUB Salva3
            RETURN
        CASE CHR$(8)
            IF ab < 1 THEN BEEP: GOTO Dir
            ab = ab - 1
            a$ = LEFT$(a$, LEN(a$) - 1)
        CASE ELSE
            ab = ab + 1
            a$ = a$ + Tasti$(1)
    END SELECT
    COLOR Col(1, 1), Col(1, 2)
    Center 10, SPACE$(28), 28
    Center 10, a$, 28
    COLOR Col(7, 1), Col(7, 2)
    GOTO Dir
END SUB

SUB IScritte (aa)
    PRINT #2, "Scritta"
    PRINT #2, Frase$(aa)
END SUB

SUB Menu (Selezione)
    IF MSel(1) > 0 THEN EXIT SUB
    COLOR Col(2, 1), Col(2, 2)
    FOR x = 1 TO UBOUND(NMenu$)
        a = a + 1
        LOCATE 2, NMenu(a)
        PRINT "<" + NMenu$(a) + ">"
    NEXT
    COLOR Col(7, 1), Col(7, 2)
    LOCATE 2, NMenu(Selezione): PRINT CHR$(17)
    LOCATE 2, NMenu(Selezione) + LEN(NMenu$(Selezione)) + 1: PRINT CHR$(16)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(0) + "H"
            BEEP
            Menu Selezione
        CASE CHR$(0) + "P"
            BEEP
            Menu Selezione
       CASE CHR$(0) + "M"
            IF Selezione < 3 THEN
                Selezione = Selezione + 1
                Menu Selezione
            ELSE
                BEEP
                Menu Selezione
            END IF
        CASE CHR$(0) + "K"
            IF Selezione > 1 THEN
                Selezione = Selezione - 1
                Menu Selezione
            ELSE
                BEEP
                Menu Selezione
             END IF
        CASE CHR$(27): STOP
        CASE CHR$(13)
            COLOR Col(2, 1), Col(2, 2)
            SELECT CASE Selezione
                CASE 1: GOSUB Menu1
                CASE 2: GOSUB Menu2
                CASE 3: GOSUB Menu3
            END SELECT
            GOSUB Selezione
        CASE ELSE: Menu Selezione
    END SELECT
    EXIT SUB

Menu1:
    Finestra 6, 20, NMenu$(Selezione)
    COLOR Col(2, 1), Col(2, 2)
    a = 0
    FOR x = 1 TO UBOUND(TipMenu$, 2)
        a = a + 1
        IF TipMenu$(Selezione, a) <> "" THEN
            Center 8 + a, STR$(a) + "- " + TipMenu$(Selezione, a), 20
        END IF
    NEXT
    MSel = Selezione
    RETURN

Menu2:
    Finestra 6, 26, NMenu$(Selezione)
    COLOR Col(2, 1), Col(2, 2)
    a = 0
    FOR x = 1 TO UBOUND(TipMenu$, 2)
        a = a + 1
        IF TipMenu$(Selezione, a) <> "" THEN
            Center 8 + a, STR$(a) + "- " + TipMenu$(Selezione, a), 24
        END IF
    NEXT
    IF Info$(2) <> "COLORE" THEN LOCATE 13, 48: PRINT "*"
    MSel = Selezione
    RETURN

Menu3:
    Finestra 2, 22, NMenu$(Selezione)
    a = 0
    COLOR Col(2, 1), Col(2, 2)
    FOR x = 1 TO UBOUND(TipMenu$, 2)
        a = a + 1
        IF TipMenu$(Selezione, a) <> "" THEN
            Center 10 + a, STR$(a) + "- " + TipMenu$(Selezione, a), 20
        END IF
    NEXT
    MSel = Selezione
    RETURN

Selezione:
    COLOR Col(2, 1), Col(2, 2)
    a = 0
    FOR x = 1 TO UBOUND(NMenu$)
        a = a + 1
        LOCATE 2, NMenu(a)
        PRINT "<" + NMenu$(a) + ">"
    NEXT
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    IF MSel = 1 AND Tasti$(1) = CHR$(49) THEN MSel(1) = 1
    IF MSel = 1 AND Tasti$(1) = CHR$(50) THEN MSel(1) = 2
    IF MSel = 1 AND Tasti$(1) = CHR$(51) THEN MSel(1) = 3
    IF MSel = 1 AND Tasti$(1) = CHR$(52) THEN MSel(1) = 4
    IF MSel = 2 AND Tasti$(1) = CHR$(49) THEN MSel(1) = 5
    IF MSel = 2 AND Tasti$(1) = CHR$(50) THEN MSel(1) = 6
    IF MSel = 2 AND Tasti$(1) = CHR$(51) THEN MSel(1) = 7
    IF MSel = 2 AND Tasti$(1) = CHR$(52) THEN MSel(1) = 8
    IF MSel = 2 AND Tasti$(1) = CHR$(53) THEN GOSUB Colorazione
    IF MSel = 3 AND Tasti$(1) = CHR$(49) THEN MSel(1) = 9
    COLOR Col(6, 1), Col(6, 2)
    RETURN

Colorazione:
    IF Info$(2) = "COLORE" THEN
        Info$(2) = "BN"
    ELSE
        Info$(2) = "COLORE"
    END IF
    OPEN Info$ FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
        PRINT #1, Info$(a)
    NEXT
    CLOSE #1
    RETURN
END SUB

SUB MenuDef
    NMenu$(1) = "Strumenti"
    NMenu$(2) = "Opzioni"
    NMenu$(3) = "?"
    NMenu(1) = 7
    NMenu(2) = 34
    NMenu(3) = 75
    TipMenu$(1, 1) = "Connetti    F2"
    TipMenu$(1, 2) = "Disconnetti F3"
    TipMenu$(1, 3) = "Invia file  F7"
    TipMenu$(1, 4) = "Esci       ESC"
    TipMenu$(2, 1) = "Utente          "
    TipMenu$(2, 2) = "Testa PC      F4"
    TipMenu$(2, 3) = "Op. di lavoro F5"
    TipMenu$(2, 4) = "Op. di invio file"
    TipMenu$(2, 5) = "Bianco e Nero [ ]"
    TipMenu$(3, 1) = "Informazioni  F1"
END SUB

SUB Opzioni
    COLOR Col(7, 1), Col(7, 2)
    Finestra 6, 30, "Impostazioni di lavoro"
    COLOR Col(7, 1), Col(7, 2)
    F1$ = "1-Velocit… dati: " + Info$(5)
    F2$ = "2-Parit…: " + Info$(6)
    F3$ = "3-Bit di dati: " + Info$(7)
    F4$ = "4-Stop: " + Info$(8)
    F5$ = "5-Numero porta: " + Info$(9)
    Center 9, F1$, 28
    Center 10, F2$, 28
    Center 11, F3$, 28
    Center 12, F4$, 28
    Center 13, F5$, 28
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27): RUN
        CASE CHR$(49)
            LOCATE 9, 44: PRINT SPACE$(10)
            LOCATE 9, 44: INPUT Info$(5)
            IF Info$(5) <> "75" AND Info$(5) <> "110" AND Info$(5) <> "150" AND Info$(5) <> "300" AND Info$(5) <> "600" AND Info$(5) <> "1200" AND Info$(5) <> "2400" AND Info$(5) <> "4800" AND Info$(5) <> "9600" THEN
                BEEP
                RUN
            END IF
            LOCATE 9, 44: PRINT Info$(5) + "  "
            GOSUB Salva2
        CASE CHR$(50)
            LOCATE 10, 37: PRINT SPACE$(8)
            LOCATE 10, 37: INPUT Info$(6)
            IF Info$(6) <> "N" AND Info$(6) <> "S" AND Info$(6) <> "E" AND Info$(6) <> "M" AND Info$(6) <> "O" THEN
                BEEP
                RUN
            END IF
            LOCATE 10, 37: PRINT Info$(7) + " sec.  "
            GOSUB Salva2
        CASE CHR$(51)
            LOCATE 11, 42: PRINT SPACE$(8)
            LOCATE 11, 42: INPUT Info$(7)
            IF Info$(7) <> "5" AND Info$(7) <> "6" AND Info$(7) <> "7" AND Info$(7) <> "8" THEN
                BEEP
                RUN
            END IF
            LOCATE 11, 42: PRINT Info$(7) + " sec.  "
            GOSUB Salva2
        CASE CHR$(52)
            LOCATE 12, 35: PRINT SPACE$(8)
            LOCATE 12, 35: INPUT Info$(8)
            IF Info$(8) <> "1" AND Info$(8) <> "1.5" AND Info$(8) <> "2" THEN
                BEEP
                RUN
            END IF
            LOCATE 12, 35: PRINT Info$(8) + " sec.  "
            GOSUB Salva2
        CASE CHR$(53)
            LOCATE 13, 43: PRINT SPACE$(8)
            LOCATE 13, 43: INPUT Info$(9)
            LOCATE 13, 43: PRINT Info$(9) + " sec.  "
            GOSUB Salva2
    END SELECT
    Opzioni

Salva2:
    OPEN Info$ FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
        IF Info$(a) <> "" THEN PRINT #1, Info$(a)
    NEXT
    CLOSE #1
    RETURN
END SUB

SUB Pagina1
    IF Conn(1) > 0 THEN COLOR Col(6, 1), Col(6, 2)
    LOCATE 4, 1
    PRINT "É" + STRING$(39, "Í") + "»"
    FOR x = 1 TO 18
        a = a + 1
        LOCATE 4 + a
        PRINT "º" + STRING$(39, " ") + "º"
    NEXT
    LOCATE 4 + a
    PRINT "È" + STRING$(39, "Í") + "¼"
    LOCATE 4, 4: PRINT " " + Info$(1) + " "
END SUB

SUB Pagina2
    IF Conn(1) > 0 THEN
        COLOR Col(6, 1), Col(6, 2)
        a$ = "»": b$ = "¼": c$ = "º": d$ = "Í"
    ELSE
        a$ = "¿": b$ = "Ù": c$ = "³": d$ = "Ä"
    END IF
    LOCATE 4, 42
    PRINT STRING$(38, d$) + a$
    FOR x = 1 TO 18
        a = a + 1
        LOCATE 4 + a, 42
        PRINT STRING$(38, " ") + c$
    NEXT
    LOCATE 4 + a, 42
    PRINT STRING$(38, d$) + b$
    IF Conn(1) > 0 THEN LOCATE 4, 45: PRINT " " + USoft$(1) + " "
END SUB

SUB PInfor
    OPEN Info$ FOR INPUT AS #1
    WHILE EOF(1) = 0
        a = a + 1
        LINE INPUT #1, Info$(a)
    WEND
    CLOSE
END SUB

SUB Preparativi
COLOR Col(2, 1), Col(2, 2)
LOCATE 2: PRINT SPACE$(80)
FOR x = 1 TO UBOUND(NMenu$)
    a = a + 1
    LOCATE 2, NMenu(a)
    PRINT "<" + NMenu$(a) + ">"
NEXT
COLOR Col(4, 1), Col(4, 2)
LOCATE 3, 9: PRINT "Connessione:"
LOCATE 3, 31: PRINT "Stato: "
LOCATE 3, 56: PRINT "Velocit…:"
IF Conn(1) = 0 THEN
    COLOR Col(3, 1), Col(3, 2)
    LOCATE 3, 37: PRINT "Disconnesso        "
    LOCATE 3, 65: PRINT Info$(5) + " "
ELSE
    COLOR Col(6, 1), Col(6, 2)
    LOCATE 3, 22: PRINT NSoft$(1) + "  "
    LOCATE 3, 37: PRINT "Connesso         "
    LOCATE 3, 65: PRINT Info$(5) + "  "
END IF
END SUB

SUB Risposta
    SLEEP 1
    IF Conn(1) = 2 AND c(1) = 0 THEN Scritte
    IF Conn(1) = 1 THEN
        IF c(1) = 1 THEN Conn(1) = 2
        EXIT SUB
    END IF
    IF Conn(1) = 2 THEN
        IF c(1) = 1 THEN c(1) = 0
        EXIT SUB
    END IF
    INPUT #2, a$
    IF a$ = Soft$ THEN
        SLEEP 1
        PRINT #2, Soft$
        SLEEP 1
        PRINT #2, Info$(1)
        SLEEP 2
        PRINT #2, Info$(5)
        Conn(1) = Conn(1) + 1
        INPUT #2, VSoft$(1)
        INPUT #2, USoft$(1)
        INPUT #2, NSoft$(1)
        IF VAL(VSoft$(1)) < VAL(Info$(5)) THEN Info$(5) = VSoft$(1)
        Tasti$(1) = CHR$(0)
    END IF
    IF a$ = "File" THEN
        INPUT #2, File$
        CLOSE
        OPEN "COM1:9600,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #2
        PRINT "*" + File$ + "*"
        OPEN File$ FOR OUTPUT AS #1
        WHILE EOF(2) = 0
            INPUT #2, a$
            PRINT #1, a$
        WEND
        CLOSE
    END IF
END SUB

SUB Scritte
    INPUT #2, a$
    IF a$ = "File" THEN
        INPUT #2, File$
        COLOR Col(7, 1), Col(7, 2)
        Finestra 4, 30, "Invio file"
        COLOR Col(7, 1), Col(7, 2)
        Center 10, "Ti stanno inviando il file", 28
        Center 11, File$, 10
        CLOSE
        OPEN "COM1:9600,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #2
        OPEN Info$(10) + "\" + File$ FOR OUTPUT AS #1
        IF Info$(12) = "S" THEN
            INPUT #2, ASCII$
            IF ASCII$ = "ASCII" THEN GOSUB ASCII
            IF ASCII$ = "ASCIIR" THEN GOSUB ASCIIR
        ELSE
            INPUT #2, a$
            IF a$ = "ASCII" OR ASCII$ = "ASCIIR" THEN GOSUB Impossibile
        END IF
        WHILE a$ <> "Fine"
            INPUT #2, a$
            g = g + LEN(a$)
            F1$ = "Grandezza:" + STR$(g)
            IF a$ <> "Fine" THEN PRINT #1, a$
            Center 12, F1$, 16
        WEND
        CLOSE
        SLEEP 5
        c(1) = 1
        RUN
    ELSE
        IF a$ = "Disconnetti" THEN
            COLOR Col(7, 1), Col(7, 2)
            Finestra 4, 20, "Avviso"
            COLOR Col(7, 1), Col(7, 2)
            Center 10, "Richiesta di ", 12
            Center 11, "disconnessione", 14
            Center 12, "accettata", 8
            SLEEP 5
            RUN
        ELSE
            Conn(1) = 0
            c(1) = 1
            b(1) = b(1) + 1
            INPUT #2, Frase$(100 + b(1))
            IF b(1) > 17 THEN b(1) = 1
            LOCATE 4 + b(1), 44
            PRINT SPACE$(36)
            LOCATE 4 + b(1), 44
            PRINT Frase$(100 + b(1))
        END IF
    END IF
    Conn(1) = 1
    EXIT SUB

Impossibile:
    COLOR Col(7, 1), Col(7, 2)
    Finestra 4, 30, "Opzioni di invio file"
    COLOR Col(7, 1), Col(7, 2)
    Center 10, "Impossibile rivcevere file", 28
    Center 11, "in formato ASCII", 28
    Center 12, "Modalit… disattivata", 28
    WHILE a$ <> "Fine"
        INPUT #2, a$
    WEND
    RETURN

ASCII:
    WHILE a$ <> "Fine"
        INPUT #2, a$
        IF a$ <> "Fine" THEN
            b$ = b$ + CHR$(VAL(a$))
            IF a$ = "013" THEN
                PRINT #1, b$
                g = g + LEN(b$)
                F1$ = "Grandezza:" + STR$(g)
                b$ = ""
            END IF
        END IF
        Center 12, F1$, 16
    WEND
    CLOSE
    SLEEP 5
    c(1) = 1
    Conn(1) = 1
    RUN

ASCIIR:
    WHILE a$ <> "Fine"
        INPUT #2, a$
        IF a$ <> "Fine" THEN
            IF a$ <> "ASCII" THEN
                IF a$ = "013" THEN
                    PRINT #1, b$
                    b$ = ""
                ELSE
                    b$ = b$ + a$
                END IF
            ELSE
                INPUT #2, a$
                b$ = b$ + CHR$(VAL(a$))
            END IF
        END IF
        g = g + LEN(a$)
        F1$ = "Grandezza:" + STR$(g)
        Center 12, F1$, 16
    WEND
    CLOSE
    SLEEP 5
    c(1) = 1
    Conn(1) = 1
    RUN
END SUB

SUB Sinistra (Alt, Text$, DisB)
    LOCATE Alt, DisB
    PRINT SPACE$(37)
    LOCATE Alt, DisB
    PRINT Text$ + " "
END SUB

SUB Tempo
    DIM a(10)
    COLOR Col(7, 1), Col(7, 2)
    Finestra 6, 30, "Testa il PC"
    COLOR Col(7, 1), Col(7, 2)
    Center 9, "Testa la velocit… del PC", 26
    Center 10, "Il Test pu• durare molto,", 26
    Center 11, "a seconda della velocit…", 26
    Center 12, "del computer", 26
    Center 13, "Premi un tasto per iniziare", 26
    DO: LOOP WHILE INKEY$ = ""
    Finestra 6, 30, "Testa il PC"
    COLOR Col(7, 1), Col(7, 2)
    Center 10, "Test in corso", 20
    Center 11, "Attendere", 20
    aa = TIMER
    FOR x = 1 TO UBOUND(a)
        a = a + 1
        b = TIMER
        FOR y = 1 TO 10000: NEXT
        c = TIMER
        a(a) = 1 / (c - b)
    NEXT
    ab = TIMER: a = 0

    FOR x = 1 TO UBOUND(a)
        a = a + 1
        t = t + a(a)
    NEXT

    TM = t / UBOUND(a)

    BA = TIMER
        FOR x = 1 TO 100000 * TM: NEXT
    BB = TIMER
    TT = BB - BA
    Info$(3) = STR$(TM)
    Sc = (10 - TT) * (100 / 10)
    TTRas = ab - aa
    SELECT CASE Sc
        CASE IS = 0: App$ = " Ottima"
        CASE IS > 0 AND Sc < 1: App$ = " Buona"
        CASE IS < 0 AND Sc > -1: App$ = " Buona"
        CASE IS > 1 AND Sc < 2: App$ = " Sufficiente"
        CASE IS < -1 AND Sc > -2: App$ = " Sufficiente"
        CASE ELSE: App$ = " Scarsa"
    END SELECT

    F1$ = "Tempo trascorso=" + STR$(CINT(TTRas)) + "s"
    F2$ = "Tempo medio=" + STR$(TM) + "s"
    F3$ = "Tempo di prova=" + STR$(TT) + "s"
    F4$ = "Scarto=" + STR$(Sc) + "%"
    F5$ = "Approssimazione=" + App$
    COLOR Col(7, 1), Col(7, 2)
    Center 9, F1$, 26
    Center 10, F2$, 26
    Center 11, F3$, 26
    Center 12, F4$, 26
    Center 13, F5$, 26
    CLOSE
    OPEN Info$ FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
        PRINT #1, Info$(a)
    NEXT
    CLOSE #1
    SLEEP 10
    RUN
END SUB

SUB Titolo
    COLOR Col(1, 1), Col(1, 2)
    LOCATE 1
    PRINT SPACE$(80)
    Tit$ = " L i n k   b y   S e r i a l   C o n n e c t o r   2 . 0 "
    LOCATE 1, 40 - (LEN(Tit$)) / 2
    PRINT Tit$
    LOCATE 23
    PRINT SPACE$(80)
    LOCATE 23, 5
    PRINT "F10=Men—"
END SUB

SUB Utente
    IF Conn(1) > 0 THEN
        COLOR Col(6, 1), Col(6, 2)
    ELSE
        COLOR Col(3, 1), Col(3, 2)
    END IF
    FOR x = 1 TO 8
        a = a + 1
        Center 7 + a, "               º               ", 30
    NEXT
    a = 0: a$ = ""
    COLOR Col(7, 1), Col(7, 2)
    Finestra 4, 30, "Opzioni utente"
    COLOR Col(7, 1), Col(7, 2)
    F1$ = "1-Nome: " + Info$(1)
    F2$ = "#-Nome computer: " + Soft$
    F3$ = "2-Tempo di chiamata:" + Info$(4) + " sec."
    Center 10, F1$, 28
    Center 11, F2$, 28
    Center 12, F3$, 28
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27): RUN
        CASE CHR$(49)
            LOCATE 10, 35: PRINT SPACE$(20)
            LOCATE 10, 35: INPUT Info$(1)
            LOCATE 10, 35: PRINT Info$(1) + "  "
            GOSUB Salva
        CASE CHR$(50)
            LOCATE 12, 47: PRINT SPACE$(8)
            LOCATE 12, 47: INPUT Info$(4)
            LOCATE 12, 47: PRINT Info$(4) + " sec.  "
            GOSUB Salva
    END SELECT
    Utente

Salva:
    OPEN Info$ FOR OUTPUT AS #1
    a = 0
    FOR x = 1 TO UBOUND(Info$)
        a = a + 1
        IF Info$(a) <> "" THEN PRINT #1, Info$(a)
    NEXT
    CLOSE #1
    RETURN
END SUB

