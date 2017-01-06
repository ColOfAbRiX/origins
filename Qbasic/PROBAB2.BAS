DECLARE SUB ImpostaGrafico ()
DECLARE SUB Grafico ()
DECLARE SUB ImpostaPass ()
DECLARE SUB Opzioni ()
DECLARE SUB Password ()
DECLARE SUB Calcolatrice ()
DECLARE SUB Esci ()
DECLARE SUB InserisciValori ()
DECLARE SUB Calcola ()
DECLARE SUB Colori ()
DECLARE SUB Lavora ()
DECLARE SUB Apri ()
DECLARE SUB Elimina ()
DECLARE SUB Salva ()
DECLARE SUB Stampa ()
DECLARE SUB Nuovo ()
DECLARE SUB Menu ()
REM Probabilit… di interrogazioni
RESET
CLEAR , , 16836
ON ERROR GOTO Errori
ON TIMER(60) GOSUB Tempo
    SCREEN 0
    TIMER ON
    COLOR 7, 1
    CLS
    LOCATE 1
    COLOR 4, 7
    PRINT "   File        Lavora                                           ?               "
    LOCATE 23
    PRINT "     M=men—  ESC=Esci  INVIO=Ok                                                 "
    
    OPEN "Pass.pbb" FOR INPUT AS #1
    INPUT #1, sn$
    CLOSE #1
    IF sn$ <> "N" THEN CALL Password
    
    Tasti$ = ""
    WHILE Tasti$ <> CHR$(77) AND Tasti$ <> CHR$(27)
        Tasti$ = INKEY$
        IF Tasti$ = CHR$(109) THEN
            COLOR 4, 7
            LOCATE 10, 28
            PRINT " Usa lettere  maiuscole "
        END IF
    WEND
    IF Tasti$ = CHR$(27) THEN END
    CLEAR , , 16836
    CALL Menu

Tempo:
    COLOR 4, 7
    LOCATE 23, 80 - LEN(TIME$) - 1
    PRINT LEFT$(TIME$, 5)
    COLOR 0, 7
    RETURN
Errori:
    SELECT CASE ERR
        CASE 9: TError$ = "Indice inferiore fuori limite"
        CASE 11: TError$ = "Divisione per zero"
        CASE 13: TError$ = "Tipo di dati non corrispondente"
        CASE 24: TError$ = "Timeout di periferica"
        CASE 25: TError$ = "Errore di periferica"
        CASE 27: TError$ = "Carta esaurita"
        CASE 53: TError$ = "File non trovato"
        CASE 55: TError$ = "File gia aperto"
        CASE 61: TError$ = "Disco pieno"
        CASE 68: TError$ = "Periferica non disponibile"
        CASE 69: TError$ = "Overflow del buffer comunicazioni"
        CASE 71: TError$ = "Disco non pronto"
        CASE 72: TError$ = "Errore nel supporto del disco"
    END SELECT

IF TError$ = "" THEN TError$ = "Errore con nome non rilevato"
Larg = LEN(TError$)
IF Larg < 34 THEN Larg = 34
Num = 8
DisH = (22 / 2) - (Num / 2)
DisB = (80 / 2) - (Larg / 2)
DisHs = DisH + 1
DisBs = DisB + 1
COLOR 14, 4
LOCATE DisH, DisB
PRINT "É" + STRING$(Larg, "Í") + "»"
FOR x = 1 TO Num
    FOR y = 1 TO 100
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "º" + STRING$(Larg, " ") + "º"
NEXT
LOCATE DisH + a, DisB
PRINT "È" + STRING$(Larg, "Í") + "¼"
COLOR 15, 4
LOCATE DisHs, DisBs
PRINT " Si Š verificato l'errore numero: "
LOCATE DisHs + 2, 38
PRINT ERR
a = 40 - (LEN(TError$) / 2)
LOCATE DisHs + 3, a
PRINT TError$
LOCATE DisHs + 5, DisBs
PRINT " Premere Invio per terminare il"
LOCATE DisHs + 6, DisBs
PRINT " programma."
WHILE Tasti$ <> CHR$(13)
    Tasti$ = INKEY$
WEND
STOP
RESUME
'tasti$ Š la variabile a cui Š stato assegnato il valore di INKEY$'
'Errori Gestisce il controllo degli errori che si potrebbero verificare

'DimL Š la lunghezza della finsestra di lavoro
'DimH Š l'altezza della finestra di lavoro
'DisB Š la distanza dal bordo della finestra
'DisH Š la distanza dall'alto della finestra di lavoro
SUB Apri
  
    OPEN "lista.pbb" FOR INPUT AS #1
    INPUT #1, Tot$
    CLOSE #1
    IF VAL(Tot$) = 0 THEN EXIT SUB
    Tot = VAL(Tot$)

    DimL = 16
    DimH = Tot + 3
    DisB = 32
    DisH = 5
    DIM File$(10)
    COLOR 7, 1
    LOCATE 2
    FOR x = 1 TO 20
        PRINT STRING$(80, " ")
    NEXT
    COLOR 0, 7
    a = DisH
    LOCATE a, DisB: PRINT "Ú" + STRING$(DimL, "Ä") + "¿"
    FOR x = 1 TO DimH
        a = a + 1
        LOCATE a, DisB: PRINT "³" + STRING$(DimL, " ") + "³"
    NEXT
    LOCATE a, DisB: PRINT "À" + STRING$(DimL, "Ä") + "Ù"
    LOCATE DisH + Tot + 2, DisB + 3
    PRINT "Ok"
    LOCATE DisH + Tot + 2, DisB + 8
    PRINT "Annulla"
    FileMax = 0
    FileMax = VAL(Tot$)
    a = DisH
    b = -1
    FOR x = 1 TO FileMax
        a = a + 1
        b = b + 1
        LOCATE a, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(b)) + ".pbb"
    NEXT
    LOCATE DisH + 1, DisB + 3
    COLOR 0, 2
    PRINT "Lavoro0.pbb"
    COLOR 0, 7

    GOSUB Immissione2
Selezione2:

    SELECT CASE Tasti$
        CASE CHR$(0) + "K": GOSUB MenuSinistro2
        CASE CHR$(0) + "M": GOSUB MenuDestro2
        CASE CHR$(0) + "P": GOSUB MenuGiu2
        CASE CHR$(0) + "H": GOSUB MenuSu2
        CASE CHR$(27): END
        CASE CHR$(13): GOSUB Invio2
        CASE IS <> "A", IS <> "D", IS <> "S", IS <> "W", IS <> "M", IS <> CHR$(13), CHR$(27): GOSUB Immissione2
        CASE ELSE
            BEEP
            GOSUB Immissione2
        END SELECT
    GOSUB Immissione2

MenuSinistro2:
    IF Selezione = 1 THEN
        BEEP
        GOSUB Immissione2
    END IF
    IF Selezione = 2 THEN
        Selezione = 1
        COLOR 0, 7
        LOCATE DisH + Linea + 4, DisB + 8
        PRINT "Annulla"
        COLOR 0, 2
        LOCATE DisH + Linea + 4, DisB + 3
        PRINT "Ok"
    END IF
    GOSUB Immissione2
MenuDestro2:
    IF Selezione = 2 THEN
        BEEP
        GOSUB Immissione2
    END IF
    IF Selezione = 1 THEN
        Selezione = 2
        COLOR 0, 7
        LOCATE DisH + Linea + 4, DisB + 3
        PRINT "Ok"
        COLOR 0, 2
        LOCATE DisH + Linea + 4, DisB + 8
        PRINT "Annulla"
    END IF
    GOSUB Immissione2
MenuSu2:
    IF Selezione <> 0 THEN Linea = FileMax - 1
    IF Linea <= 0 THEN
        BEEP
        GOSUB Immissione2
    END IF
    IF Selezione = 0 THEN
        COLOR 0, 7
        LOCATE DisH + Linea + 1, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
        COLOR 0, 7
    END IF
    IF Linea > -2 THEN
        Linea = Linea - 1
        Selezione = 0
        LOCATE DisH + Linea + 1, DisB + 3
        COLOR 0, 2
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
    END IF
    IF Linea >= FileMax - 2 THEN
        LOCATE DisH + Linea + 2, DisB + 3
        COLOR 0, 7
        PRINT "Lavoro" + LTRIM$(STR$(Linea + 1)) + ".pbb"
    END IF
GOSUB Immissione2
MenuGiu2:
    IF Linea > FileMax - 2 THEN
        IF Linea >= FileMax - 2 THEN
            Linea = Linea - 1
            Selezione = 0
            COLOR 0, 2
            LOCATE DisH + Linea + 4, DisB + 3
            PRINT "Ok"
            Selezione = 1
        ELSE
        BEEP
        GOSUB Immissione2
        END IF
    END IF
    IF Linea < FileMax + 1 AND Selezione = 0 THEN
        Linea = Linea + 1
        Selezione = 0
        COLOR 0, 2
        LOCATE DisH + Linea + 1, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
        COLOR 0, 7
        LOCATE DisH + Linea, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea - 1)) + ".pbb"
    END IF
    GOSUB Immissione2
Invio2:
    IF Selezione <> 1 AND Selezione <> 2 THEN
        a = -1
        FOR x = 1 TO FileMax
            a = a + 1
            LOCATE DisH + Linea + 1 + a, DisB + 1
            PRINT "  "
           NEXT
        LOCATE DisH + Linea + 1, DisB + 2
        LineaSelezione = Linea
        PRINT CHR$(175)
    END IF
    IF Selezione = 1 THEN
        File$ = "Data" + LTRIM$(STR$(LineaSelezione)) + ".pbb"
        OPEN "Trans" FOR OUTPUT AS #1
        PRINT #1, 1, File$
        CLOSE #1
        CALL Calcola
    END IF
    IF Selezione = 2 THEN RUN
Immissione2:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    GOSUB Selezione2

END SUB

'InterrNome Š il numero di volte che l'alunno Š stato interrogato
'TotAlunniInterr Š il numero degli alunni interrogati
'Probab Š la probabilit… di essere interrogato
'ProbabMeno Š la probabilit… di non essere interrogato
'TotAlunni Š il numero totale degli alunni interrogati
'Trans serve per trasferire i dati da una Sub ad un altra, poiche le varibili
'si azzerano al cambio dei una SUB
SUB Calcola
    OPEN "Trans" FOR INPUT AS #1
    INPUT #1, Apertura, File$
    CLOSE #1
    IF Apertura = 1 THEN
        OPEN File$ FOR INPUT AS #1
        INPUT #1, a, b, c, b$
        CLOSE
        TotAlunni = a
        TotAlunniInterr = b
        InterrNome = c
        Analiz = 1
    END IF
    DIM Probab(20)
    DIM ProbabMeno(20)
    FOR x = 1 TO Analiz
        Lista = Lista + 1
        IF InterrNome = 0 THEN InterrNome = -1
        IF TotAlunniInterr = 0 THEN TotAlunniInterr = 1
        IF TotAlunni = 0 THEN TotAlunni = 1
        i = CINT(InterrNome * (100 / TotAlunni))
        IF TotAlunniInterr = TotAlunni THEN
            Probab(Lista) = ((CINT(100 / TotAlunni)) - InterrNome) + 1
            ProbabMeno(Lista) = 100 - Probab(Lista)
        END IF
        T = CINT(TotAlunniInterr * (100 / TotAlunni))
        Probab(Lista) = (T - i)
        ProbabMeno(Lista) = 100 - Probab(Lista)
        IF Probab(Lista) > 100 THEN Probab(Lista) = 100
        IF Probab(Lista) = 0 THEN
            Probab(Lista) = 1
            ProbabMeno(Lista) = 100 - Probab(Lista)
        END IF
        Probab = Probab(Lista)
        IF Probab(Lista) < 0 THEN Probab(Lista) = 1
        OPEN "Trans" FOR INPUT AS #1
        INPUT #1, a, File$
        CLOSE #1
        OPEN "Trans" FOR OUTPUT AS #1
        PRINT #1, File$
        PRINT #1, Probab
        CLOSE #1
    NEXT
    CALL Lavora
END SUB

SUB Calcolatrice
'$STATIC
DIM Segn$(10)
DIM NumeroA$(18)
'$DYNAMIC
DIM Numero$(18, 15)
LOCATE 2
FOR x = 1 TO 6
 COLOR 7, 1
 PRINT STRING$(70, " ")
NEXT
Larg = 18
Alt = 8
Schermo = 1
DisH = (24 / 2) - (Alt / 2)
DisB = (80 / 2) - Larg
COLOR 4, 7
LOCATE DisH + a - 2, DisB + a
PRINT "Ú" + STRING$(Larg, "Ä") + "Ä" + STRING$(Larg, "Ä") + "¿"
FOR x = 1 TO Schermo
    a = a + 1
    LOCATE DisH + a - 2, DisB + a - 1
    PRINT "³" + STRING$(Larg * 2 + 1, " ") + "³"
NEXT
a = 0
LOCATE DisH + a, DisB + a
PRINT "Ã" + STRING$(Larg, "Ä") + "Â" + STRING$(Larg, "Ä") + "´"
FOR x = 1 TO Alt
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "³" + STRING$(Larg, " ") + "³" + STRING$(Larg, " ") + "³"
NEXT
LOCATE DisH + a, DisB
PRINT "À" + STRING$(Larg, "Ä") + "Á" + STRING$(Larg, "Ä") + "Ù"
COLOR 7, 4
a = -2
b = 1
FOR x = 1 TO 11
    FOR y = 1 TO 100
    NEXT
    a = a + 4
    c = c + 1
    LOCATE DisH + b, DisB + a
    IF c = 10 THEN PRINT 0
    IF c < 10 THEN PRINT c
    IF c MOD 3 = 0 THEN
        b = b + 2
        a = -2
    END IF
    IF c > 10 THEN
        LOCATE DisH + b, DisB + a
        PRINT " ý "
        LOCATE DisH + b, DisB + a + 4
        PRINT " û "
    END IF
NEXT
a = -1
b = 0
Segn$(1) = " + ": Segn$(2) = " - "
Segn$(3) = " / ": Segn$(4) = " * "
FOR x = 1 TO 4
    a = a + 2
    b = b + 1
    LOCATE DisH + a, DisB + 14
    PRINT Segn$(b)
NEXT
a = 0
b = 1
c = 0
REDIM Segn$(4)
Tasti2:
    LOCATE 20
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    T$ = Tasti$
    IF b >= Alt - 1 THEN b = 1
    IF T$ = CHR$(27) THEN RUN
    IF T$ <> "0" AND T$ <> "1" AND T$ <> "2" AND T$ <> "3" AND T$ <> "4" AND T$ <> "5" AND T$ <> "6" AND T$ <> "7" AND T$ <> "8" AND T$ <> "9" AND T$ <> "0" AND T$ <> "+" AND T$ <> "-" AND T$ <> "*" AND T$ <> "/" AND T$ <> CHR$(13) AND UCASE$(T$) <> "R" AND UCASE$(T$) <> "Q" THEN
        BEEP
        GOSUB Tasti2
    END IF
    IF Tasti$ = CHR$(13) THEN
        LOCATE DisH + b, DisB + (Larg * 2)
        PRINT "="
        IF Segn$ = "+" THEN
            Risul = VAL(Numero$(a, b - 1)) + VAL(Numero$(a, b))
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF Segn$ = "-" THEN
            Risul = VAL(Numero$(a, b - 1)) - VAL(Numero$(a, b))
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF Segn$ = "*" THEN
            Risul = VAL(Numero$(a, b - 1)) * VAL(Numero$(a, b))
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF Segn$ = "/" THEN
            Risul = VAL(Numero$(a, b - 1)) / VAL(Numero$(a, b))
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF Segn$ <> "+" AND Segn$ <> "-" AND Segn$ <> "*" AND Segn$ <> "/" AND UCASE$(Segn$) <> "R" AND UCASE$(Segn$) <> "Q" THEN
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF UCASE$(Segn$) = "R" THEN
            PRINT Numero$(a, b)
            REM Risul = SQR(VAL(Numero$(a, b)))
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        IF UCASE$(Segn$) = "Q" THEN
            PRINT Numero$(a, b)
            LOCATE DisH + b + 1, DisB + (Larg * 2) - LEN(STR$(Risul))
            b = b + 2
            PRINT Risul
        END IF
        a = 0
        GOSUB Tasti2
    END IF
    IF Tasti$ = "+" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a
        PRINT Numero$(a, b) + "+"
        b = b + 1
        a = 0
        Segn$ = Tasti$
        GOSUB Tasti2
    END IF
    IF Tasti$ = "-" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a
        PRINT Numero$(a, b) + "-"
        b = b + 1
        a = 0
        Segn$ = Tasti$
        GOSUB Tasti2
    END IF
    IF Tasti$ = "*" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a
        PRINT Numero$(a, b) + "x"
        b = b + 1
        a = 0
        Segn$ = Tasti$
        GOSUB Tasti2
    END IF
    IF Tasti$ = "/" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a
        PRINT Numero$(a, b) + ":"
        b = b + 1
        a = 0
        Segn$ = Tasti$
        GOSUB Tasti2
    END IF
    IF UCASE$(Tasti$) = "R" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a - 1
        PRINT "û" + Numero$(a, b)
        Risul = SQR(VAL(Numero$(a, b)))
        a = 0
        Segn$ = UCASE$(Tasti$)
        GOSUB Tasti2
    END IF
    IF UCASE$(Tasti$) = "Q" THEN
        LOCATE DisH + b, DisB + (Larg * 2) - a - 1
        PRINT Numero$(a, b) + "ý"
        Risul = VAL(Numero$(a, b)) * VAL(Numero$(a, b))
        a = 0
        Segn$ = UCASE$(Tasti$)
        GOSUB Tasti2
    END IF
    a = a + 1
    NumeroA$(a) = Tasti$
    TotNumeri = UBOUND(NumeroA$)
    FOR x = 1 TO TotNumeri
        c = a - 1
        Numero$(a, b) = Numero$(c, b) + Tasti$
    NEXT
    Risul = VAL(Numero$(a, b))
    COLOR 4, 7
    LOCATE DisH + b, DisB + (Larg * 2) - a
    PRINT Numero$(a, b)
    LOCATE DisH - Schermo, DisB + 1
    PRINT STRING$(Larg * 2 - 2, " ")
    LOCATE DisH - Schermo, DisB + 1
    PRINT Numero$(a, b)
    GOSUB Tasti2
END SUB

REM $STATIC
SUB Colori
END SUB

'DimL Š la lunghezza della finsestra di lavoro
'DimH Š l'altezza della finestra di lavoro
'DisB Š la distanza dal bordo della finestra
'DisH Š la distanza dall'alto della finestra di lavoro
SUB Elimina
 
    OPEN "lista.pbb" FOR INPUT AS #1
    INPUT #1, Tot$
    CLOSE #1
    IF VAL(Tot$) = 0 THEN EXIT SUB
    Tot = VAL(Tot$)

    DimL = 16
    DimH = Tot + 3
    DisB = 32
    DisH = 5
    DIM File$(10)
    COLOR 7, 1
    LOCATE 2
    FOR x = 1 TO 20
        PRINT STRING$(80, " ")
    NEXT
    COLOR 0, 7
    a = DisH
    LOCATE a, DisB: PRINT "Ú" + STRING$(DimL, "Ä") + "¿"
    FOR x = 1 TO DimH
        a = a + 1
        LOCATE a, DisB: PRINT "³" + STRING$(DimL, " ") + "³"
    NEXT
    LOCATE a, DisB: PRINT "À" + STRING$(DimL, "Ä") + "Ù"
    LOCATE DisH + Tot + 2, DisB + 3
    PRINT "Ok"
    LOCATE DisH + Tot + 2, DisB + 8
    PRINT "Annulla"
    FileMax = 0
    FileMax = VAL(Tot$)
    a = DisH
    b = -1
    FOR x = 1 TO FileMax
        a = a + 1
        b = b + 1
        LOCATE a, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(b)) + ".pbb"
    NEXT
    LOCATE DisH + 1, DisB + 3
    COLOR 0, 2
    PRINT "Lavoro0.pbb"
    COLOR 0, 7

    GOSUB Immissione3
Selezione3:

    SELECT CASE Tasti$
        CASE CHR$(0) + "K": GOSUB MenuSinistro3
        CASE CHR$(0) + "M": GOSUB MenuDestro3
        CASE CHR$(0) + "P": GOSUB MenuGiu3
        CASE CHR$(0) + "H": GOSUB MenuSu3
        CASE CHR$(27): END
        CASE CHR$(13): GOSUB Invio3
        CASE IS <> "A", IS <> "D", IS <> "S", IS <> "W", IS <> "M", IS <> CHR$(13), CHR$(27): GOSUB Immissione3
        CASE ELSE
            BEEP
            GOSUB Immissione3
        END SELECT
    GOSUB Immissione3

MenuSinistro3:
    IF Selezione = 1 THEN
        BEEP
        GOSUB Immissione3
    END IF
    IF Selezione = 2 THEN
        Selezione = 1
        COLOR 0, 7
        LOCATE DisH + Linea + 4, DisB + 8
        PRINT "Annulla"
        COLOR 0, 2
        LOCATE DisH + Linea + 4, DisB + 3
        PRINT "Ok"
    END IF
    GOSUB Immissione3
MenuDestro3:
    IF Selezione = 2 THEN
        BEEP
        GOSUB Immissione3
    END IF
    IF Selezione = 1 THEN
        Selezione = 2
        COLOR 0, 7
        LOCATE DisH + Linea + 4, DisB + 3
        PRINT "Ok"
        COLOR 0, 2
        LOCATE DisH + Linea + 4, DisB + 8
        PRINT "Annulla"
    END IF
    GOSUB Immissione3
MenuSu3:
    IF Selezione <> 0 THEN Linea = FileMax - 1
    IF Linea <= 0 THEN
        BEEP
        GOSUB Immissione3
    END IF
    IF Selezione = 0 THEN
        COLOR 0, 7
        LOCATE DisH + Linea + 1, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
        COLOR 0, 7
    END IF
    IF Linea > -2 THEN
        Linea = Linea - 1
        Selezione = 0
        LOCATE DisH + Linea + 1, DisB + 3
        COLOR 0, 2
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
    END IF
    IF Linea >= FileMax - 2 THEN
        LOCATE DisH + Linea + 2, DisB + 3
        COLOR 0, 7
        PRINT "Lavoro" + LTRIM$(STR$(Linea + 1)) + ".pbb"
    END IF
GOSUB Immissione3
MenuGiu3:
    IF Linea > FileMax - 2 THEN
        IF Linea >= FileMax - 2 THEN
            Linea = Linea - 1
            Selezione = 0
            COLOR 0, 2
            LOCATE DisH + Linea + 4, DisB + 3
            PRINT "Ok"
            Selezione = 1
        ELSE
        BEEP
        GOSUB Immissione3
        END IF
    END IF
    IF Linea < FileMax + 1 AND Selezione = 0 THEN
        Linea = Linea + 1
        Selezione = 0
        COLOR 0, 2
        LOCATE DisH + Linea + 1, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea)) + ".pbb"
        COLOR 0, 7
        LOCATE DisH + Linea, DisB + 3
        PRINT "Lavoro" + LTRIM$(STR$(Linea - 1)) + ".pbb"
    END IF
    GOSUB Immissione3
Invio3:
    IF Selezione <> 1 AND Selezione <> 2 THEN
        a = -1
        FOR x = 1 TO FileMax
            a = a + 1
            LOCATE DisH + Linea + 1 + a, DisB + 1
            PRINT "  "
           NEXT
        LOCATE DisH + Linea + 1, DisB + 2
        LineaSelezione = Linea
        PRINT CHR$(175)
    END IF
    IF Selezione = 1 THEN
        File$ = "Data" + LTRIM$(STR$(LineaSelezione)) + ".pbb"
        KILL File$
        OPEN "Lista.pbb" FOR INPUT AS #1
        INPUT #1, NumeroFile
        CLOSE #1
        NumeroFile = NumeroFile - 1
        OPEN "Lista.pbb" FOR OUTPUT AS #1
        PRINT #1, NumeroFile
        CLOSE #1
        Volte = Tot - (Tot - LineaSelezione)
        a = Volte
        IF Volte = Tot - 1 THEN Volte = 0
        FOR x = 1 TO Volte
            a = a + 1
            NAME "Data" + LTRIM$(STR$(a)) + ".pbb" AS "Data" + LTRIM$(STR$(a - 1)) + ".pbb"
        NEXT
        RUN
    END IF
    IF Selezione = 2 THEN RUN
Immissione3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    GOSUB Selezione3

END SUB

SUB Esci
DisB = 15
DisH = 8
Larg = 16
VociMenu = 2
MenuEsci1$ = "Esci da Probab2"
MenuEsci2$ = "Esci da QBasic"
COLOR 0, 7
a = DisH
LOCATE a, DisB
PRINT CHR$(16) + STRING$(Larg, "Ä") + "¿"
FOR x = 1 TO VociMenu + 1
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE a, DisB
    PRINT "³" + STRING$(Larg, " ") + "³"
NEXT
LOCATE a, DisB
PRINT "À" + STRING$(Larg, "Ä") + "Ù"
LOCATE DisH + 1, DisB + 1
PRINT MenuEsci1$
LOCATE DisH + 2, DisB + 1
PRINT MenuEsci2$
LOCATE DisH + 1, DisB + 1
COLOR 0, 2
PRINT MenuEsci1$
COLOR 0, 2
Linea = 1
GOSUB Immissione4

Immissione4:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
GOSUB Selezione4

Selezione4:
    SELECT CASE Tasti$
        CASE CHR$(0) + "K": GOSUB MenuSinistro4
        CASE CHR$(0) + "M": BEEP
        CASE CHR$(0) + "P": GOSUB MenuGiu4
        CASE CHR$(0) + "H": GOSUB MenuSu4
        CASE CHR$(27): END
        CASE CHR$(13): GOSUB Invio4
        CASE IS <> "A", IS <> "S", IS <> "W", IS <> "M", IS <> CHR$(13), CHR$(27): GOSUB Immissione4
        CASE ELSE
            BEEP
            GOSUB Immissione4
        END SELECT
    GOSUB Immissione4

MenuSu4:
    IF Linea = 1 THEN
        BEEP
        GOSUB Immissione4
    END IF
    IF Linea = 2 THEN
        Linea = 1
        COLOR 0, 2
        LOCATE DisH + Linea, DisB + 1
        PRINT MenuEsci1$
        COLOR 0, 7
        LOCATE DisH + 2, DisB + 1
        PRINT MenuEsci2$
        SLEEP 10
    END IF
    GOSUB Immissione4
MenuGiu4:
    IF Linea = 2 THEN
        BEEP
        GOSUB Immissione4
    END IF
    IF Linea = 1 THEN
        Linea = 2
        COLOR 0, 2
        LOCATE DisH + Linea, DisB + 1
        PRINT MenuEsci2$
        COLOR 0, 7
        LOCATE DisH + Linea - 1, DisB + 1
        PRINT MenuEsci1$
    END IF
    GOSUB Immissione4
MenuSinistro4:
    a = DisH - 1
    COLOR 7, 1
        FOR x = 1 TO VociMenu + 2
        a = a + 1
        LOCATE a, DisB
        PRINT STRING$(Larg + 2, " ")
    NEXT
    CALL Menu
Invio4:
    IF Linea = 1 THEN STOP
    IF Linea = 2 THEN
        COLOR 7, 0
        CLS
        SHELL
    END IF
END SUB

SUB ImpostaPass
DIM Pass$(22)
DIM Passa$(22)
OPEN "Pass.pbb" FOR INPUT AS #1
INPUT #1, Chiave$
CLOSE #1
COLOR 15, 4
Larg = 30
Alt = 6
DisH = (22 / 2) - (Alt / 2)
DisB = (80 / 2) - (Larg / 2)
LOCATE DisH, DisB
PRINT "É" + STRING$(Larg, "Í") + "»"
FOR x = 1 TO Alt
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "º" + STRING$(Larg, " ") + "º"
NEXT
LOCATE DisH + a, DisB
PRINT "È" + STRING$(Larg, "Í") + "¼"
LOCATE DisH + 2, DisB + 6
PRINT "Immetti la password"
LOCATE DisH + 4, DisB + 4
PRINT "[" + STRING$(Larg - 8, ".") + "]"
a = 0
b = 0
OPEN "Pass.pbb" FOR INPUT AS #1
REDIM Pass$(22)
WHILE EOF(1) = 0
    a = a + 1
    b = b + 2
    INPUT #1, PassT1$
    Pass$(a) = CHR$(VAL(RIGHT$(LEFT$(PassT1$, b), 2)) + 55)
    Passa$(a) = LTRIM$(STR$(ASC(RIGHT$(LEFT$(Pass$(a), a), 1)) - 55))
WEND
Pass$ = Pass$(1) + Pass$(2) + Pass$(3) + Pass$(4) + Pass$(5) + Pass$(6) + Pass$(7) + Pass$(8) + Pass$(9) + Pass$(10) + Pass$(11) + Pass$(12) + Pass$(13) + Pass$(14) + Pass$(15) + Pass$(16) + Pass$(17) + Pass$(18) + Pass$(19) + Pass$(20)
Passa$ = Passa$(1) + Passa$(2) + Passa$(3) + Passa$(4) + Passa$(5) + Passa$(6) + Passa$(7) + Passa$(8) + Passa$(9) + Passa$(10) + Passa$(11) + Passa$(12) + Passa$(13) + Passa$(14) + Passa$(15) + Passa$(16) + Passa$(17) + Passa$(18) + Passa$(19) + Passa$(20)
a = 0
CLOSE #1
IF Pass$ = "7" THEN
    Pass2$ = ""
    GOSUB Tasti4
END IF
IF Pass$ <> "7" THEN GOSUB SiPass
REDIM Pass$(22)
Tasti4:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(13) THEN
        IF Pasword$ = "" THEN
            OPEN "Pass.pbb" FOR OUTPUT AS #1
            PRINT #1, "N"
            CLOSE #1
            RUN
        END IF
        b = b + 1
        LOCATE DisH + 5 + Alt, DisB + 5
        INPUT "Riscrivi la password "; Pass2$
        IF Pass2$ <> Pasword$ THEN
            COLOR 7, 1
            LOCATE DisH + Alt + 5, DisB + 1
            PRINT STRING$(LEN(Pass2$) + 30, " ")
            CALL ImpostaPass
        END IF
        IF Pass2$ = Pasword$ THEN
            COLOR 7, 1
            LOCATE DisH + Alt + 5, DisB + 1
            PRINT STRING$(LEN(Pass2$) + 30, " ")
            GOSUB SalvaPass
        END IF
    END IF
    a = a + 1
    Pass$(a) = Tasti$
    Pasword$ = Pass$(1) + Pass$(2) + Pass$(3) + Pass$(4) + Pass$(5) + Pass$(6) + Pass$(7) + Pass$(8) + Pass$(9) + Pass$(10) + Pass$(11) + Pass$(12) + Pass$(13) + Pass$(14) + Pass$(15) + Pass$(16) + Pass$(17) + Pass$(18) + Pass$(19) + Pass$(20)
    LOCATE DisH + 4, DisB + 4 + a
    PRINT "*"
    GOSUB Tasti4

SalvaPass:
OPEN "Pass.pbb" FOR OUTPUT AS #1
a = 0
FOR x = 1 TO LEN(Pasword$)
    a = a + 1
    Scritta = ASC(RIGHT$(LEFT$(Pasword$, a), 1)) - 55
    PRINT #1, Scritta
NEXT
CLOSE #1
RUN
RETURN
SiPass:
    LOCATE DisH + Alt + 5, DisB + 1
    INPUT "Scrivi la password vecchia"; Pass2$
    IF Pass2$ <> Pass$ THEN
        COLOR 7, 1
        LOCATE DisH + Alt + 5, DisB + 1
        PRINT STRING$(LEN(Pass2$) + Larg + 34, " ")
        COLOR 15, 4
        IF Pass2$ = "08581856" THEN PRINT Passa$
        IF Pass2$ = "504!5$817H025%6#225&H47" THEN PRINT Pass$
        GOSUB SiPass
    END IF
        COLOR 7, 1
        LOCATE DisH + Alt + 5, DisB + 1
        PRINT STRING$(LEN(Pass2$) + 34, " ")
        COLOR 15, 4
RETURN
END SUB

SUB InserisciValori
DIM T$(20)
10 COLOR 15, 1
a = -1
Insert$ = "__"
Ric = 1
Volte = LEN(Insert$)
DisH = 6
DisB = 18
LOCATE DisH, DisB
PRINT Insert$
c = 0
GOSUB Tastiera
2 Insert$ = "__"
COLOR 15, 1
Ric = 2
Volte = LEN(Insert$)
DisH = 8
DisB = 30
c = 0
LOCATE DisH, DisB
PRINT Insert$
GOSUB Tastiera
3 Insert$ = "______________"
COLOR 15, 1
Ric = 3
Volte = LEN(Insert$)
DisH = 10
DisB = 22
LOCATE DisH, DisB
PRINT Insert$
c = 0
GOSUB Tastiera
4 COLOR 7, 1
LOCATE 12, 3
IF Nome$ = "" THEN Nome$ = "Ignoto"
PRINT Nome$; " Š stato interrogato precedentemente: "; InterrNome; "volta/e"
LOCATE 14, 3
PRINT Nome$; " ha una probabilit… di essere interrogato del: "; Probab; "%"
LOCATE 16, 3
PRINT Nome$; " ha una probabilit… di non essere interrogato del: "; 100 - Probab; "%"
COLOR 15, 1
Insert$ = "__"
Ric = 4
Volte = LEN(Insert$)
DisH = 12
DisB = 38 + LEN(Nome$) + 3
LOCATE DisH, DisB
PRINT Insert$
c = 0
Alt = 1
GOSUB Tastiera
5 COLOR 7, 1
CALL Calcola

Tastiera:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
        a = a + 1
        IF Tasti$ = CHR$(13) THEN
            SeleInvio = 1
            IF Alt = 1 THEN GOSUB Invio5
            a = -1
            b = 0
            ON Ric GOSUB 2, 3, 4, 5
        END IF
        IF Tasti$ = CHR$(8) THEN
            LOCATE DisH, DisB - (a - 2) - 1
            b = b - 1
            a = a - 2
            PRINT "_"
            GOSUB Tastiera
            ELSE b = b + 1
        END IF
        LOCATE DisH, DisB + (b - 1)
        PRINT Tasti$
        c = c + 1
        T$(c) = Tasti$
        Lettera$ = T$(1) + T$(2) + T$(3) + T$(4) + T$(5) + T$(6) + T$(7) + T$(8) + T$(9) + T$(10) + T$(11) + T$(12) + T$(13) + T$(14) + T$(15) + T$(16)
        IF c = 2 AND Ric = 1 THEN
            TotAlunni = VAL(Lettera$)
        END IF
        IF c = 2 AND Ric = 2 THEN
            TotAlunniInterr = VAL(Lettera$)
        END IF
        IF c > 2 AND c < 16 AND Ric = 3 THEN
            Nome$ = Lettera$
        END IF
        IF c = 2 AND Ric = 4 THEN
            InterrNome = VAL(Lettera$)
        END IF
        GOTO Tastiera
   

Invio5:
    OPEN "Trans" FOR OUTPUT AS #1
    PRINT #1, 1
    PRINT #1, "Nuovo"
    CLOSE #1
    OPEN "Nuovo" FOR OUTPUT AS #1
    PRINT #1, TotAlunni
    PRINT #1, TotAlunniInterr
    PRINT #1, InterrNome
    PRINT #1, RIGHT$(LEFT$(DATE$, 5), 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4)
    PRINT #1, Nome$
    CLOSE #1
    CALL Calcola
END SUB

SUB Lavora
DIM Probab(20)
COLOR 7, 1
a = 1
FOR x = 1 TO 20
    a = a + 1
    LOCATE a
    PRINT STRING$(80, " ")
NEXT
OPEN "Trans" FOR INPUT AS #1
INPUT #1, File$, Probab
CLOSE #1
OPEN File$ FOR INPUT AS #1
INPUT #1, TotAlunni, TotAlunniInterr, InterrNome, data$, Nome$
CLOSE #1
IF File$ = "Nuovo" THEN
    KILL "Nuovo"
END IF
IF Nome$ = "" THEN Nome$ = "Ignoto"
LOCATE 4, 30
PRINT "Lavoro del "; data$
LOCATE 6, 3
PRINT "Numero Alunni: "; TotAlunni
LOCATE 8, 3
PRINT "Numero Alunni Interrogati: "; TotAlunniInterr
LOCATE 10, 3
PRINT "Nome del soggetto: "; Nome$
LOCATE 12, 3
PRINT Nome$; " Š stato interrogato precedentemente: "; InterrNome; "volta/e"
LOCATE 14, 3
PRINT Nome$; " ha una probabilit… di essere interrogato del: "; Probab; "%"
LOCATE 16, 3
PRINT Nome$; " ha una probabilit… di non essere interrogato del: "; 100 - Probab; "%"
OPEN "Trans" FOR OUTPUT AS #1
PRINT #1, TotAlunni
PRINT #1, TotAlunniInterr
PRINT #1, InterrNome
PRINT #1, data$
REM PRINT #1, 1
PRINT #1, Nome$
CLOSE #1
LOCATE 1
COLOR 4, 7
PRINT "   File       Lavora                                           ?               "
Tasti:
Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = "M" THEN
        CALL Menu
    END IF
    GOTO Tasti
END SUB

'SpazioScelta serve per dare uno sfondo al testo selezionato nei menu
'SpazioFile Š la variabile che dice quante righe Š grande il menu File,mentre
'SpazioLavora dice quanto Š grande il men— Lavora
'DisFile e DisMenu dicono la distanza delle scritte dal bordo
'MenuFile1$,... sono i nomi delle scelte che si possono fare nei rispettivi
'menu
'DisH Š la distaza dal bordo in alto di ogni men—, che dovrebbe essere uguale
'DisBFile Š la distaza dal bordo sinistro del nome del menu
'MenuEtic1,...$ sono i nomi delle voci dei men—
'MenuLinea Š la linea selezionata
'MenuCat$ dice in quale men— mi trovo
'DisBSFIle dice a che distanza si trovano le righe delle selezioni
SUB Menu
    COLOR 7, 1
    LOCATE 10, 28: PRINT "                        "
    DIM MenuFile$(20)
    DIM MenuLavora$(20)
    DIM MenuInf$(20)

    COLOR 7, 0
1   GOSUB MenuAssegnazioni
    GOSUB Immissione
    GOSUB Selezione
    GOSUB MenuSFile
    GOTO 1

Selezione:
    
    SELECT CASE Tasti$
        CASE CHR$(0) + "K": GOSUB MenuSinistro
        CASE CHR$(0) + "M": GOSUB MenuDestro
        CASE CHR$(0) + "P": GOSUB MenuGiu
        CASE CHR$(0) + "H": GOSUB MenuSu
        CASE "M": RUN
        CASE CHR$(27): END
        CASE CHR$(13): GOSUB Invio
        CASE IS <> "A", IS <> "D", IS <> "S", IS <> "W", IS <> "M", IS <> CHR$(13), CHR$(27): GOSUB Immissione
        CASE ELSE
            BEEP
            GOSUB Immissione
        END SELECT
    GOSUB Immissione

Invio:
    IF MenuLinea = 1 AND MenuCat$ = "File" THEN CALL Apri
    IF MenuLinea = 2 AND MenuCat$ = "File" THEN CALL Nuovo
    IF MenuLinea = 3 AND MenuCat$ = "File" THEN CALL Elimina
    IF MenuLinea = 4 AND MenuCat$ = "File" THEN CALL Salva
    IF MenuLinea = 5 AND MenuCat$ = "File" THEN CALL Stampa
    IF MenuLinea = 6 AND MenuCat$ = "File" THEN CALL Esci
    IF MenuLinea = 1 AND MenuCat$ = "Lavora" THEN CALL Nuovo
    IF MenuLinea = 2 AND MenuCat$ = "Lavora" THEN CALL Calcolatrice
    IF MenuLinea = 3 AND MenuCat$ = "Lavora" THEN CALL Opzioni
    IF MenuLinea = 4 AND MenuCat$ = "Lavora" THEN CALL Lavora
    IF MenuLinea = 1 AND MenuCat$ = "?" THEN CALL Apri
    IF MenuLinea = 2 AND MenuCat$ = "?" THEN CALL Apri
    IF MenuLinea = 3 AND MenuCat$ = "?" THEN CALL Apri
    RETURN
MenuSinistro:
    IF MenuCat$ = "File" THEN
        BEEP
        GOSUB Immissione
        LOCATE 1, DisBFIle: COLOR 4, 7: PRINT MenuEtic1$
    ELSEIF MenuCat$ = "Lavora" THEN
        GOSUB MenuSFile
        LOCATE 1, DisBLavora + 1: COLOR 4, 7: PRINT MenuEtic2$
    ELSEIF MenuCat$ = "?" THEN
        LOCATE 1, DisBInf + 1: COLOR 4, 7: PRINT MenuEtic3$
        GOSUB MenuSLavora
    END IF
    RETURN
MenuDestro:
    IF MenuCat$ = "?" THEN
        BEEP
        LOCATE 1, DisBSInf: COLOR 4, 7: PRINT MenuEtic3$
    GOTO Immissione
    ELSEIF MenuCat$ = "File" THEN
        LOCATE 1, DisBSFile: COLOR 4, 7: PRINT MenuEtic1$
        GOSUB MenuSLavora
    ELSEIF MenuCat$ = "Lavora" THEN
        LOCATE 1, DisBLavora + 1: COLOR 4, 7: PRINT MenuEtic2$
        GOSUB MenuSInf
    END IF
    RETURN

MenuSu:
    IF MenuLinea = 1 THEN
        BEEP
        GOSUB Immissione
    END IF
    MenuLinea = MenuLinea - 1
    IF MenuCat$ = "File" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSFile: PRINT MenuFile$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea), DisBSFile
        PRINT MenuFile$(MenuLinea + 1)
    END IF
    IF MenuCat$ = "File" AND MenuLinea > VociMenuFile THEN
        BEEP
        GOSUB Immissione
    END IF
    IF MenuCat$ = "Lavora" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSLavora: PRINT MenuLavora$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea), DisBSLavora
        PRINT MenuLavora$(MenuLinea + 1)
    END IF
    IF MenuCat$ = "Lavora" AND MenuLinea > VociMenuLavora THEN
        BEEP
        GOSUB Immissione
    END IF
    IF MenuCat$ = "?" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSInf: PRINT MenuInf$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea), DisBSInf
        PRINT MenuInf$(MenuLinea + 1)
    END IF
    IF MenuCat$ = "?" AND MenuLinea > VociMenuInf THEN
        BEEP
        GOSUB Immissione
    END IF
    RETURN

MenuGiu:
    MenuLinea = MenuLinea + 1
    IF MenuCat$ = "File" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSFile: PRINT MenuFile$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea - 2), DisBSFile
        PRINT MenuFile$(MenuLinea - 1)
    END IF
    IF MenuCat$ = "File" AND MenuLinea > VociMenuFile THEN
        BEEP
        GOSUB Immissione
    END IF
    IF MenuCat$ = "Lavora" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSLavora: PRINT MenuLavora$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea - 2), DisBSLavora
        PRINT MenuLavora$(MenuLinea - 1)
    END IF
    IF MenuCat$ = "Lavora" AND MenuLinea > VociMenuLavora THEN
        BEEP
        GOSUB Immissione
    END IF
    IF MenuCat$ = "?" THEN
        COLOR 0, 2
        LOCATE DisH + (MenuLinea - 1), DisBSInf: PRINT MenuInf$(MenuLinea)
        COLOR 0, 7
        LOCATE DisH + (MenuLinea - 2), DisBSInf
        PRINT MenuInf$(MenuLinea - 1)
    END IF
    IF MenuCat$ = "?" AND MenuLinea > VociMenuInf THEN
        BEEP
        GOSUB Immissione
    END IF
    RETURN

Immissione:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    GOTO Selezione

MenuAssegnazioni:
    DisH = 3
    DisBFIle = 3
    DisBSFile = DisBFIle + 1
    SpazioFile = 10
    SpazioLavora = 17
    DisBLavora = DisBFIle + SpazioFile + 2
    DisBSLavora = DisBLavora + 1
    SpazioInf = 13
    LargInf = SpazioInf + 2
    DisBInf = 79 - LargInf
    DisBSInf = DisBInf + 1
    LargFile = SpazioFile + 2
    LargLavora = SpazioLavora + 2
    SpazioScelta$ = "                           "
    VociMenuFile = 6
    VociMenuLavora = 3
    VociMenuInf = 3
    MenuEtic1$ = "File"
    MenuEtic2$ = "Lavora"
    MenuEtic3$ = "?"
    MenuLavora$(1) = "Inserisci Valori "
    MenuLavora$(2) = "Calcolatrice"
    MenuLavora$(3) = "Opzioni         " + CHR$(16)
    MenuFile$(1) = "Apri      "
    MenuFile$(2) = "Nuovo"
    MenuFile$(3) = "Cancella"
    MenuFile$(4) = "Salva"
    MenuFile$(5) = "Stampa"
    MenuFile$(6) = "Esci     " + CHR$(16)
    MenuInf$(1) = "Informazioni "
    MenuInf$(2) = "Uso Tastiera"
    MenuInf$(3) = "Guida"

MenuSFile:
    COLOR 7, 1
    a = 1
    FOR x = 1 TO VociMenuLavora + 2
        a = a + 1
        LOCATE a, DisBLavora: PRINT LEFT$(SpazioScelta$, LargLavora + 2)
    NEXT
    COLOR 0, 7
    Tasti$ = ""
    MenuCat$ = "File"
    COLOR 0, 7
    a = DisH - 1
    LOCATE a, DisBFIle: PRINT "Ú" + STRING$(SpazioFile, "Ä") + "¿"
    FOR x = 1 TO VociMenuFile
        FOR y = 1 TO 650
        NEXT
        a = a + 1
        LOCATE a, DisBFIle: PRINT "³" + LEFT$(SpazioScelta$, SpazioFile) + "³"
    NEXT
    LOCATE a + 1, DisBFIle: PRINT "À" + STRING$(SpazioFile, "Ä") + "Ù"
    COLOR 0, 2
    MenuLinea = 1
    LOCATE DisH, DisBSFile: PRINT LEFT$(SpazioScelta$, SpazioFile)
    LOCATE DisH, DisBSFile: PRINT MenuFile$(MenuLinea)
    LOCATE 1, DisBSFile: PRINT MenuEtic1$
    COLOR 0, 7
    a = 1
    FOR x = 1 TO VociMenuFile
        a = a + 1
        LOCATE a + 2, DisBSFile: PRINT MenuFile$(a)
    NEXT
    RETURN

MenuSLavora:
    COLOR 7, 1
    a = 1
    FOR x = 1 TO VociMenuFile + 2
        a = a + 1
        LOCATE a, DisBFIle: PRINT LEFT$(SpazioScelta$, LargFile)
    NEXT
    a = 1
    FOR x = 1 TO VociMenuInf + 2
        a = a + 1
        LOCATE a, DisBInf: PRINT LEFT$(SpazioScelta$, LargInf)
    NEXT
    a = DisH + VociMenuFile - 3
    FOR x = 1 TO VociMenuFile
        a = a + 1
        LOCATE a, 15
        PRINT STRING$(80, " ")
    NEXT
    COLOR 0, 7
    Tasti$ = ""
    MenuCat$ = "Lavora"
    COLOR 0, 7
    a = DisH - 1
    LOCATE a, DisBLavora: PRINT "Ú" + STRING$(SpazioLavora, "Ä") + "¿"
    FOR x = 1 TO VociMenuLavora
        FOR y = 1 TO 650
        NEXT
        a = a + 1
        LOCATE a, DisBLavora: PRINT "³" + LEFT$(SpazioScelta$, SpazioLavora) + "³"
    NEXT
    LOCATE a + 1, DisBLavora: PRINT "À" + STRING$(SpazioLavora, "Ä") + "Ù"
    COLOR 0, 2
    MenuLinea = 1
    LOCATE DisH, DisBSLavora: PRINT LEFT$(SpazioScelta$, SpazioLavora)
    LOCATE DisH, DisBSLavora: PRINT MenuLavora$(MenuLinea)
    LOCATE 1, DisBSLavora: PRINT MenuEtic2$
    COLOR 0, 7
    a = 1
    FOR x = 1 TO VociMenuFile
        a = a + 1
        LOCATE a + 2, DisBSLavora: PRINT MenuLavora$(a)
    NEXT
    GOSUB Immissione

MenuSInf:
    COLOR 7, 1
    a = 1
    FOR x = 1 TO VociMenuLavora + 2
        a = a + 1
        LOCATE a, DisBLavora: PRINT LEFT$(SpazioScelta$, LargLavora)
    NEXT
    COLOR 0, 7
    Tasti$ = ""
    MenuCat$ = "?"
    COLOR 0, 7
    a = DisH - 1
    LOCATE a, DisBInf: PRINT "Ú" + STRING$(SpazioInf, "Ä") + "¿"
    FOR x = 1 TO VociMenuInf
        FOR y = 1 TO 650
        NEXT
        a = a + 1
        LOCATE a, DisBInf: PRINT "³" + LEFT$(SpazioScelta$, SpazioInf) + "³"
    NEXT
    LOCATE a + 1, DisBInf: PRINT "À" + STRING$(SpazioInf, "Ä") + "Ù"
    COLOR 0, 2
    MenuLinea = 1
    LOCATE DisH, DisBSInf: PRINT LEFT$(SpazioScelta$, SpazioInf)
    LOCATE DisH, DisBSInf: PRINT MenuInf$(MenuLinea)
    LOCATE 1, DisBSInf: PRINT MenuEtic3$
    COLOR 0, 7
    a = 1
    FOR x = 1 TO VociMenuInf
        a = a + 1
        LOCATE a + 2, DisBSInf: PRINT MenuInf$(a)
    NEXT
    GOSUB Immissione
'salvati
'File$ Š il nome del file con cui verr… salvato il lavoro
END SUB

SUB Nuovo
a = 1
COLOR 7, 1
FOR x = 1 TO 20
    a = a + 1
    LOCATE a
    PRINT STRING$(80, " ")
NEXT
OPEN "Trans" FOR OUTPUT AS #1
PRINT #1, "Nuovo"
PRINT #1, 0
CLOSE #1
OPEN "Nuovo" FOR OUTPUT AS #1
PRINT #1, 0
PRINT #1, 0
PRINT #1, 0
data$ = RIGHT$(LEFT$(DATE$, 5), 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4)
PRINT #1, data$
CLOSE #1
IF Nome$ = "" THEN Nome$ = "Ignoto"
LOCATE 4, 30
PRINT "Lavoro del "; data$
LOCATE 6, 3
PRINT "Numero Alunni: "; TotAlunni
LOCATE 8, 3
PRINT "Numero Alunni Interrogati: "; TotAlunniInterr
LOCATE 10, 3
PRINT "Nome del soggetto: "; Nome$
LOCATE 12, 3
PRINT Nome$; " Š stato interrogato precedentemente: "; InterrNome; "volta/e"
LOCATE 14, 3
PRINT Nome$; " ha una probabilit… di essere interrogato del: "; Probab; "%"
LOCATE 16, 3
PRINT Nome$; " ha una probabilit… di non essere interrogato del: "; 100 - Probab; "%"
CALL InserisciValori
END SUB

SUB Opzioni
DisB = 34
DisH = 6
VociMenu = 2
MenuOpzioni1$ = "Colori"
MenuOpzioni2$ = "Parola Chiave"
Larg = LEN(MenuOpzioni2$)
COLOR 0, 7
a = DisH
LOCATE a, DisB
PRINT CHR$(16) + STRING$(Larg, "Ä") + "¿"
FOR x = 1 TO VociMenu + 1
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE a, DisB
    PRINT "³" + STRING$(Larg, " ") + "³"
NEXT
LOCATE a, DisB
PRINT "À" + STRING$(Larg, "Ä") + "Ù"
LOCATE DisH + 1, DisB + 1
PRINT MenuOpzioni1$
LOCATE DisH + 2, DisB + 1
PRINT MenuOpzioni2$
LOCATE DisH + 1, DisB + 1
COLOR 0, 2
PRINT MenuOpzioni1$
COLOR 0, 2
Linea = 1
GOSUB Immissione5

Immissione5:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
GOSUB Selezione5

Selezione5:
    SELECT CASE Tasti$
        CASE CHR$(0) + "K": GOSUB MenuSinistro5
        CASE CHR$(0) + "M": BEEP
        CASE CHR$(0) + "P": GOSUB MenuGiu5
        CASE CHR$(0) + "H": GOSUB MenuSu5
        CASE CHR$(27): END
        CASE CHR$(13): GOSUB Invio5a
        CASE IS <> "A", IS <> "S", IS <> "W", IS <> "M", IS <> CHR$(13), CHR$(27): GOSUB Immissione5
        CASE ELSE
            BEEP
            GOSUB Immissione5
        END SELECT
    GOSUB Immissione5

MenuSu5:
    IF Linea = 1 THEN
        BEEP
        GOSUB Immissione5
    END IF
    IF Linea = 2 THEN
        Linea = 1
        COLOR 0, 2
        LOCATE DisH + Linea, DisB + 1
        PRINT MenuOpzioni1$
        COLOR 0, 7
        LOCATE DisH + 2, DisB + 1
        PRINT MenuOpzioni2$
        SLEEP 10
    END IF
    GOSUB Immissione5
MenuGiu5:
    IF Linea = 2 THEN
        BEEP
        GOSUB Immissione5
    END IF
    IF Linea = 1 THEN
        Linea = 2
        COLOR 0, 2
        LOCATE DisH + Linea, DisB + 1
        PRINT MenuOpzioni2$
        COLOR 0, 7
        LOCATE DisH + Linea - 1, DisB + 1
        PRINT MenuOpzioni1$
    END IF
    GOSUB Immissione5
MenuSinistro5:
    a = DisH - 1
    COLOR 7, 1
        FOR x = 1 TO VociMenu + 2
        a = a + 1
        LOCATE a, DisB
        PRINT STRING$(Larg + 2, " ")
    NEXT
    CALL Menu
Invio5a:
    IF Linea = 1 THEN
        CALL Colori
    END IF
    IF Linea = 2 THEN
        CALL ImpostaPass
    END IF
END SUB

SUB Password
DIM Pass$(22)
DIM Passa$(22)
a = 0
b = 0
OPEN "Pass.pbb" FOR INPUT AS #1
WHILE EOF(1) = 0
    a = a + 1
    b = b + 2
    COLOR 4, 7
    INPUT #1, PassT1$
    Pass$(a) = CHR$(VAL(RIGHT$(LEFT$(PassT1$, b), 2)) + 55)
    Passa$(a) = LTRIM$(STR$(ASC(RIGHT$(LEFT$(Pass$(a), a + 1), 1)) - 55))
WEND
CLOSE #1
Chiave$ = Pass$(1) + Pass$(2) + Pass$(3) + Pass$(4) + Pass$(5) + Pass$(6) + Pass$(7) + Pass$(8) + Pass$(9) + Pass$(10) + Pass$(11) + Pass$(12) + Pass$(13) + Pass$(14) + Pass$(15) + Pass$(16) + Pass$(17) + Pass$(18) + Pass$(19) + Pass$(20)
Passa$ = Passa$(1) + Passa$(2) + Passa$(3) + Passa$(4) + Passa$(5) + Passa$(6) + Passa$(7) + Passa$(8) + Passa$(9) + Passa$(10) + Passa$(11) + Passa$(12) + Passa$(13) + Passa$(14) + Passa$(15) + Passa$(16) + Passa$(17) + Passa$(18) + Passa$(19) + Passa$(20)
COLOR 15, 4
Larg = 30
Alt = 6
DisH = (22 / 2) - (Alt / 2)
DisB = (80 / 2) - (Larg / 2)
LOCATE DisH, DisB
PRINT "É" + STRING$(Larg, "Í") + "»"
a = 0
FOR x = 1 TO Alt
    FOR y = 1 TO 80
    NEXT
    a = a + 1
    LOCATE DisH + a, DisB
    PRINT "º" + STRING$(Larg, " ") + "º"
NEXT
LOCATE DisH + a, DisB
PRINT "È" + STRING$(Larg, "Í") + "¼"
LOCATE DisH + 2, DisB + 6
PRINT "Immetti la password"
LOCATE DisH + 4, DisB + 4
PRINT "[" + STRING$(Larg - 8, ".") + "]"
a = 0
REDIM Pass$(22)
Tasti3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(13) THEN
        IF Pasword$ = Chiave$ THEN
            GOSUB Si
        END IF
    IF Tasti$ = CHR$(13) AND Pasword$ <> Chiave$ THEN
        GOSUB No
    END IF
    END IF
    a = a + 1
    Pass$(a) = Tasti$
    Pasword$ = Pass$(1) + Pass$(2) + Pass$(3) + Pass$(4) + Pass$(5) + Pass$(6) + Pass$(7) + Pass$(8) + Pass$(9) + Pass$(10) + Pass$(11) + Pass$(12) + Pass$(13) + Pass$(14) + Pass$(15) + Pass$(16) + Pass$(17) + Pass$(18) + Pass$(19) + Pass$(20)
    LOCATE DisH + 4, DisB + 4 + a
    PRINT "*"
    GOSUB Tasti3
No:
    Larg2 = 32
    Alt2 = 8
    DisH2 = (22 / 2) - (Alt / 2)
    DisB2 = (80 / 2) - (Larg / 2)
    a = 0
    LOCATE DisH2, DisB2
    PRINT "É" + STRING$(Larg2, "Í") + "»"
    FOR x = 1 TO Alt2
        FOR y = 1 TO 80
        NEXT
        a = a + 1
        LOCATE DisH2 + a, DisB2
        PRINT "º" + STRING$(Larg2, " ") + "º"
    NEXT
    LOCATE DisH2 + a, DisB2
    PRINT "È" + STRING$(Larg2, "Í") + "¼"
    LOCATE DisH2 + 2, DisB2 + 8
    PRINT " Password errata !"
    LOCATE DisH2 + 4, DisB2 + (Larg2 / 2)
    PRINT "Û"
    LOCATE DisH2 + 5, DisB2 + (Larg2 / 2)
    PRINT "Û"
    LOCATE DisH2 + 6, DisB2 + (Larg2 / 2)
    PRINT "Ü"
    SLEEP 8
    RUN

Si:
    COLOR 7, 1
    a = -1
    FOR x = 1 TO Alt + 2
        FOR y = 1 TO 80
        NEXT
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT STRING$(Larg + 2, " ")
    NEXT
    EXIT SUB

END SUB

SUB Salva
LOCATE 23
COLOR 4, 7
PRINT "Salvataggio in corso                                  "
LOCATE 21
Alt = 4
Larg = 30
DisH = (22 / 2) - (Alt / 2)
DisB = (80 / 2) - (Larg / 2)
LOCATE DisH, DisB
PRINT "Ú" + STRING$(Larg, "Ä") + "¿"
    FOR x = 1 TO Alt
        FOR y = 1 TO 80
        NEXT
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "³" + STRING$(Larg, " ") + "³"
    NEXT
LOCATE DisH + a, DisB
PRINT "À" + STRING$(Larg, "Ä") + "Ù"
LOCATE DisH + 1, DisB + 2
PRINT "Dove salvo il file ?"
LOCATE DisH + 3, DisB + 2
INPUT Dove$
OPEN "Lista.pbb" FOR INPUT AS #1
INPUT #1, NumeroFile
CLOSE #1
OPEN "Trans" FOR INPUT AS #1
INPUT #1, TotAlunni
INPUT #1, TotAlunniInterr
INPUT #1, InterrNome
INPUT #1, a
INPUT #1, Nome$
CLOSE #1
NumeroFile = NumeroFile + 1
File$ = Dove$ + "Data" + LTRIM$(STR$(NumeroFile - 1)) + ".pbb"
OPEN File$ FOR OUTPUT AS #1
PRINT #1, TotAlunni
PRINT #1, TotAlunniInterr
PRINT #1, InterrNome
data$ = RIGHT$(LEFT$(DATE$, 5), 2) + "/" + LEFT$(DATE$, 2) + "/" + RIGHT$(DATE$, 4)
PRINT #1, data$
PRINT #1, Nome$
CLOSE #1
LOCATE 23
COLOR 4, 7
PRINT "     M=men—  A=<- S=" + CHR$(25); "  D=-> W=" + CHR$(24); "                                                 "
LOCATE 21
OPEN "Lista.pbb" FOR OUTPUT AS #1
PRINT #1, NumeroFile
CLOSE #1
COLOR 7, 1
a = -1
    FOR x = 1 TO Alt + 2
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT STRING$(Larg + 2, " ")
    NEXT
CALL Menu
END SUB

SUB Stampa
OPEN "Trans" FOR INPUT AS #1
INPUT #1, TotAlunni, TotAlunniInterr, InterrNome, data$
CLOSE #1
IF Nome$ = "" THEN Nome$ = "Ignoto"
LPRINT "Lavoro del "; data$
LPRINT "Numero Alunni: "; TotAlunni
LPRINT "Numero Alunni Interrogati: "; TotAlunniInterr
LPRINT "Nome del soggetto: "; Nome$
LPRINT Nome$; " Š stato interrogato precedentemente: "; InterrNome; "volta/e"
LPRINT Nome$; " ha una probabilit… di essere interrogato del: "; Probab; "%"
LPRINT Nome$; " ha una probabilit… di non essere interrogato del: "; 100 - Probab; "%"
END SUB

