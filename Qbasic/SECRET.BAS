DECLARE SUB Decifraggio ()
DECLARE SUB Ritorno ()
DECLARE SUB Informazioni ()
DECLARE SUB ErrorControl (Errore$)
DECLARE SUB Segreti ()
DIM SHARED Parole$(1000)
DIM SHARED Trad$(1000)
TYPE Variabili
    s AS STRING * 10
    Errore AS STRING * 255
    aa AS INTEGER
END TYPE

ON ERROR GOTO Errori
DO
    CLS
    INPUT "Password "; Pass$
LOOP UNTIL Pass$ = "FABRIZIO"
3 SCREEN 0
COLOR 7, 0
CLS
LOCATE 2
PRINT "Scrivi la modalit… di uso del programma"
INPUT "(SECRET - UNSECRET - ESC) "; s$
IF UCASE$(s$) = UCASE$("UNSECRET") THEN Decifraggio
IF UCASE$(s$) = UCASE$("SECRET") THEN Segreti
IF UCASE$(s$) = UCASE$("ESC") THEN STOP
IF UCASE$(s$) = "/*/" THEN Informazioni
IF UCASE$(s$) = "" THEN END
RUN 3

Errori:
    LOCATE 1
    COLOR 4, 0
    IF ERR = 5 THEN Errore$ = "Codice errato/non presente"
    IF ERR = 53 THEN Errore$ = "File non trovato"
    IF ERR = 52 THEN Errore$ = "File non trovato"
    IF ERR = 69 THEN Errore$ = "Overflow del buffer"
    IF ERR = 72 THEN Errore$ = "Disco rovinato"
    IF ERR = 75 THEN Errore$ = "Percorso sbagliato"
    IF ERR = 76 THEN Errore$ = "Percorso sbagliato"
RESUME

SUB Decifraggio STATIC
CLS
Subprog$ = "Decifraggio"
LOCATE 2
PRINT "Scrivi il percorso e il nome del file da decifrare"
PRINT "Es. C:\UTILITA\QBASIC\AGENDA2.AGE"
LOCATE 4
INPUT "File da decifrare (INVIO per terminare)"; File$
IF File$ = "" THEN STOP
OPEN File$ FOR INPUT AS #1
WHILE EOF(1) = 0
    A = A + 1
    INPUT #1, Parole$(A)
WEND
CLOSE
LOCATE 5
PRINT "Decodificazione:"
Selezione:
SELECT CASE Parole$(1)
    CASE "083101099114101116"  'Metodo ASC() 3x
        GOSUB Metodo1
    CASE "150168166181168183"   'Metodo ACS()+67 3x
        GOSUB Metodo2
    CASE "284644594661"  'Metodo ASC()-55 2x
        GOSUB Metodo3
    CASE "607876917893"   'Metodo ASC()-23 2x
        GOSUB Metodo4
    CASE "Tfdsfu" 'Metodo lettera per lettera
        GOSUB Metodo5
    CASE ELSE
        GOSUB NessunMetodo
END SELECT

Metodo1:
    A = 0
    FOR x = 1 TO UBOUND(Parole$)
        b = 0
        A = A + 1
        IF Parole$(A) <> "" THEN
            FOR Z = 1 TO LEN(Parole$(A)) / 3
                b = b + 3
                A$ = RIGHT$(LEFT$(Parole$(A), b), 3)
                Trad$(A) = Trad$(A) + CHR$(VAL(A$))
                FOR y = 1 TO 100
                    LOCATE 22
                    PRINT "Decifraggio lettra:"; A$
                NEXT
            NEXT
            IF Trad$(A) <> "" THEN
                LOCATE 5 + A
                PRINT Trad$(A)
                Trad$ = ""
            END IF
        END IF
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato      "
    Ritorno

Metodo2:
    A = 0
    FOR x = 1 TO UBOUND(Parole$)
        b = 0
        A = A + 1
        IF Parole$(A) <> "" THEN
            FOR Z = 1 TO LEN(Parole$(A)) / 3
                b = b + 3
                A$ = RIGHT$(LEFT$(Parole$(A), b), 3)
                Trad$(A) = Trad$(A) + LTRIM$(RTRIM$(CHR$(VAL(A$) - 67)))
                FOR y = 1 TO 100
                    LOCATE 22
                    PRINT "Decifraggio lettra:"; A$
                NEXT
            NEXT
            IF Trad$(A) <> "" THEN
                LOCATE 5 + A
                PRINT Trad$(A)
                Trad$ = ""
            END IF
        END IF
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato    "
    Ritorno

Metodo3:
    A = 0
    FOR x = 1 TO UBOUND(Parole$)
        b = 0
        A = A + 1
        IF Parole$(A) <> "" THEN
            FOR Z = 1 TO LEN(Parole$(A)) / 2
                b = b + 2
                A$ = RIGHT$(LEFT$(Parole$(A), b), 2)
                Trad$(A) = Trad$(A) + CHR$(VAL(A$) + 55)
                FOR y = 1 TO 100
                    LOCATE 22
                    PRINT "Decifraggio lettra:"; A$
                NEXT
            NEXT
            IF Trad$(A) <> "" THEN
                LOCATE 5 + A
                PRINT Trad$(A)
                Trad$ = ""
            END IF
        END IF
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato         "
    Ritorno

Metodo4:
    A = 0
    FOR x = 1 TO UBOUND(Parole$)
        b = 0
        A = A + 1
        IF Parole$(A) <> "" THEN
            FOR Z = 1 TO LEN(Parole$(A)) / 2
                b = b + 2
                A$ = RIGHT$(LEFT$(Parole$(A), b), 2)
                Trad$(A) = Trad$(A) + CHR$(VAL(A$) + 23)
                FOR y = 1 TO 100
                    LOCATE 22
                    PRINT "Decifraggio lettra:"; A$
                NEXT
            NEXT
            IF Trad$(A) <> "" THEN
                LOCATE 5 + A
                PRINT Trad$(A)
                Trad$ = ""
            END IF
        END IF
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato        "
    Ritorno

Metodo5:
    A = 0
    FOR x = 1 TO UBOUND(Parole$)
        b = 0
        A = A + 1
        IF Parole$(A) <> "" THEN
            FOR Z = 1 TO LEN(Parole$(A))
                b = b + 1
                aa = ASC(RIGHT$(LEFT$(Parole$(A), b), 1))
                Trad$(A) = Trad$(A) + CHR$(aa - 1)
                FOR y = 1 TO 100
                    LOCATE 22
                    PRINT "Decifraggio lettra:"; aa
                NEXT
            NEXT
            IF Trad$(A) <> "" THEN
                LOCATE 5 + A
                PRINT Trad$(A)
                Trad$ = ""
            END IF
        END IF
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato        "
    Ritorno

NessunMetodo:
    LOCATE 6
    INPUT "Scrivi il metodo di cript "; Rico$
    IF Rico$ = "*" THEN
        PRINT "1- Metodo ASC()x3"
        PRINT "2- Metodo ACS()+67 3x"
        PRINT "3- Metodo ASC()-55 2x"
        PRINT "4- Metodo ASC()-23 2x"
        PRINT "5- Metodo lettera per lettera"
        GOSUB NessunMetodo
    END IF
    A = 0
    FOR x = 1 TO 6
        A = A + 1
        LOCATE 5 + A
        PRINT STRING$(70, " ")
    NEXT
    A = 0
    IF Rico$ = "1" THEN GOSUB Metodo1
    IF Rico$ = "2" THEN GOSUB Metodo2
    IF Rico$ = "3" THEN GOSUB Metodo3
    IF Rico$ = "4" THEN GOSUB Metodo4
    IF Rico$ = "5" THEN GOSUB Metodo5
    RETURN
END SUB

SUB ErrorControl (Errore$)
    COLOR 12, 0
    LOCATE 1
    PRINT Errore$
    PRINT Subprog$
    COLOR 7, 0
END SUB

SUB Informazioni
    CLS
    LOCATE 1
    PRINT "                          C O D I C I   S E G R E T I"
    PRINT "Metodo ASC() 3x;significa metodo che usa tre cifre(3x) del carattere ASCII."
    PRINT "Quindi, teoricamente, si potrebbero usare tutti i caratteri del codice ASCII"
    PRINT "ma il problema Š che una persona che lo conosce, saprebbe subito tradurlo."
    PRINT "Indice di protezione:   **                  Difficolt… di progetto: *"
    PRINT "Metodo ACS()+67 3x;anch'esso utilizza il procedimento (3x).Il difetto del (3x)"
    PRINT "sta che la parola verrebbe lunghissima, problema risolto col metodo (2x)."
    PRINT "La differenza del metodo precedente Š che in questo vengono usati dei numeri"
    PRINT "sopra il 100, usando una somma +67.Si possono utilizzare tutti i caratteri ASCII"
    PRINT "Indice di protezione:   ***                 Difficolt… di progetto: ***"
    PRINT "Metodo ASC()-55 2x,si pu• definire l'evoluzione del metodo(3x),ma per• pu• usare"
    PRINT "due cifre e quindi bisogna sottrarre -55 per avere -unico difetto- le lettere"
    PRINT "maiuscole e minuscole.Tutti gli altri caratteri non possono essere utilizzati."
    PRINT "Indice di protezione:   ****                Difficolt… di progetto: ***"
    PRINT "Metodo ASC()-23 2x; il miglior codice. Ha due cifre (2x) e quindi una parola Š "
    PRINT "il doppio di quella reale, mentre nel (3x) Š lunga 3 volte quella reale.Posse-"
    PRINT "dendo una sottrazione di -23 si possono utilizzare tutti i caratteri dal 33 al "
    PRINT "122, cioŠ quasi tutta la tastiera"
    PRINT "Indice di protezione:   *****               Difficolt… di progetto: ***"
    PRINT "Metodo lettera per lettera; Š un metodo poco utilizzato che non fornisce un alto"
    PRINT "grado di protezione.La lettera vera Š la sequente a quella vera. Utilizza solo "
    PRINT "lettere"
    PRINT "Indice di protezione:   ***                 Difficolt… di progetto: ****"
    SLEEP 60
END SUB

SUB Ritorno
    LOCATE 22
    PRINT "Premere un tasto per continuare"
    LOCATE 21
    PRINT "Decifraggio terminato"
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    RUN 3
END SUB

SUB Segreti STATIC
Subprog$ = "Segreti"
CLS
LOCATE 2
PRINT "Scrivi il percorso e il nome del file da codificare"
PRINT "Es. C:\UTILITA\QBASIC\AGENDA2.AGE"
LOCATE 4
INPUT "File da codificare (INVIO per terminare) "; File$
OPEN File$ FOR OUTPUT AS #1
INPUT "Che codice vuoi usare "; Cod$
INPUT "Quante parole sono "; NParole
CLS
SELECT CASE Cod$
    CASE "1"
        GOSUB Codice1
    CASE "2"
        GOSUB Codice2
    CASE "3"
        GOSUB Codice3
    CASE "4"
        GOSUB Codice4
    CASE "5"
        GOSUB Codice5
    CASE ELSE
        CALL Segreti
END SELECT

Codice1:
    PRINT #1, "083101099114101116"
    FOR x = 1 TO NParole
        A = A + 1
        b = 0
        INPUT "Scrivi la parola "; Parole$(A)
        FOR y = 1 TO LEN(Parole$(A))
            b = b + 1
            A$ = LTRIM$(RTRIM$(STR$(ASC(RIGHT$(LEFT$(Parole$(A), b), 1)))))
            IF LEN(A$) = 2 THEN A$ = "0" + A$
            b$ = b$ + A$
            FOR Z = 1 TO 100
                LOCATE 22
                PRINT "Decifraggio lettera:"; A$
            NEXT
        NEXT
        CLS
        PRINT #1, b$
        b$ = ""
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato               "
    CLOSE #1
    File1$ = LEFT$(RIGHT$(File$, 4), 1)
    IF File1$ <> "." THEN
        CLS
        LOCATE 2
        PRINT " Possibili estensioni del file '"; File$; "'"
        PRINT "1- .sct"
        PRINT "2- nessuna"
        INPUT "Che estensione vuoi dare al file "; Est
        IF Est = 1 THEN NAME File$ AS File$ + ".sct"
        CLS
    END IF
GOTO Ritorno2

Codice2:
    PRINT #1, "150168166181168183"
    FOR x = 1 TO NParole
        A = A + 1
        b = 0
        INPUT "Scrivi la parola "; Parole$(A)
        FOR y = 1 TO LEN(Parole$(A))
            b = b + 1
            A$ = LTRIM$(RTRIM$(STR$(ASC(RIGHT$(LEFT$(Parole$(A), b), 1)) + 67)))
            IF VAL(A$) < 100 THEN
                BEEP
                RUN
            END IF
            IF LEN(A$) = 2 THEN A$ = "0" + A$
            b$ = b$ + A$
            FOR Z = 1 TO 100
                LOCATE 22
                PRINT "Decifraggio lettera:"; A$
            NEXT
        NEXT
        CLS
        PRINT #1, b$
        b$ = ""
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato               "
    CLOSE #1
    File1$ = LEFT$(RIGHT$(File$, 4), 1)
    IF File1$ <> "." THEN
        CLS
        LOCATE 2
        PRINT " Possibili estensioni del file '"; File$; "'"
        PRINT "1- .sct"
        PRINT "2- .sct"
        PRINT "3- nessuna"
        INPUT "Che estensione vuoi dare al file "; Est
        IF Est = 1 THEN NAME File$ AS File$ + ".sct"
        IF Est = 2 THEN NAME File$ AS File$ + ".lsc"
        CLS
    END IF
GOTO Ritorno2

Codice3:
    PRINT #1, "284644594661"
    FOR x = 1 TO NParole
        A = A + 1
        b = 0
        INPUT "Scrivi la parola "; Parole$(A)
        FOR y = 1 TO LEN(Parole$(A))
            b = b + 1
            A$ = LTRIM$(RTRIM$(STR$(ASC(RIGHT$(LEFT$(Parole$(A), b), 1)) - 55)))
            IF VAL(A$) > 99 THEN
                BEEP
                RUN
            END IF
            b$ = b$ + A$
            FOR Z = 1 TO 100
                LOCATE 22
                PRINT "Decifraggio lettera:"; A$
            NEXT
        NEXT
        CLS
        PRINT #1, b$
        b$ = ""
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato               "
    CLOSE #1
    File1$ = LEFT$(RIGHT$(File$, 4), 1)
    IF File1$ <> "." THEN
        CLS
        LOCATE 2
        PRINT " Possibili estensioni del file '"; File$; "'"
        PRINT "1- .pbb"
        PRINT "2- .sct"
        PRINT "3- nessuna"
        INPUT "Che estensione vuoi dare al file "; Est
        IF Est = 1 THEN NAME File$ AS File$ + ".pbb"
        IF Est = 2 THEN NAME File$ AS File$ + ".sct"
        CLS
    END IF
GOTO Ritorno2

Codice4:
    PRINT #1, "607876917893"
    FOR x = 1 TO NParole
        A = A + 1
        b = 0
        INPUT "Scrivi la parola "; Parole$(A)
        FOR y = 1 TO LEN(Parole$(A))
            b = b + 1
            A$ = LTRIM$(RTRIM$(STR$(ASC(RIGHT$(LEFT$(Parole$(A), b), 1)) - 23)))
            IF VAL(A$) > 99 THEN
                BEEP
                RUN
            END IF
            b$ = b$ + A$
            FOR Z = 1 TO 100
                LOCATE 22
                PRINT "Decifraggio lettera:"; A$
            NEXT
        NEXT
        CLS
        PRINT #1, b$
        b$ = ""
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato               "
    CLOSE #1
    File1$ = LEFT$(RIGHT$(File$, 4), 1)
    IF File1$ <> "." THEN
        CLS
        LOCATE 2
        PRINT " Possibili estensioni del file "; File$
        PRINT "1- .age"
        PRINT "2- .sct"
        PRINT "3- nessuna"
        INPUT "Che estensione vuoi dare al file "; Est
        IF Est = 1 THEN NAME File$ AS File$ + ".age"
        IF Est = 2 THEN NAME File$ AS File$ + ".sct"
        CLS
    END IF
GOTO Ritorno2

Codice5:
    PRINT #1, "Tfdsfu"
    FOR x = 1 TO NParole
        A = A + 1
        b = 0
        INPUT "Scrivi la parola "; Parole$(A)
        FOR y = 1 TO LEN(Parole$(A))
            b = b + 1
            A$ = CHR$(ASC(RIGHT$(LEFT$(Parole$(A), b), 1)) + 1)
            b$ = b$ + A$
            FOR Z = 1 TO 100
                LOCATE 22
                PRINT "Decifraggio lettera:"; A$
            NEXT
        NEXT
        CLS
        PRINT #1, b$
        b$ = ""
    NEXT
    LOCATE 22
    PRINT "Decifraggio terminato               "
    CLOSE #1
    File1$ = LEFT$(RIGHT$(File$, 4), 1)
    IF File1$ <> "." THEN
        CLS
        LOCATE 2
        PRINT " Possibili estensioni del file '"; File$; "'"
        PRINT "1- .sct"
        PRINT "2- nessuna"
        INPUT "Che estensione vuoi dare al file "; Est
        IF Est = 1 THEN NAME File$ AS File$ + ".sct"
        CLS
    END IF
GOTO Ritorno2

Ritorno2:
    LOCATE 22
    PRINT "Premere un tasto per continuare"
    LOCATE 21
    PRINT "Decifraggio terminato"
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    RUN 3
END SUB

