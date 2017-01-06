'
'                          Q b a s i c    A g e n d a
'
'                        Copyright (C) Col S.N.C. 2000
'                   Copyright (C) Microsoft Corporation 1990
'
' Questa agenda telefonica permette di archiviare i vostri numeri telefonici
' in modo semplicissimo utilizzando il tastierino numerico o i numeri sopra
' la tastiera per muovervi nel men—. Le funzioni pi— importanti sono segnate
' nella barra men—. Per avviare il programma premete i tasti shift + F5,
' mentre per uscire premete il tasto ESC. C'Š anche la possibilit… di mettere
' una password al vostro numero di telefono "segreto", senza aver paura che
' qualche curioso veda i vostri segreti, perchŠ il sistema di archiviazione
' con password prevede di criptare i file. Se poi decidete che un numero non
' vi interessa pi— potete tranquillamente cancellarlo.
' Si pu• dire che questa agenda sia perfetta, purtroppo non Š cos: ogni tanto
' si blocca oppure da il messaggio "HEAP di tipo far alterato". In questo caso
' riavviare il computer e l'agenda.
' L'agenda telefonica in se non ha limiti di immagazzinamento, tranne che
' sullo schermo che ci stanno un massimo di 36 nomi contemporaneamente, se si
' supera questo limite la schermata "impazzisce" e c'Š una buona probabilit…
' che non vediate i primi nomi.
' Inoltre a fianco della struttura del programma c'Š la traduzione in italiano
' del programma stesso, cos potrete capire come funziona.
' Buon divertimento !
'
DECLARE SUB Presentazione ()
DECLARE SUB Avvertenze ()
DECLARE SUB Nuovo ()
DECLARE SUB Ricerca ()
DECLARE SUB Elimina ()
ON ERROR GOTO Errori

Inizio:
SCREEN 0
COLOR 7, 1
CLS
LOCATE 1
COLOR 4, 7
PRINT "      1-Nuovo         2-Ricerca         3-Elimina         4-Avvertenze          "
LOCATE 23
PRINT "                                                                                "
LOCATE 20
Tasti$ = ""
WHILE Tasti$ = ""
    Tasti$ = INKEY$
WEND
SELECT CASE Tasti$
    CASE CHR$(27)
        STOP
    CASE CHR$(49)
        CALL Nuovo
    CASE CHR$(50)
        CALL Ricerca
    CASE CHR$(51)
        CALL Elimina
    CASE CHR$(52)
        CALL Avvertenze
    CASE ELSE
        BEEP
        GOTO Inizio
END SELECT
GOTO Inizio

Errori:
    RUN
END

SUB Avvertenze
COLOR 14, 4
LOCATE 6, 20
PRINT "É" + STRING$(38, "Í") + "»"
a = 0
FOR x = 1 TO 10
    a = a + 1
    LOCATE 6 + a, 20
    PRINT "º" + STRING$(38, " ") + "º"
NEXT
LOCATE 6 + a, 20
PRINT "È" + STRING$(38, "Í") + "¼"
COLOR 15, 4
LOCATE 8, 24
PRINT "     A V V E R T E N Z E "
LOCATE 9, 22
PRINT "Attenzione ad usare questo programma"
LOCATE 10, 22
PRINT "E'molto instabile e si blocca spesso"
LOCATE 11, 22
PRINT "Si blocca ogni 2 volte che salvate "
LOCATE 12, 22
PRINT "un numero "
LOCATE 13, 22
PRINT "Riusate il programma riavviando il"
LOCATE 14, 22
PRINT "computer e il comando RESTORE"
END
END SUB

SUB Elimina
LOCATE 1
PRINT "    [ Elimina ]                                                               "
OPEN "Lista.age" FOR INPUT AS #1
COLOR 7, 1
INPUT #1, NFile
CLOSE #1
FOR x = 1 TO NFile
    a = a + 1
    c = c + 1
    IF a > 18 THEN
        a = 1
        b = b + 40
    END IF
    LOCATE 2 + a, 5 + b
    PRINT LTRIM$(RTRIM$(STR$(c))) + "- Agenda" + LTRIM$(RTRIM$(STR$(c))) + ".age"
NEXT
LOCATE 4 + 18, 5
INPUT "File da eliminare"; File
IF File = 0 THEN RUN
OPEN "Lista.age" FOR OUTPUT AS #1
PRINT #1, NFile - 1
CLOSE #1
File$ = "Agenda" + LTRIM$(RTRIM$(STR$(File))) + ".age"
KILL File$
a = File
    FOR x = 1 TO NFile - File
        IF NFile <> File THEN
            a = a + 1
            NAME "Agenda" + LTRIM$(RTRIM$(STR$(a))) + ".age" AS "Agenda" + LTRIM$(RTRIM$(STR$(a - 1))) + ".age"
        END IF
    NEXT
RUN
END SUB

SUB Nuovo
DIM p$(21)
DIM p(21)
LOCATE 1
PRINT "   [ Nuovo ]      ESC=Esci      Ctrl+A=Indietro   Ctrl+B=Passwrod             "
OPEN "Lista.age" FOR INPUT AS #1
INPUT #1, NFile
CLOSE #1
File$ = "Agenda" + LTRIM$(RTRIM$(STR$(NFile + 1))) + ".age"
COLOR 7, 1
LOCATE 3, 30
PRINT "Nome file:"; File$
Immissione:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
    CASE CHR$(27)
        STOP
    CASE CHR$(1)
        RUN
    CASE CHR$(2)
        GOSUB Password
    CASE ELSE
        GOSUB Scritto
    END SELECT
    GOSUB Immissione

Scritto:
    COLOR 15, 1
    LOCATE 6, 5
    PRINT "Nome:"
    LOCATE 8, 5
    PRINT "Cognome:"
    LOCATE 10, 5
    PRINT "Telefono:"
    LOCATE 12, 5
    PRINT "Cellulare/Ufficio:"
    LOCATE 14, 5
    PRINT "Via:"
    LOCATE 16, 5
    PRINT "Numero civico:"
    LOCATE 18, 5
    PRINT "Note:"
    LOCATE 6, 10
    INPUT Nome$
    LOCATE 6, 10
    PRINT Nome$ + STRING$(30, " ")
    LOCATE 8, 13
    INPUT Cognome$
    LOCATE 8, 13
    PRINT Cognome$ + STRING$(30, " ")
    LOCATE 10, 14
    INPUT Telefono$
    LOCATE 10, 14
    PRINT Telefono$ + STRING$(30, " ")
    LOCATE 12, 23
    INPUT Cell$
    LOCATE 12, 23
    PRINT Cell$ + STRING$(30, " ")
    LOCATE 14, 9
    INPUT Via$
    LOCATE 14, 9
    PRINT Via$ + STRING$(30, " ")
    LOCATE 16, 19
    INPUT NC$
    LOCATE 16, 19
    PRINT NC$ + STRING$(30, " ")
    LOCATE 18, 10
    INPUT Note$
    LOCATE 18, 10
    PRINT Note$ + STRING$(30, " ")

Salva:
    IF RicPw = 1 THEN GOTO Criptx
    OPEN File$ FOR OUTPUT AS #1
    PRINT #1, Nome$
    PRINT #1, Cognome$
    PRINT #1, Telefono$
    PRINT #1, Cell$
    PRINT #1, Via$
    PRINT #1, NC$
    IF Note$ = "" THEN Note$ = "Nessune"
    PRINT #1, Note$
    PRINT #1, Password$
    CLOSE #1
    OPEN "lista.age" FOR INPUT AS #1
        INPUT #1, NFile
    CLOSE #1
    OPEN "lista.age" FOR OUTPUT AS #1
        PRINT #1, NFile + 1
    CLOSE #1
    RUN

Password:
    LOCATE 20, 8
    COLOR 14, 1
    INPUT "Password "; Pass$
    FOR x = 1 TO LEN(Pass$)
        a = a + 1
        p$(a) = RIGHT$(LEFT$(Pass$, a), 1)
        p(a) = ASC(p$(a)) - 54
    NEXT
    a = 0
    FOR x = 1 TO UBOUND(p$)
        a = a + 1
        IF p(a) <> 0 THEN
            Password$ = Password$ + LTRIM$(RTRIM$(STR$(p(a))))
        END IF
    NEXT
    IF Nome$ = "" THEN
        COLOR 7, 1
        RicPw = 1
        GOTO Scritto
    ELSE
Criptx:
    IF Cell$ = "" THEN Cell$ = "Nessuno"
    IF Note$ = "" THEN Note$ = "Nessune"
    a = 0
    FOR x = 1 TO LEN(Nome$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Nome$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Nome$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(Cognome$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Cognome$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Cognome$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(Telefono$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Telefono$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Telefono$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(Cell$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Cell$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Cell$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(Via$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Via$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Via$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(NC$)
        a = a + 1
        a$ = RIGHT$(LEFT$(NC$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    NC$ = a1$
    a1$ = ""
    a = 0
    FOR x = 1 TO LEN(Note$)
        a = a + 1
        a$ = RIGHT$(LEFT$(Note$, a), 1)
        a1$ = a1$ + LTRIM$(RTRIM$(STR$(((ASC(a$) - 23)))))
    NEXT
    Note$ = a1$
    a1$ = ""
        RicPw = 0
        GOTO Salva
    END IF
END SUB

SUB Ricerca
DIM NumeroFile(100)
DIM Pwd$(50)
DIM Pwr$(50)
COLOR 4, 7
LOCATE 1
PRINT "    [ Ricerca ]    Invio=Avvia    ESC=Esci                              "
Tasti$ = ""
WHILE Tasti$ = ""
    Tasti$ = INKEY$
WEND
SELECT CASE Tasti$
    CASE CHR$(27)
        RUN
    CASE CHR$(13)
        GOTO MRicerca
    CASE ELSE
        BEEP
        CALL Ricerca
END SELECT

MRicerca:
    COLOR 7, 1
    LOCATE 4, 5
    INPUT "Nome/cognome da cercare "; Criterio$
    OPEN "Lista.age" FOR INPUT AS #1
    INPUT #1, NFile
    Tempo = (100 / 2) / NFile + 1
    IF Tempo MOD 2 <> 0 THEN Tempo = Tempo - 1
    CLOSE #1
    FOR x = 1 TO NFile
        a = a + 1
        File$ = "Agenda" + LTRIM$(RTRIM$(STR$(a))) + ".age"
        OPEN File$ FOR INPUT AS #1
        INPUT #1, Nome$
        INPUT #1, Cognome$
        aa = 0
        FOR y = 1 TO LEN(Criterio$)
            aa = aa + 1
            aa$ = RIGHT$(LEFT$(Criterio$, aa), 1)
            aa1$ = aa1$ + LTRIM$(RTRIM$(STR$(((ASC(aa$) - 23)))))
        NEXT
        Criterio1$ = aa1$
        aa1$ = ""
        IF UCASE$(Criterio$) = UCASE$(Nome$) OR UCASE$(Criterio$) = UCASE$(Cognome$) OR UCASE$(Criterio$) = LEFT$(UCASE$(Nome$), 1) OR UCASE$(Criterio$) = LEFT$(UCASE$(Cognome$), 1) OR UCASE$(Nome$) = UCASE$(Criterio1$) OR UCASE$(Cognome$) = UCASE$(Criterio1$) OR UCASE$(Criterio1$) = LEFT$(UCASE$(Nome$), 2) OR LEFT$(UCASE$(Cognome$), 2) = UCASE$(Criterio1$) THEN
           b = b + 1
           NumeroFile(b) = a
        END IF
    CLOSE #1
    NEXT
    LOCATE 4
    PRINT STRING$(50, " ")
    a = 0
    b = 0
    c = 0
    d = 0
    e = 0
    FOR x = 1 TO NFile
        a = a + 1
        COLOR 1, 7
        LOCATE 23, (Tempo + 1 * (a - 1))
        PRINT STRING$(Tempo, "þ")
        LOCATE 20
        COLOR 7, 1
        IF NumeroFile(a) <> 0 THEN
            File$ = "Agenda" + LTRIM$(RTRIM$(STR$(NumeroFile(a)))) + ".age"
            b = b + 1
            e = e + 1
            c = NumeroFile(a)
            IF e > 18 THEN
                d = 40
                e = 1
            END IF
            OPEN File$ FOR INPUT AS #1
            INPUT #1, Nome$
            INPUT #1, Cognome$
            INPUT #1, Telefono$
            INPUT #1, Cell$
            INPUT #1, Via$, NC$
            INPUT #1, Note$
            INPUT #1, Pass$
            CLOSE #1
            LOCATE 2 + e, 5 + d
            IF Pass$ = "" THEN
                PRINT "Nome"; c; ":"; Nome$ + " " + Cognome$;
            ELSE
                PRINT "Nome"; c; ":****** ******"
            END IF
        END IF
        IF c = 0 THEN
            COLOR 14, 1
            LOCATE 4, 14
            PRINT "Non Š stata trovata alcuna voce con il nome specificato"
            COLOR 7, 1
            SLEEP 10
            LOCATE 4
            PRINT STRING$(70, " ")
            CALL Ricerca
       END IF
    NEXT
    a = 0
    LOCATE 21, 5
    INPUT "Numero del Nome "; NumFile
    COLOR 7, 1
    LOCATE 21
    PRINT STRING$(60, " ")
    GOSUB Apri
    SLEEP 60
    RUN

Apri:
    a = 0
    FOR x = 1 TO 20
        a = a + 1
        LOCATE 1 + a
        PRINT STRING$(80, " ")
    NEXT
    File$ = "Agenda" + LTRIM$(RTRIM$(STR$(NumFile))) + ".age"
    OPEN File$ FOR INPUT AS #1
    INPUT #1, Nome$
    INPUT #1, Cognome$
    INPUT #1, Telefono$
    INPUT #1, Cell$
    INPUT #1, Via$, NC$
    INPUT #1, Note$
    INPUT #1, Pass$
    CLOSE #1
    IF Pass$ <> "" THEN
        Ric = 1
        GOSUB Pass
    END IF
1   LOCATE 4
    PRINT STRING$(50, " ")
    COLOR 15, 1
    LOCATE 6, 5
    PRINT "Nome:"; Nome$
    LOCATE 8, 5
    PRINT "Cognome:"; Cognome$
    LOCATE 10, 5
    PRINT "Telefono:"; Telefono$
    LOCATE 12, 5
    PRINT "Cellulare/Ufficio:"; Cell$
    LOCATE 14, 5
    PRINT "Via:"; Via$
    LOCATE 16, 5
    PRINT "Numero civico:"; NC$
    LOCATE 18, 5
    PRINT "Note:"; Note$
    SLEEP 10
    LOCATE 23
    COLOR 1, 7
    PRINT STRING$(50, " ")
    COLOR 7, 1
    RETURN

Pass:
    LOCATE 20, 5
    INPUT "Password "; Pass2$
    a = 0
    FOR x = 1 TO LEN(Pass$) / 2
        a = a + 2
        Pwd$(a) = RIGHT$(LEFT$(Pass$, a), 2)
    NEXT
    a = 0
    FOR x = 1 TO LEN(Pass$) / 2
        a = a + 2
        Pwr$(a) = CHR$(VAL(Pwd$(a)) + 54)
        Password$ = Password$ + Pwr$(a)
    NEXT
    PRINT ""
    IF Pass2$ <> Password$ THEN
        LOCATE 21, 5
        COLOR 12, 1
        PRINT "Password errata"
        COLOR 7, 1
        Pass2$ = ""
        Password$ = ""
        GOSUB Pass
    ELSE
        LOCATE 20
        PRINT STRING$(50, " ")
        LOCATE 21
        PRINT STRING$(50, " ")
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Nome$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Nome$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Nome$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Cognome$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Cognome$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Cognome$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Telefono$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Telefono$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Telefono$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Cell$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Cell$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Cell$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Via$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Via$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Via$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(NC$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(NC$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        NC$ = a$
        a = 0
        a$ = ""
        FOR x = 1 TO LEN(Note$) / 2
            a = a + 2
            NPwd = VAL(RIGHT$(LEFT$(Note$, a), 2)) + 23
            a$ = a$ + CHR$(NPwd)
        NEXT
        Note$ = a$
        GOTO 1
    END IF
END SUB

