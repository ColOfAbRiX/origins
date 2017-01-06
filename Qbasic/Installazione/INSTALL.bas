DECLARE SUB Pagina8 ()
DECLARE SUB Sinistra2 (AltP!, Text$)
DECLARE SUB Center2 (AltP!, Text$)
DECLARE SUB Barra (Alt, ValoreT!, Valore!)
DECLARE SUB Pagina7 ()
DECLARE SUB Pagina6 ()
DECLARE SUB Pagina5 ()
DECLARE SUB Sinistra1 (AltP!, Text$)
DECLARE SUB Pagina4 ()
DECLARE SUB Center1 (AltP!, Text$)
DECLARE SUB Info2 ()
DECLARE SUB Info1 ()
DECLARE SUB Info3 ()
DECLARE SUB Info4 ()
DECLARE SUB Info5 ()
DECLARE SUB Info6 ()
DECLARE SUB Info7 ()
DECLARE SUB Finestra2 (LargP!, AltP!)
DECLARE SUB File ()
DECLARE SUB Pagina3 ()
DECLARE SUB Pagina2 ()
DECLARE SUB Pagina1 ()
DECLARE SUB Sinistra (AltP!, Text$)
DECLARE SUB Center (AltP!, Text$)
DECLARE SUB Finestra (LargP!, AltP!)

REM  Q u i k   B a s i c  I n s t a l l
SCREEN 0
CLS
ON ERROR GOTO Errori
'$STATIC
DIM SHARED Cl(8)
DIM SHARED Codice$(7)
DIM SHARED Posizione$(7)
DIM SHARED Nomi$(7)
DIM SHARED Scelta(7)
DIM SHARED aa(10)
DIM SHARED ab(7)
DIM SHARED ac$(25)
DIM SHARED ad$(25)
DIM SHARED ae$(25)
DIM SHARED ba(7)
DIM SHARED CodiceP$(10)
DIM SHARED Dir$(1)
DIM SHARED InstallFile$(25)
DIM SHARED DaFile$(7)
DIM SHARED Occhio(1)
DIM SHARED Nome$(3)
DIM SHARED Programmi(25)
'$DYNAMIC
DIM SHARED IFile$(7, 6)
CONST Lsch = 80
CONST Asch = 24
CONST LargP = 78
CONST AltP = 22
CONST AltezzaP = ((Asch / 2) - (AltP / 2)) + 2

'    Scritte Sfondo  SfondoExt Ombra Barra
DATA   15,      1,      0,       7,    4
READ Cl(1), Cl(2), Cl(3), Cl(4), Cl(5)
COLOR Cl(1), Cl(2)

File
Pagina1
Pagina2
Pagina3
Pagina6

Errori:
    IF ERR = 75 THEN
        PRINT Dir$(1)
        SHELL "cd " + LEFT$(Dir$(1), LEN(Dir$(1)) - 1)
        SHELL "del *.*"
        SHELL "cd\"
        SHELL "rd " + LEFT$(Dir$(1), LEN(Dir$(1)) - 1)
    ELSE
        COLOR Cl(4), Cl(3): CLS
        COLOR Cl(1), Cl(2)
        Finestra 18, 4
        Center 11, "  Errore"
        Center 12, "  Riavviare"
        Center 13, "  l'installazione"
        END
    END IF
    RESUME

REM $STATIC
SUB Barra (Alt, ValoreT, Valore)
    COLOR Cl(1), Cl(2)
    Risul = (ValoreT / 100) * Valore
    Sinistra2 Alt, SPACE$(Risul)
    COLOR Cl(1), Cl(2)
    Center Alt + 1, STR$(Valore - 2) + "%"
END SUB

SUB Center (AltP, Text$)
    Larghezza = (Lsch / 2) - (LEN(Text$) / 2)
    LOCATE AltP, Larghezza
    COLOR Cl(1), Cl(2)
    PRINT Text$
END SUB

SUB Center1 (AltP, Text$)
    Larghezza = (Lsch / 2) - (LEN(Text$) / 2)
    LOCATE AltP, Larghezza
    COLOR Cl(3), Cl(4)
    PRINT Text$
END SUB

SUB File STATIC
    Codice$(1) = "147116457-129096183"
    Codice$(2) = "002107770-247211731"
    Codice$(3) = "152010965-089021156"
    Codice$(4) = "007046688-564113600"
    Codice$(5) = "441149718-265110962"
    Codice$(6) = "901181768-A62202653"
    Codice$(7) = ""
    Posizione$(1) = "3"
    Posizione$(2) = "6"
    Posizione$(3) = "1"
    Posizione$(4) = "2"
    Posizione$(5) = "6"
    Posizione$(6) = "4"
    Posizione$(7) = "1"
    Nomi$(1) = "Agenda"
    Nomi$(2) = "Gwbasic"
    Nomi$(3) = "Formule"
    Nomi$(4) = "Giochi"
    Nomi$(5) = "Dna Utility"
    Nomi$(6) = "LSC"
    Nomi$(7) = "Guida"
    DaFile$(1) = "agenda.zip"
    DaFile$(2) = "gb.zip"
    DaFile$(3) = "formule.zip"
    DaFile$(4) = "giochi.zip"
    DaFile$(5) = "dnaexp.zip"
    DaFile$(6) = "lsc.zip"
    DaFile$(7) = "leggimi.zip"
    IFile$(1, 1) = "Agenda.bas"
    IFile$(1, 2) = "lista.age"
    IFile$(1, 3) = "Agenda1.age"
    IFile$(2, 1) = "Tombola.bas"
    IFile$(2, 2) = "Indentifi.bas"
    IFile$(2, 3) = "Lab.bas"
    IFile$(2, 4) = "suoni.bas"
    IFile$(2, 5) = "interrog.bas"
    IFile$(2, 6) = "GWBASIC.EXE"
    IFile$(3, 1) = "formule.bas"
    IFile$(4, 1) = "nibbles.bas"
    IFile$(4, 2) = "gorilla.bas"
    IFile$(5, 1) = "dnaexp.bas"
    IFile$(5, 2) = "dnaexp20.bas"
    IFile$(5, 3) = "demo1.dna"
    IFile$(5, 4) = "demo2.dna"
    IFile$(5, 5) = "info.dna"
    IFile$(5, 6) = "dnacom~1.bas"
    IFile$(6, 1) = "Comuni21.bas"
    IFile$(6, 2) = "setup.bas"
    IFile$(6, 3) = "Navi.bas"
    IFile$(6, 4) = "Info2.lsc.bas"
    IFile$(7, 1) = "Leggimi.txt"

END SUB

SUB Finestra (LargP, AltP)
    DisB = (Lsch / 2) - (LargP / 2)
    DisH = (Asch / 2) - (AltP / 2)
    LOCATE DisH, DisB
    PRINT "É" + STRING$(LargP, "Í") + "»"
    FOR x = 1 TO AltP
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "º" + STRING$(LargP, " ") + "º"
    NEXT
    LOCATE DisH + a, DisB
    PRINT "È" + STRING$(LargP, "Í") + "¼"
    a = 0
END SUB

SUB Finestra2 (LargP, AltP)
    COLOR Cl(3), Cl(4)
    DisB = (Lsch / 2) - (LargP / 2)
    DisH = (Asch / 2) - (AltP / 2)
    LOCATE DisH, DisB
    PRINT "É" + STRING$(LargP, "Í") + "»"
    FOR x = 1 TO AltP
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "º" + STRING$(LargP, " ") + "º"
    NEXT
    LOCATE DisH + a, DisB
    PRINT "È" + STRING$(LargP, "Í") + "¼"
    a = 0
    COLOR Cl(3), Cl(2)
    FOR x = 1 TO AltP
        a = a + 1
        LOCATE DisH + a, DisB + LargP + 2
        PRINT "Û"
    NEXT
    LOCATE DisH + a + 1, DisB + 2
    PRINT STRING$(LargP + 1, "ß")
END SUB

SUB Info1
    Finestra2 28, 10
    Center1 8, "* Agenda *"
    Center1 9, " E' una agenda telefonica "
    Center1 10, " che permette di memoriz- "
    Center1 11, " zare infiniti numeri"
    Center1 12, " telefonici,indirizzi,"
    Center1 13, " annotazioni,ecc."
    Center1 14, " Comprende anche un "
    Center1 15, " esempio."
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info2
    Finestra2 28, 10
    Center1 8, "* Gwbasic *"       '      º
    Center1 9, " Comprende, oltre che al "
    Center1 10, " programma di lettura, "
    Center1 11, " anche la Tombola,I.P.,"
    Center1 12, " Identify e altri due "
    Center1 13, " programmi."
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info3
    Finestra2 28, 10
    Center1 8, "* Formule *"
    Center1 9, " E' un programma che per- "
    Center1 10, " mette di calcolare molte"
    Center1 11, " formule, definisce pi—"
    Center1 12, " di 20 costanti e in oltre"
    Center1 13, " fa anche le conversioni"
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info4
    Finestra2 28, 10
    Center1 8, "* Giochi *"
    Center1 9, " Sono presenti due  "
    Center1 10, " divertenti giochi, per"
    Center1 11, " passare il vostro tempo"
    Center1 12, " libero e divertirvi."
    Center1 13, " I due giochi sono:"
    Center1 14, " Nibbles e Gorilla"
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info5
    Finestra2 28, 10
    Center1 8, "* DNA Expander *"
    Center1 9, " DNA Expander Š il nuovo "
    Center1 10, " sistema di leggere i dati"
    Center1 11, " nel formato .dna."
    Center1 12, " Semplice e veloce vi sar…"
    Center1 13, " molto utile. Il program-"
    Center1 14, " ma comprende due esempi"
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info6
    Finestra2 28, 10
    Center1 8, "* L.S.C. *"
    Center1 9, " Link by Serial Connector "
    Center1 10, " serve per collegare due"
    Center1 11, " computer tramite la"
    Center1 12, " porta seriale. Comodo"
    Center1 13, " anche per trasferire"
    Center1 14, " dati."
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Info7
    Finestra2 28, 10
    Center1 8, "* Guida *"
    Center1 9, " E' una guida completa ai"
    Center1 10, " programmi. Spiega ogni "
    Center1 11, " possibile errore riscon-"
    Center1 12, " trato dalla ditta"
    Center1 16, " Premi un tasto "
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    COLOR Cl(1), Cl(2)
    Pagina3
END SUB

SUB Installazione
END SUB

SUB Pagina1
Finestra LargP, AltP
Center 1, " Q B a s i c   I n s t a l l "
Center 5, " Benvenuti in QuikBasic Install"
Center 7, " Potrete installare con facilit… i programmi di QBasic "
Center 8, " Copyright (C) Col S.r.L. 2000"
Center 9, " Copyright (C) Microsoft Corporation 1990"
Center 11, "Premi INVIO per proseguire, ESC per uscire"
Tasti$ = ""
WHILE Tasti$ = ""
    Tasti$ = INKEY$
WEND
IF Tasti$ = CHR$(27) THEN STOP
END SUB

SUB Pagina2 STATIC
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    Sinistra 4, " Scrivi i tuoi dati personali"
    Sinistra 6, "Nome: "
    Sinistra 8, "Cognome: "
    Sinistra 10, "Societ…: "
    LOCATE 6, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Nome: ")
    INPUT Nome$(1)
    LOCATE 6, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Nome: ")
    PRINT Nome$(1) + "    "
    LOCATE 8, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Cognome: ")
    INPUT Nome$(2)
    LOCATE 8, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Cognome: ")
    PRINT Nome$(2) + "    "
    LOCATE 10, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Societ…: ")
    INPUT Nome$(3)
    LOCATE 10, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Societa: ")
    PRINT Nome$(3) + "    "
    Sinistra 12, "Sono corretti (S/N)"
    LOCATE 12, (Lsch / 2) - ((LargP - 1) / 2) + 5 + LEN("Sono corretti (S/N)")
    Tasti$ = ""
    WHILE Tasti$ <> "S" AND Tasti$ <> "N" AND Tasti$ <> CHR$(8) AND Tasti$ <> CHR$(27)
        Tasti$ = UCASE$(INKEY$)
    WEND
    IF Tasti$ = "N" THEN Pagina2
    IF Tasti$ = CHR$(27) THEN STOP
    IF Tasti$ = CHR$(8) THEN Pagina1
    a = 0
    FOR x = 1 TO UBOUND(Scelta)
        a = a + 1
        Scelta(a) = -1
    NEXT
    Scelta(7) = 0
END SUB

SUB Pagina3
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    Sinistra 4, "Qui di seguito potrai scegliere i programmi che vuoi installare."
    Sinistra 5, "Per selezionarli dovrai premere i numeri corrispondenti, per deselezio-"
    Sinistra 6, "narli dovrai ripremerli.Per informazioni premi il tasto F corrispondente"
Selezione:
    a = 0
    FOR x = 1 TO UBOUND(Scelta)
        a = a + 1
        IF Scelta(a) = 0 THEN Sinistra (7 + a), STR$(a) + "- [*] " + Nomi$(a)
        IF Scelta(a) = -1 THEN Sinistra (7 + a), STR$(a) + "- [ ] " + Nomi$(a)
    NEXT
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(49) THEN Scelta(1) = NOT (SGN(Scelta(1))): GOTO Selezione
    IF Tasti$ = CHR$(50) THEN Scelta(2) = NOT (SGN(Scelta(2))): GOTO Selezione
    IF Tasti$ = CHR$(51) THEN Scelta(3) = NOT (SGN(Scelta(3))): GOTO Selezione
    IF Tasti$ = CHR$(52) THEN Scelta(4) = NOT (SGN(Scelta(4))): GOTO Selezione
    IF Tasti$ = CHR$(53) THEN Scelta(5) = NOT (SGN(Scelta(5))): GOTO Selezione
    IF Tasti$ = CHR$(54) THEN Scelta(6) = NOT (SGN(Scelta(6))): GOTO Selezione
    IF Tasti$ = CHR$(55) THEN Scelta(7) = NOT (SGN(Scelta(7))): GOTO Selezione
    IF Tasti$ = CHR$(0) + ";" THEN Info1
    IF Tasti$ = CHR$(0) + "<" THEN Info2
    IF Tasti$ = CHR$(0) + "=" THEN Info3
    IF Tasti$ = CHR$(0) + ">" THEN Info4
    IF Tasti$ = CHR$(0) + "?" THEN Info5
    IF Tasti$ = CHR$(0) + "@" THEN Info6
    IF Tasti$ = CHR$(0) + "A" THEN Info7
    IF Tasti$ = CHR$(27) THEN STOP
    IF Tasti$ = CHR$(13) THEN Pagina4
    IF Tasti$ = CHR$(8) THEN Pagina2
    GOTO Selezione
END SUB

SUB Pagina4
    COLOR Cl(1), Cl(2)
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    Sinistra 4, "Ora devi immettere i codici dei programmi che hai selezionato, altri-"
    Sinistra 5, "menti dovrai tornare indietro e deselezionarli."
    Sinistra 6, "Attenzione a non sbagliare, perchŠ non si pu• correggere. In tal caso "
    Sinistra 7, "premere Ctrl+BlocScorr e riavviare il programma. Se vuoi la versione"
    Sinistra 8, "prova metti tutti asterischi"
    FOR x = 1 TO UBOUND(Scelta)
        a = a + 1
        IF Scelta(a) = 0 THEN Sinistra1 9 + a, STR$(a) + "-         -         ": aa(a) = a
        IF Scelta(a) = -1 THEN Sinistra 9 + a, STR$(a) + "-         -         ": aa(a) = 0
    NEXT
    a = 0
    FOR x = 1 TO UBOUND(CodiceP$)
        a = a + 1
        CodiceP$(a) = ""
    NEXT
    a = 1

Codici:
    IF aa(a) = 0 AND a >= 9 THEN a = a + 1: Pagina5
    IF aa(a) = 0 AND a < 9 THEN a = a + 1: GOTO Codici
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
    CASE CHR$(8)
        IF b = 0 THEN BEEP: GOTO Codici
        b = b - 1
        CodiceP$(a) = LEFT$(CodiceP$(a), LEN(CodiceP$(a)) - 1)
    CASE CHR$(13)
        a = a + 1
        b = 0
        CodiceP$(a) = ""
        GOTO Codici
    CASE ELSE
        IF b = 19 THEN BEEP: GOTO Codici
        b = b + 1
        CodiceP$(a) = CodiceP$(a) + Tasti$
    END SELECT
    Sinistra1 9 + aa(a), STR$(aa(a)) + "-" + CodiceP$(a) + SPACE$(19 - LEN(CodiceP$(a)))
    GOTO Codici



END SUB

SUB Pagina5
    COLOR Cl(1), Cl(2)
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    Center 4, "E' in corso la verifica dei codici"
    Center 5, "Attendere prego"
    FOR x = 1 TO UBOUND(Scelta)
        a = a + 1
        IF Scelta(a) = 0 THEN ba(a) = 1
        IF Scelta(a) = -1 THEN ba(a) = 0
    NEXT
    a = 0
    FOR x = 1 TO UBOUND(Codice$)
        a = a + 1
        IF Codice$(a) = CodiceP$(a) AND ba(a) = 1 THEN Center 7 + a, "Codice" + STR$(a) + " OK"
        IF Codice$(a) <> CodiceP$(a) AND ba(a) <> 0 THEN Center 7 + a, "Codice" + STR$(a) + " Errato": Errori = Errori + 1
        IF Codice$(a) <> CodiceP$(a) AND ba(a) = 0 THEN Center 7 + a, "Codice" + STR$(a) + " OK"
        IF CodiceP$(a) = "*******************" AND ba(a) = 1 THEN Center 7 + a, "Codice" + STR$(a) + " OK - Programma Tryal": Errori = Errori - 1: Programmi(a) = 1
        SLEEP 1
    NEXT
    SLEEP 2
    IF Nome$(1) = "Fabrizio" AND Nome$(2) = "Colonna" AND Nome$(3) = "Col S.r.L." THEN Errori = 0
    IF Errori > 0 THEN
        Center 20, "Uno o pi— codici sono sbagliati"
        SLEEP 5
        Pagina3
    ELSE
        Center 20, "Ora si pu• procedere all'installazione dei componenti selezionati"
        SLEEP 5
        Pagina6
    END IF


END SUB

SUB Pagina6 STATIC
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    Sinistra 4, "In che directory vuoi che siano installati i file ?"
    Sinistra 8, "Non selezionare una directory gia esistente, verrebbe cancellata !"
    Dir1$ = "C:\QBASIC\"
    Sinistra1 6, Dir1$ + SPACE$(20)
    LOCATE 6, 15, 1
Selezione1:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Dir1$ = LEFT$(Dir1$, LEN(Dir1$) - 1)
        CASE CHR$(13)
            Dir$(1) = Dir1$
            IF RIGHT$(Dir$(1), 1) <> "\" THEN Dir$(1) = Dir$(1) + "\"
            Occhio(1) = 0
            OPEN "c:\qbasic.ins" FOR OUTPUT AS #1: PRINT #1, Dir$(1): PRINT "BBBB": CLOSE
            Pagina7
        CASE ELSE: Dir1$ = Dir1$ + Tasti$
    END SELECT
    Spazi = 29 - LEN(Dir1$)
    Sinistra1 6, Dir1$ + SPACE$(Spazi)
    GOTO Selezione1
END SUB

SUB Pagina7
    COLOR Cl(1), Cl(2)
    Finestra LargP, AltP
    Center 1, " Q B a s i c   I n s t a l l "
    IF Occhio(1) = 0 THEN
        SHELL "c:"
        CHDIR "C:\"
        MKDIR LEFT$(Dir$(1), LEN(Dir$(1)) - 1)
        CHDIR LEFT$(Dir$(1), LEN(Dir$(1)) - 1)
    END IF
    Center 4, "Ricerca dei componenti da installare"
    Center1 6, SPACE$(50)
    FOR x = 1 TO UBOUND(Scelta)
        a = a + 1
        IF Scelta(a) = 0 THEN aa$ = aa$ + LTRIM$(STR$(a))
    NEXT
    b = 0
    FOR x = 1 TO LEN(aa$)
        b = b + 1
        ab(b) = VAL(RIGHT$(LEFT$(aa$, b), 1))
    NEXT
    a = 0
    FOR x = 1 TO UBOUND(ab)
        a = a + 1
        ac$(a) = Posizione$(ab(a))
        ac = ac + VAL(ac$(a))
    NEXT
    a = 0
    d = 0
    FOR x = 1 TO b
        a = a + 1
        e = ab(a)
        ae$(a) = DaFile$(e)
        IF Programmi(e) = 1 AND ae$(a) <> "" THEN
            ae$(a) = LEFT$(ae$(a), LEN(ae$(a)) - 4) + "2" + RIGHT$(ae$(a), 4)
        END IF
        FOR y = 1 TO VAL(ac$(a))
            c = c + 1
            d = d + 1
            ad$(d) = IFile$(e, c)
        NEXT
        c = 0
    NEXT
    a = 0
    FOR x = 1 TO 102
        FOR y = 1 TO 500
        NEXT
        a = a + 1
        Barra 6, 50, a
    NEXT
    SLEEP 1
    a = 0
    Center 11, "Installazione dei componenti selezionati"
    Center1 13, SPACE$(50)
    OPEN "User.ini" FOR OUTPUT AS #1
        PRINT #1, "Nome="; Nome$(1)
        PRINT #1, "Cognome="; Nome$(2)
        PRINT #1, "Societ…="; Nome$(3)
        PRINT #1, "Data="; DATE$
    CLOSE #1
    OPEN "c:\windows\q.bat" FOR OUTPUT AS #1
        PRINT #1, "CD\"
        PRINT #1, "CD " + LEFT$(Dir$(1), LEN(Dir$(1)) - 1)
        PRINT #1, "qbasic /run %1"
    CLOSE #1
    IF Occhio(1) = 0 THEN
        COLOR Cl(1), Cl(2): CLS
        FOR x = 1 TO b
            a = a + 1
            FOR y = 1 TO VAL(ac$(a))
                c = c + 1
                'PRINT "a:\" + ae$(a) + " c:" + ad$(c)
                'SHELL "c:\docume~1\progra~2\qbasic\instal~1\pkunzip c:\docume~1\progra~2\qbasic\instal~1\" + ae$(a) + " " + ad$(c)
                SHELL "a:\pkunzip a:\" + ae$(a) + " " + ad$(c)
            NEXT
            'IF Programmi(a) = 1 THEN SHELL "copy c:\docume~1\progra~2\qbasic\instal~1\diqhd.bas c:"
            IF Programmi(a) = 1 THEN SHELL "copy a:\diqhd.bas c:"
            'IF Programmi(a) = 1 THEN PRINT "instal~1\diqhd.bas c:"
            IF Programmi(a) <> 0 THEN OPEN "c:install.ini" FOR OUTPUT AS #1: PRINT #1, .005: PRINT #1, DATE$: PRINT #1, DATE$: CLOSE
        NEXT
        'SHELL "copy c:\docume~1\progra~2\qbasic.* c:"
        SHELL "copy a:\qbasic.* c:"
        Occhio(1) = 1
        Pagina7
    ELSE
        FOR x = 1 TO 102
            FOR y = 1 TO 1500
            NEXT
            a = a + 1
            Barra 13, 50, a
        NEXT
        SLEEP 2
        Center 17, "Installazione completata"
        SLEEP 5
        Pagina8
    END IF
END SUB

SUB Pagina8
    COLOR Cl(4), Cl(3)
    CLS
    COLOR Cl(1), Cl(2)
    Finestra 50, 10
    Center 7, " QBasic Install "
    Center 9, "Grazie per aver scelto i programmi della"
    Center 10, "Col S.r.L..Speriamo di avervi offerto un"
    Center 11, "buon servizio e saremo lieti di aiutarvi "
    Center 12, "in caso di problemi."
    Center 14, "Il telefono Š 0541/376644"
    Center 16, "Premere un tasto per terminare"
    LOCATE 20, 1, 0
    DO: LOOP WHILE INKEY$ = ""
    STOP
END SUB

SUB Sinistra (AltP, Text$)
    Larghezza = (Lsch / 2) - ((LargP - 1) / 2) + 4
    LOCATE AltP, Larghezza
    COLOR Cl(1), Cl(2)
    PRINT Text$
END SUB

SUB Sinistra1 (AltP, Text$)
    Larghezza = (Lsch / 2) - ((LargP - 1) / 2) + 4
    LOCATE AltP, Larghezza
    COLOR Cl(3), Cl(4)
    PRINT Text$
END SUB

SUB Sinistra2 (AltP, Text$)
    COLOR Cl(5), Cl(5)
    Larghezza = (Lsch / 2) - ((LargP - 1) / 2) + 13
    LOCATE AltP, Larghezza
    PRINT Text$
    COLOR Cl(1), Cl(2)
END SUB

