DECLARE SUB Disinst ()
DECLARE SUB Speciale ()
DECLARE SUB Giochi ()
DECLARE SUB Errore ()
DECLARE SUB Guida ()
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
ON ERROR GOTO Errori
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
DIM SHARED f$(15)
DIM SHARED SubW$(1)
DIM SHARED Special(10)
DIM SHARED Compiler$(10)
'$DYNAMIC
DIM SHARED COl(9, 2)
DIM SHARED TipMenu$(3, 6)
 'Costanti
CONST Info$ = "Info2.lsc"
CONST Soft$ = "0001"
     'Titolo  Men—  Pagina  Stato  Errore  Attivo  MSel  Punto Guida
Colorato:
DATA   4,7,   0,3,   7,1,   15,1,   14,4,  15,1,   14,3,  12,7, 10,7
BlackW:
DATA   0,7,   7,0,   7,0,    7,0,   15,0,  15,0,   15,0,   0,7, 15,7
Special:
DATA  13,0,  12,0,   1,0,   11,0,   14,4,  10,0,   14,0,  12,0, 10,7
Compiler$(7) = "Diritti riservati"
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

Errori:
    COLOR COl(5, 1), COl(5, 2)
    Finestra 6, 36, "Errore"
    COLOR COl(5, 1), COl(5, 2)
    a$ = "Errore N." + STR$(ERR)
    SELECT CASE ERR
        CASE 5
            Center 10, "Chiamata di funzione non valida", 32
        CASE 24
            Center 10, "Timeout di periferica", 22
            b$ = "Periferica:" + ERDEV$
        CASE 25
            Center 10, "Errore di periferica", 20
            b$ = "Periferica:" + ERDEV$
        CASE 52
            Center 10, "Nome del file errato", 20
        CASE 53
            Center 10, "File non trovato", 16
        CASE 57
            Center 10, "Errore di I/O di periferica", 28
            b$ = "Periferica:" + ERDEV$
        CASE 61
            Center 10, "Disco pieno", 10
        CASE 64
            Center 10, "Nome del file errato", 20
        CASE 68
            Center 10, "Periferica non disponibile", 26
            b$ = "Periferica: " + ERDEV$
        CASE 69
            Center 10, "Owerflow buffer comunicazioni", 30
        CASE 70
            Center 10, "Permesso negato", 14
        CASE 71
            Center 10, "Disco non pronto", 16
        CASE 72
            Center 10, "Errore supporto del disco", 24
        CASE 76
            Center 10, "Percorso non trovato", 20
        CASE ELSE
            Center 10, "Errore non previsto", 18
    END SELECT
    Center 9, a$, 13
    Center 11, b$, 20
    Center 12, "Riavvio del programma...", 24
    FOR x = 1 TO 100000 * VAL(Info$(3)): NEXT
    RUN

REM $STATIC
SUB Center (Alt, Text$, Fin)
    DisB = 40 - Fin / 2 + 1
    LOCATE Alt, DisB
    PRINT Text$
END SUB

SUB Colore
    IF Info$(2) = "COLORE" THEN RESTORE Colorato:  ELSE RESTORE BlackW
    IF Special(2) = 1 THEN RESTORE Special
    FOR x = 1 TO UBOUND(COl, 1)
        a = a + 1
        FOR y = 1 TO 2
            b = b + 1
            READ COl(a, b)
        NEXT
        b = 0
    NEXT
    COLOR COl(3, 2), COl(3, 2): CLS
    Compiler$(1) = "Colonna Fabrizio"
    Compiler$(8) = "Post-produzione:Conti Ronny"
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
    SLEEP 1
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
LOCATE 3, 22: PRINT "____"
Immissione:
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    IF Tasti$(1) = CHR$(27) THEN Esecuzione
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
    PRINT a$ + STRING$(4 - LEN(a$), "_")
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
    FOR y = 1 TO VAL(Info$(4)) / a
        c = TIMER
        PRINT #2, NSoft$(1)
        FOR x = 1 TO (VAL(Info$(4)) / 10) * 10000 * VAL(Info$(3)) / a
            IF Conn(1) = 1 THEN EXIT SUB
            IF b MOD 100 * VAL(Info$(3)) = 0 THEN GOSUB Punto2
            b = b + 1
        NEXT
        a = a * 2
        d = TIMER
        IF d - c = 0 THEN EXIT FOR
        IF Conn(1) = 1 THEN EXIT SUB
    NEXT
    EXIT SUB

Punto2:
    COLOR COl(5, 1), COl(1, 2)
    aa = aa + 1
    IF aa > 9 THEN aa = 0
    F1$ = "AÃ" + STRING$(10, "Ä") + "´B"
    Center 23, F1$, 50
    LOCATE 23, 18 + aa
    COLOR COl(8, 1), COl(8, 2)
    PRINT CHR$(254)
    COLOR COl(5, 1), COl(1, 2)
RETURN

END SUB

SUB Disconnetti
    IF Conn(1) = 0 THEN
        COLOR COl(7, 1), COl(7, 2)
        Finestra 2, 20, ""
        COLOR COl(7, 1), COl(7, 2)
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
    Colore
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
    Compiler$(6) = "Copyright (C) 2000)"
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
        CASE CHR$(0) + "@": Guida
        CASE CHR$(0) + "A": Invio
        CASE CHR$(0) + "B": Giochi
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
        CASE 10: Guida
        CASE 11
            COLOR 7, 0
            IF Special(2) = 1 THEN
                a = 0
                b = 7
                IF Special(5) = 1 THEN RANDOMIZE TIMER: a = INT(8 * RND): b = 0
                Tasti$(1) = ""
                WHILE Tasti$(1) = ""
                    Tasti$(1) = INKEY$
                WEND
                IF Tasti$(1) = CHR$(0) + "o" THEN COLOR COl(a, 1), b
            END IF
            CLS
            PRINT "Scivere EXIT e premere INVIO per uscire"
            SHELL
            RUN
        CASE 12: Giochi
    END SELECT
    IF Conn(1) = 1 THEN Esecuzione
    Esecuzione

ScritteA:
    c(1) = 1
    LOCATE 23, 5
    COLOR COl(1, 1), COl(1, 2)
    PRINT "< EDITOR >   INVIO=Invio riga ESC+F10=Menu"
    COLOR COl(6, 1), COl(6, 2)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(27)
            Esecuzione
        CASE CHR$(44)
            Frase$(aa) = Frase$(aa) + "ú"
        CASE CHR$(34)
            Frase$(aa) = Frase$(aa) + "ù"
        CASE CHR$(8)
            IF a < 1 THEN
                BEEP
                GOTO ScritteA
            END IF
            a = a - 1
            IF Special(5) = 1 THEN
                Frase$(aa) = RIGHT$(Frase$(aa), LEN(Frase$(aa)) - 1)
            ELSE
                Frase$(aa) = LEFT$(Frase$(aa), LEN(Frase$(aa)) - 1)
            END IF
        CASE CHR$(13)
            a = 0
            IScritte aa
            aa = aa + 1
            Frase$(aa - 1) = ""
            GOTO ScritteA
        CASE ELSE
            IF aa > 16 THEN aa = 1
            a = a + LEN(Tasti$(1))
            IF Special(5) = 1 THEN
                Frase$(aa) = Tasti$(1) + Frase$(aa)
            ELSE
                Frase$(aa) = Frase$(aa) + Tasti$(1)
            END IF
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
    COLOR COl(2, 1), COl(3, 2)
    a = 0
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB + Larg + 2
        PRINT "Û"
    NEXT
    LOCATE DisH + a + 1, DisB + 2
    PRINT STRING$(Larg + 1, "ß")
END SUB

SUB Giochi
    IF Conn(1) > 0 THEN
        COLOR COl(6, 1), COl(6, 2)
    ELSE
        COLOR COl(3, 1), COl(3, 2)
    END IF
    FOR x = 1 TO 10
        a = a + 1
        Center 6 + a, "               º               ", 30
    NEXT
    COLOR COl(2, 1), COl(2, 2)
    Finestra 4, 20, "Giochi"
    COLOR COl(2, 1), COl(2, 2)
    Center 10, "1-Battaglia navale", 20
    IF Special(3) = 1 THEN
        Center 11, "2-Tre carte", 20
        Center 12, "3-Gorilla", 20
    END IF
Immissione3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(27): EXIT SUB
        CASE CHR$(49): GOSUB Scollegati: GOTO Gioco1
        CASE ELSE
            IF Special(3) = 1 THEN
                IF Tasti$ = CHR$(50) THEN GOSUB Scollegati: GOTO Gioco2
                IF Tasti$ = CHR$(51) THEN GOSUB Scollegati: GOTO Gioco3
            END IF
            BEEP
    END SELECT
    GOTO Immissione3

Scollegati:
    IF Conn(1) = 0 THEN
        COLOR COl(7, 1), COl(7, 2)
        Finestra 4, 30, "Messaggio di sistema"
        COLOR COl(7, 1), COl(7, 2)
        Center 10, "Impossibile usare il gioco", 26
        Center 11, "Non si Š collegati a", 20
        Center 12, "nessun computer", 16
        SLEEP 10
        RUN
    END IF
    RETURN
Gioco1:
    PRINT #2, "Avviso"
    SLEEP 1
    PRINT #2, "Gioco"
    SLEEP 1
    PRINT #2, "Navi.bas"
    OPEN "temp.lsc" FOR OUTPUT AS #1: PRINT #1, "1": CLOSE
    SLEEP 3
    RUN "C:NAVI"
Gioco2:
    PRINT #2, "Avviso"
    SLEEP 1
    PRINT #2, "Gioco"
    SLEEP 1
    PRINT #2, "carte.bas"
    OPEN "temp.lsc" FOR OUTPUT AS #1: PRINT #1, "1": CLOSE
    SLEEP 3
    RUN "C:carte"
Gioco3:
    PRINT #2, "Avviso"
    SLEEP 1
    PRINT #2, "Gioco"
    SLEEP 1
    PRINT #2, "gorilla2.bas"
    OPEN "temp.lsc" FOR OUTPUT AS #1: PRINT #1, "1": CLOSE
    SLEEP 3
    RUN "C:gorilla2"


END SUB

SUB Guida
    COLOR COl(1, 1), COl(1, 2)
    Finestra 16, 36, "Guida"
    COLOR COl(1, 1), COl(1, 2)
    GOSUB Pagina1
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(27): Esecuzione
        CASE CHR$(49): GOSUB Pagina2
        CASE CHR$(50): GOSUB Pagina3
        CASE CHR$(51): GOSUB Pagina4
        CASE CHR$(52): GOSUB Pagina7
        CASE ELSE
            BEEP
            Guida
    END SELECT

Pagina1:
    f$(1) = "Guida all'utilizzo del programma"
    f$(2) = "Con questa guida potrete imparare"
    f$(3) = "ad usare L.S.C. con facilit…."
    f$(4) = "Per selezionare l'argomento desi-"
    f$(5) = "derato premete il numero cor-"
    f$(6) = "rispondente "
    f$(7) = ""
    f$(8) = ""
    f$(9) = ""
    f$(10) = ""
    f$(11) = ""
    f$(12) = ""
    f$(13) = ""
    f$(14) = ""
    f$(15) = ""
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, f$(a), 34
    NEXT
    COLOR COl(9, 1), COl(9, 2)
    f$(8) = "1-Connettersi e disconnettersi"
    f$(9) = "2-Inviare file"
    f$(10) = "3-Modificare le opzioni"
    f$(11) = "4-Dialogare"
    a = 6
    FOR x = 1 TO UBOUND(f$) - a
        a = a + 1
        Center 3 + a, f$(a), 34
    NEXT
    RETURN

Pagina2:
    f$(1) = "Per connettersi con un altro compu-"
    f$(2) = "ter andate nel men— Strumenti e"
    f$(3) = "sciegliete 1. A questo punto digi-"
    f$(4) = "tate le 4 cifre del nome del "
    f$(5) = "computer che volete chiamare e il"
    f$(6) = "programma provveder… automaticamen-"
    f$(7) = "te a chiamarlo. Se il computer "
    f$(8) = "chiamato non risponde tutto torne-"
    f$(9) = "r… come all'inizio, altrimenti le"
    f$(10) = "finestre diventeranno bianche."
    f$(11) = "Tasto di scelta rapida = F2"
    f$(12) = "Per disconnetrsi andate in Stru-"
    f$(13) = "menti e premete 2. Tasto di scelta"
    f$(14) = "rapida = F3. Per uscire dal pro-"
    f$(15) = "gramma premete ESC"
    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(34), 34
        Center 3 + a, f$(a), 34
    NEXT
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): Esecuzione
        CASE CHR$(0) + "G": Esecuzione
        CASE ELSE
            BEEP
            GOSUB Pagina2
    END SELECT
    RETURN

Pagina3:
    f$(1) = "Per inviare un file al computer"
    f$(2) = "con il quale si Š collegati andate"
    f$(3) = "su Strumenti e premete 3. Vi appa-"
    f$(4) = "rir… una finestra nella quale do-"
    f$(5) = "vrete scrivere il percorso del file"
    f$(6) = "e il suo nome, dopodichŠ partir… l'"
    f$(7) = "invio del file. La velocit… con cui"
    f$(8) = "esso verr… inviato dipender… dal"
    f$(9) = "tipo di trasmissione scelta e potr…"
    f$(10) = "essere molto veloce o molto lenta"
    f$(11) = "                    . Quando il"
    f$(12) = "file sar… trasmesso tutto il pro-"
    f$(13) = "gramma ritorner… allo stato non"
    f$(14) = "connesso."
    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(34), 34
        Center 3 + a, f$(a), 34
    NEXT
    COLOR COl(9, 1), COl(9, 2)
    Center 14, "(1-Mod. invio file)", 34
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): Esecuzione
        CASE CHR$(0) + "G": Esecuzione
        CASE CHR$(49): GOSUB Pagina6
        CASE ELSE
            BEEP
            GOSUB Pagina3
    END SELECT
    RETURN

Pagina4:
    f$(1) = "Tutte le opzioni si trovano nel "
    f$(2) = "men— Opzioni. Da qui potrete modi-"
    f$(3) = "ficare le opzioni che riguardano l'"
    f$(4) = "utente, quelle che riguardano la "
    f$(5) = "porta seriale e le opzioni di invio"
    f$(6) = "file.Nella proma metterete il Vs. "
    f$(7) = "nome ecognome e il tempo di chiama-"
    f$(8) = "ta, nel secondo opzioni tecniche"
    f$(9) = "per i pi— esperti."
    f$(11) = "Vedasi inoltre:"
    f$(10) = ""
    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(34), 34
        Center 3 + a, f$(a), 34
    NEXT
    COLOR COl(9, 1), COl(9, 2)
    Center 15, "1-Opioni porta seriale", 34
    Center 16, "2-Opzioni di invio file", 34
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): Esecuzione
        CASE CHR$(0) + "G": Esecuzione
        CASE CHR$(49): GOSUB Pagina5
        CASE CHR$(50): GOSUB Pagina6
        CASE ELSE
            BEEP
            GOSUB Pagina4
    END SELECT
    RETURN
Pagina5:
    f$(1) = "Le opzioni che potrete modificare"
    f$(2) = "sono la parit… dei dati, a quanti"
    f$(3) = "bit bisogna trasmettere, il numero"
    f$(4) = "della porta e la velocit… di"
    f$(5) = "conversazione"
    f$(6) = ""
    f$(7) = ""
    f$(8) = ""
    f$(9) = ""
    f$(11) = ""
    f$(10) = ""

    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(35), 34
        Center 3 + a, f$(a), 34
    NEXT
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): STOP
        CASE CHR$(0) + "G": Esecuzione
        CASE ELSE
            BEEP
            GOSUB Pagina5
    END SELECT
    RETURN

Pagina6:
    f$(1) = "Potete scegliere dove salvare i"
    f$(2) = "file che vi arrivano premendo 1."
    f$(3) = "Modalit… Norm: vel.MAX=52,2 KB/min."
    f$(4) = "! N.B.!non trasmette le virgole"
    f$(5) = "Mod. ASCII: vel.MAX= 2 KB/min."
    f$(6) = "Mod.ASCII Rapida:v.Max=2,77 KB/min."
    f$(7) = "Mod.ASCII Compressa= 4,88 KB/min."
    f$(8) = "Mod ASCII R. Compressa=5,55 KB/min."
    f$(9) = "!ATTENZIONE! la modalit… Norm non"
    f$(10) = "trasmette le virgole, mentre le"
    f$(11) = "altre si, ma a discapito della"
    f$(12) = "velocit…"
    f$(13) = ""
    f$(14) = ""
    f$(15) = ""

    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(35), 34
        Center 3 + a, f$(a), 34
    NEXT
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): STOP
        CASE CHR$(0) + "G": Esecuzione
        CASE ELSE
            BEEP
            GOSUB Pagina6
    END SELECT
    RETURN

Pagina7:
    f$(1) = "Per andare nell'editor, cioŠ per"
    f$(2) = "poter conversare, dovete premere "
    f$(3) = "INVIO. A questo punto nella barra"
    f$(4) = "bianca sottostante compare la"
    f$(5) = "scritta EDITOR. Da adesso potrete"
    f$(6) = "scrivere le frasi da trasmettere."
    f$(7) = "Ogni volta che si preme INVIO la "
    f$(8) = "frase viene trasmessa.Viene tras-"
    f$(9) = "messa anche quando si va a capo,"
    f$(11) = "procedura che viene effettuata"
    f$(10) = "automaticamente.Il segno ú sta "
    f$(11) = "a rappresentare la virgola e il"
    f$(12) = "segno ù rappresenta le doppie"
    f$(13) = "virgloette. Per uscire dall'edit"
    f$(14) = "premere ESC"

    COLOR COl(1, 1), COl(1, 2)
    a = 0
    FOR x = 1 TO UBOUND(f$)
        a = a + 1
        Center 3 + a, SPACE$(35), 34
        Center 3 + a, f$(a), 34
    NEXT
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(8): Guida
        CASE CHR$(27): Esecuzione
        CASE ELSE
            BEEP
            GOSUB Pagina6
    END SELECT
    RETURN

END SUB

SUB Informazioni
    COLOR COl(7, 1), COl(7, 2)
    Finestra 8, 40, "Informazioni su L.S.C."
    COLOR COl(7, 1), COl(7, 2)
    Center 8, "Link by Serial Connector", 26
    Center 9, "Copyright (C) Col S.p.A. 2000", 32
    Center 10, "Svilupato interamente su Software QBasic", 40
    Center 11, "Copyright (C) Microsoft 1987-1993", 34
    Center 12, "Prodotto dalla Col S.p.A.", 26
    Center 13, "Versione 2.1", 14
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    IF Tasti$(1) = CHR$(0) + CHR$(15) THEN
        IF Special(3) = 0 THEN EXIT SUB
        Tasti$(1) = ""
        WHILE Tasti$(1) = ""
            Tasti$(1) = INKEY$
        WEND
        IF Tasti$(1) = CHR$(0) + "s" THEN Special(5) = 1
    END IF
    MSel(1) = 0
    EXIT SUB
END SUB

SUB Invio
    IF Conn(1) = 0 THEN
        IF Info$(1) <> "Colonna" OR Special(1) = 0 THEN
            COLOR COl(7, 1), COl(7, 2)
            Finestra 4, 30, "Messaggio di sistema"
            COLOR COl(7, 1), COl(7, 2)
            Center 10, "Impossibile inviare file", 26
            Center 11, "Non si Š collegati a", 20
            Center 12, "nessun computer", 16
            SLEEP 10
            EXIT SUB
        END IF
    END IF
    COLOR COl(7, 1), COl(7, 2)
    Finestra 8, 30, "Invia file"
    COLOR COl(7, 1), COl(7, 2)
    Center 8, "Scrivi il percorso del file", 28
    Center 10, "Scrivi il nome del file", 24
    Center 14, "AÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ´B", 20
    COLOR COl(1, 1), COl(1, 2)
    Center 9, SPACE$(27), 28
    Center 11, SPACE$(27), 28
    CLOSE
    LOCATE 9, 27: INPUT Perc$
    IF Perc$ = "" THEN
        Perc$ = LEFT$(Info$(10), 2)
    ELSE
        IF RIGHT$(Perc$, 1) <> "\" THEN Perc$ = Perc$ + "\"
    END IF
    LOCATE 9, 27: PRINT Perc$ + "  "
    IF Special(1) = 1 THEN
        a = TIMER: a$ = ""
        WHILE (TIMER - a) < 2 AND a$ = ""
            a$ = INKEY$
        WEND
        IF a$ = CHR$(8) THEN
            Special(3) = 1
        END IF
        EXIT SUB
    END IF
    LOCATE 11, 27: INPUT File$
    IF File$ = "" THEN Esecuzione
    LOCATE 11, 27: PRINT File$ + "  "
    COLOR COl(7, 1), COl(7, 2)
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
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
            PRINT #2, a$
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
    COLOR COl(8, 1), COl(7, 2)
    PRINT CHR$(254)
    COLOR COl(7, 1), COl(7, 2)
RETURN

ASCII2:
    IF Info$(14) = "S" THEN GOSUB ASCII2T
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
    IF Info$(14) = "S" THEN GOSUB ASCIIR2T
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
            ELSEIF ASC(b$) < 32 THEN
                    PRINT #2, "ASCII"
                    PRINT #2, ASC(b$)
            ELSE
                    PRINT #2, b$
            END IF
            FOR y = 1 TO 90 * VAL(Info$(3)): NEXT
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
    EXIT SUB
RETURN

ASCII2T:
    OPEN Info$(10) + "Temp.lsc" FOR OUTPUT AS #3
    Center 12, "Compressione in corso", 22
    b = 0
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
        FOR x = 1 TO LEN(a$)
            b = b + 1
            b$ = RIGHT$(LEFT$(a$, b), 1)
            PRINT #3, RTRIM$(LTRIM$(STR$(ASC(b$))))
        NEXT
        b = 0
        b$ = ""
            PRINT #3, "13"
    WEND
    Center 12, "                     ", 22
    CLOSE #1, #3
    OPEN Info$(10) + "Temp.lsc" FOR INPUT AS #1
    PRINT #2, "Comp1"
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
            PRINT #2, a$
        FOR x = 1 TO 115 * VAL(Info$(3)): NEXT
        g = g + LEN(a$)
        Grand$ = "Grandezza:" + STR$(g)
        Center 12, Grand$, 18
        GOSUB Punto
    WEND
    PRINT #2, "Fine"
    CLOSE
    KILL Info$(10) + "Temp.lsc"
    EXIT SUB
       
ASCIIR2T:
    OPEN Info$(10) + "Temp.lsc" FOR OUTPUT AS #3
    Center 12, "Compressione in corso", 22
    b = 0
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
        FOR x = 1 TO LEN(a$)
            b = b + 1
            b$ = RIGHT$(LEFT$(a$, b), 1)
            IF b$ = "," THEN
                    PRINT #3, "ASCII"
                    PRINT #3, ASC(b$)
            ELSEIF b$ = CHR$(32) THEN
                    PRINT #3, "ASCII"
                    PRINT #3, ASC(" ")
            ELSEIF b$ = CHR$(34) THEN
                    PRINT #3, "ASCII"
                    PRINT #3, "34"
            ELSEIF ASC(b$) < 32 THEN
                    PRINT #3, "ASCII"
                    PRINT #3, ASC(b$)
            ELSE
                    PRINT #3, b$
            END IF
        NEXT
        PRINT #3, "13"
        b$ = "": b = 0
    WEND
    Center 12, "                     ", 22
    CLOSE #1, #3
    OPEN Info$(10) + "Temp.lsc" FOR INPUT AS #1
    PRINT #2, "Comp1"
    WHILE EOF(1) = 0
            LINE INPUT #1, a$
            PRINT #2, a$
        FOR x = 1 TO 100 * VAL(Info$(3)): NEXT
        g = g + LEN(LTRIM$(STR$(ASC(a$))))
        Grand$ = "Grandezza:" + STR$(g)
        Center 12, Grand$, 18
        GOSUB Punto
    WEND
    PRINT #2, "Fine"
    CLOSE
    KILL Info$(10) + "Temp.lsc"
    EXIT SUB

END SUB

SUB InvioOp
    COLOR COl(7, 1), COl(7, 2)
    Finestra 8, 30, "Opzioni di invio file"
    COLOR COl(7, 1), COl(7, 2)
    F1$ = "1-Directory di arrivo: "
    F2$ = "2-Invio ASCII: " + Info$(11)
    F3$ = "3-Ricezione aut.ASCII: " + Info$(12)
    F4$ = "4-Invio ASCII rapido:" + Info$(13)
    F5$ = "5-Compressione automatica:" + Info$(14)
    F6$ = "6-Decomp. automatica:" + Info$(15)
    Center 8, F1$, 28
    Center 10, F2$, 28
    Center 11, F3$, 28
    Center 12, F4$, 28
    Center 13, F5$, 28
    Center 14, F6$, 28
    COLOR COl(1, 1), COl(1, 2)
    Center 9, SPACE$(28), 28
    Center 9, Info$(10), 28
    COLOR COl(7, 1), COl(7, 2)
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
            LOCATE 10, 41: PRINT SPACE$(10)
            LOCATE 10, 41: INPUT Info$(11)
            IF UCASE$(Info$(11)) <> "N" AND UCASE$(Info$(11)) <> "S" THEN RUN
            LOCATE 10, 41: PRINT Info$(11) + "  "
            Info$(11) = UCASE$(Info$(11))
            GOSUB Salva3
        CASE CHR$(51)
            LOCATE 11, 50: PRINT SPACE$(5)
            LOCATE 11, 50: INPUT Info$(12)
            IF UCASE$(Info$(12)) <> "N" AND UCASE$(Info$(12)) <> "S" THEN RUN
            LOCATE 11, 50: PRINT Info$(12) + "  "
            Info$(12) = UCASE$(Info$(12))
            GOSUB Salva3
        CASE CHR$(52)
            LOCATE 12, 48: PRINT SPACE$(5)
            LOCATE 12, 48: INPUT Info$(13)
            IF UCASE$(Info$(13)) <> "N" AND UCASE$(Info$(13)) <> "S" THEN RUN
            LOCATE 12, 48: PRINT Info$(13) + "  "
            Info$(13) = UCASE$(Info$(13))
            GOSUB Salva3
        CASE CHR$(53)
            LOCATE 13, 53: PRINT SPACE$(3)
            LOCATE 13, 53: INPUT Info$(14)
            IF UCASE$(Info$(14)) <> "N" AND UCASE$(Info$(14)) <> "S" THEN RUN
            LOCATE 13, 53: PRINT Info$(14) + "  "
            Info$(14) = UCASE$(Info$(14))
            GOSUB Salva3
        CASE CHR$(54)
            LOCATE 14, 48: PRINT SPACE$(5)
            LOCATE 14, 48: INPUT Info$(15)
            IF UCASE$(Info$(15)) <> "N" AND UCASE$(Info$(15)) <> "S" THEN RUN
            LOCATE 14, 48: PRINT Info$(15) + "  "
            Info$(15) = UCASE$(Info$(15))
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
            IF RIGHT$(Info$(10), 1) <> "\" THEN Info$(10) = Info$(10) + "\"
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
    COLOR COl(1, 1), COl(1, 2)
    Center 9, SPACE$(28), 28
    Center 9, a$, 28
    COLOR COl(7, 1), COl(7, 2)
    GOTO Dir
END SUB

SUB IScritte (aa)
    PRINT #2, "Scritta"
    PRINT #2, Frase$(aa)
    IF Special(4) = 1 THEN
        FOR x = 1 TO 10000 * VAL(Info$(3)): NEXT
        PRINT #2, "Scritta"
        PRINT #2, "Produttori del programma:"
        FOR x = 1 TO 10000 * VAL(Info$(3)): NEXT
        RANDOMIZE TIMER
        a = INT(10 * RND(1))
        PRINT #2, "Scritta"
        PRINT #2, Compiler$(a)
    END IF
END SUB

SUB Menu (Selezione)
    Compiler$(2) = "Conti Riccardo"
    Compiler$(3) = "Conti Maria Stella"
Selezione2:
    a = 0
    IF MSel(1) > 0 THEN EXIT SUB
    COLOR COl(2, 1), COl(2, 2)
    FOR x = 1 TO UBOUND(NMenu$)
        a = a + 1
        LOCATE 2, NMenu(a)
        PRINT "<" + NMenu$(a) + ">"
    NEXT
    COLOR COl(7, 1), COl(7, 2)
    LOCATE 2, NMenu(Selezione): PRINT CHR$(17)
    LOCATE 2, NMenu(Selezione) + LEN(NMenu$(Selezione)) + 1: PRINT CHR$(16)
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    SELECT CASE Tasti$(1)
        CASE CHR$(0) + "`": Speciale
        CASE CHR$(0) + "H"
            BEEP
            GOTO Selezione2
        CASE CHR$(0) + "P"
            BEEP
            GOTO Selezione2
       CASE CHR$(0) + "M"
            IF Selezione < 3 THEN
                Selezione = Selezione + 1
                GOTO Selezione2
            ELSE
                BEEP
                GOTO Selezione2
            END IF
        CASE CHR$(0) + "K"
            IF Selezione > 1 THEN
                Selezione = Selezione - 1
                GOTO Selezione2
            ELSE
                BEEP
                GOTO Selezione2
             END IF
        CASE CHR$(27): Esecuzione
        CASE CHR$(13)
            COLOR COl(2, 1), COl(2, 2)
            SELECT CASE Selezione
                CASE 1: GOSUB Menu1
                CASE 2: GOSUB Menu2
                CASE 3: GOSUB Menu3
            END SELECT
            GOSUB Selezione
        CASE ELSE: GOTO Selezione2
    END SELECT
    'GOTO Selezione2
    EXIT SUB

Menu1:
    Finestra 8, 20, NMenu$(Selezione)
    COLOR COl(2, 1), COl(2, 2)
    a = 0
    FOR x = 1 TO UBOUND(TipMenu$, 2)
        a = a + 1
        IF TipMenu$(Selezione, a) <> "" THEN
            Center 7 + a, STR$(a) + "- " + TipMenu$(Selezione, a), 20
        END IF
    NEXT
    MSel = Selezione
    RETURN

Menu2:
    Finestra 6, 26, NMenu$(Selezione)
    COLOR COl(2, 1), COl(2, 2)
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
    Finestra 4, 22, NMenu$(Selezione)
    a = 0
    COLOR COl(2, 1), COl(2, 2)
    FOR x = 1 TO UBOUND(TipMenu$, 2)
        a = a + 1
        IF TipMenu$(Selezione, a) <> "" THEN
            Center 9 + a, STR$(a) + "- " + TipMenu$(Selezione, a), 20
        END IF
    NEXT
    MSel = Selezione
    RETURN

Selezione:
    COLOR COl(2, 1), COl(2, 2)
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
    IF MSel = 1 AND Tasti$(1) = CHR$(52) THEN MSel(1) = 11
    IF MSel = 1 AND Tasti$(1) = CHR$(53) THEN MSel(1) = 12
    IF MSel = 1 AND Tasti$(1) = CHR$(54) THEN MSel(1) = 4
    IF MSel = 2 AND Tasti$(1) = CHR$(49) THEN MSel(1) = 5
    IF MSel = 2 AND Tasti$(1) = CHR$(50) THEN MSel(1) = 6
    IF MSel = 2 AND Tasti$(1) = CHR$(51) THEN MSel(1) = 7
    IF MSel = 2 AND Tasti$(1) = CHR$(52) THEN MSel(1) = 8
    IF MSel = 2 AND Tasti$(1) = CHR$(53) THEN GOSUB Colorazione
    IF MSel = 3 AND Tasti$(1) = CHR$(49) THEN MSel(1) = 9
    IF MSel = 3 AND Tasti$(1) = CHR$(50) THEN MSel(1) = 10
    COLOR COl(6, 1), COl(6, 2)
    RETURN

Colorazione:
    IF Info$(2) = "COLORE" THEN
        Info$(2) = "BN"
    ELSE
        Info$(2) = "COLORE"
    END IF
    IF Special(1) = 1 THEN Special(2) = 1: Info$(2) = "COLORE"
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
    Compiler$(10) = "QBASIC Production"
    TipMenu$(1, 1) = "Connetti    F2"
    TipMenu$(1, 2) = "Disconnetti F3"
    TipMenu$(1, 3) = "Invia file  F7"
    TipMenu$(1, 4) = "Shell di MS-DOS"
    TipMenu$(1, 5) = "Giochi      F8"
    TipMenu$(1, 6) = "Esci       ESC"
    TipMenu$(2, 1) = "Utente          "
    TipMenu$(2, 2) = "Testa PC      F4"
    TipMenu$(2, 3) = "Op. di lavoro F5"
    TipMenu$(2, 4) = "Op. di invio file"
    TipMenu$(2, 5) = "Bianco e Nero [ ]"
    TipMenu$(3, 1) = "Informazioni  F1"
    TipMenu$(3, 2) = "Guida         F6"
    Compiler$(5) = "Colonna & Co. (C)"
END SUB

SUB Opzioni
    COLOR COl(7, 1), COl(7, 2)
    Finestra 6, 30, "Impostazioni di lavoro"
    COLOR COl(7, 1), COl(7, 2)
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
    IF Conn(1) > 0 THEN COLOR COl(6, 1), COl(6, 2)
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
        COLOR COl(6, 1), COl(6, 2)
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
    IF Info$(1) = "" THEN RUN "c:setup.bas"
END SUB

SUB Preparativi
COLOR COl(2, 1), COl(2, 2)
LOCATE 2: PRINT SPACE$(80)
FOR x = 1 TO UBOUND(NMenu$)
    a = a + 1
    LOCATE 2, NMenu(a)
    PRINT "<" + NMenu$(a) + ">"
NEXT
COLOR COl(4, 1), COl(4, 2)
LOCATE 3, 9: PRINT "Connessione:          "
LOCATE 3, 31: PRINT "Stato: "
LOCATE 3, 56: PRINT "Velocit…:"
IF Conn(1) = 0 THEN
    COLOR COl(3, 1), COl(3, 2)
    LOCATE 3, 37: PRINT "Disconnesso        "
    LOCATE 3, 65: PRINT Info$(5) + " "
ELSE
    COLOR COl(6, 1), COl(6, 2)
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
        COLOR COl(7, 1), COl(7, 2)
        Finestra 4, 30, "Invio file"
        COLOR COl(7, 1), COl(7, 2)
        Center 10, "Ti stanno inviando il file", 28
        Center 11, File$, 10
        CLOSE
        Porta$(1) = "COM" + Info$(9) + ":9600," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
        OPEN Porta$ FOR RANDOM AS #2
        OPEN Info$(10) + "\" + File$ FOR OUTPUT AS #1
        IF Info$(12) = "S" THEN
            INPUT #2, Ascii$
            IF Ascii$ = "ASCII" THEN GOSUB Ascii
            IF Ascii$ = "ASCIIR" THEN GOSUB ASCIIR
        ELSE
            INPUT #2, a$
            IF a$ = "ASCII" OR Ascii$ = "ASCIIR" THEN GOSUB Impossibile
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
            COLOR COl(7, 1), COl(7, 2)
            Finestra 4, 20, "Avviso"
            COLOR COl(7, 1), COl(7, 2)
            Center 10, "Richiesta di ", 12
            Center 11, "disconnessione", 14
            Center 12, "accettata", 8
            SLEEP 5
            RUN
        ELSE
            IF a$ = "Avviso" THEN
                INPUT #2, a$
                IF a$ = "Gioco" THEN
                    INPUT #2, a$
                    OPEN "temp.lsc" FOR OUTPUT AS #1: PRINT #1, "0": CLOSE
                    RUN a$
                END IF
            END IF
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
    COLOR COl(7, 1), COl(7, 2)
    Finestra 4, 30, "Opzioni di invio file"
    COLOR COl(7, 1), COl(7, 2)
    Center 10, "Impossibile rivcevere file", 28
    Center 11, "in formato ASCII", 28
    Center 12, "Modalit… disattivata", 28
    WHILE a$ <> "Fine"
        INPUT #2, a$
    WEND
    RETURN

Ascii:
    WHILE a$ <> "Fine"
        INPUT #2, a$
        IF a$ <> "Fine" THEN
            IF a$ <> "Comp1" THEN
                b$ = b$ + CHR$(VAL(a$))
                IF a$ = "013" THEN
                    PRINT #1, b$
                    g = g + LEN(b$)
                    F1$ = "Grandezza:" + STR$(g)
                    b$ = ""
                END IF
            ELSE
                GOSUB ASCIIC
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
        IF a$ <> "Comp1" THEN
            IF a$ <> "Fine" THEN
                IF a$ <> "ASCII" THEN
                    IF a$ = "013" THEN
                        PRINT #1, b$
                        b$ = ""
                    ELSE
                        b$ = b$ + a$
                    END IF
                    b$ = b$ + CHR$(VAL(a$))
                ELSE
                    INPUT #2, a$
                    b$ = b$ + CHR$(VAL(a$))
                END IF
            END IF
            g = g + LEN(a$)
            F1$ = "Grandezza:" + STR$(g)
            Center 12, F1$, 16
        ELSE
            GOSUB ASCIIRC
        END IF
    WEND
    CLOSE
    SLEEP 5
    c(1) = 1
    Conn(1) = 1
    RUN
              
ASCIIC:
    OPEN Info$(10) + "Temp.lsc" FOR OUTPUT AS #3
    WHILE a$ <> "Fine"
        IF a$ <> "Fine" THEN
                INPUT #2, a$
                PRINT #3, a$
                g = g + LEN(a$)
                F1$ = "Grandezza:" + STR$(g)
                Center 12, F1$, 16
        END IF
    WEND
    CLOSE #3
        IF Info$(15) <> "N" THEN
            Center 12, "Decompressione in corso", 24
            OPEN Info$(10) + "Temp.lsc" FOR INPUT AS #3
            WHILE EOF(3) = 0
                LINE INPUT #3, a$
                IF a$ = "13" THEN
                    PRINT #1, b$
                    b$ = ""
                ELSE
                    b$ = b$ + CHR$(VAL(a$))
                END IF
            WEND
            CLOSE
            KILL Info$(10) + "Temp.lsc"
        ELSE
            COLOR COl(7, 1), COl(7, 2)
            Finestra 4, 30, "Opzioni di invio file"
            COLOR COl(7, 1), COl(7, 2)
            Center 10, "Impossibile decomprimere", 28
            Center 11, "file.", 28
            Center 12, "Modalit… disattivata", 28
            NAME Info$(10) + "Temp.lsc" AS Info$(10) + "\" + File$
            WHILE a$ <> "Fine"
                INPUT #2, a$
            WEND
        END IF
        RUN
   
ASCIIRC:
    OPEN Info$(10) + "Temp.lsc" FOR OUTPUT AS #3
    WHILE a$ <> "Fine"
        IF a$ <> "Fine" THEN
                INPUT #2, a$
                PRINT #3, a$
                g = g + LEN(a$)
                F1$ = "Grandezza:" + STR$(g)
                Center 12, F1$, 16
        END IF
    WEND
    CLOSE #3
    IF Info$(15) <> "N" THEN
        Center 12, "Decompressione in corso", 24
        OPEN Info$(10) + "Temp.lsc" FOR INPUT AS #3
        WHILE a$ <> "Fine"
            INPUT #3, a$
            IF a$ <> "Fine" THEN
                IF a$ <> "ASCII" THEN
                    IF a$ = "13" THEN
                        PRINT #1, b$
                        b$ = ""
                    ELSE
                        b$ = b$ + a$
                    END IF
                ELSE
                    INPUT #3, a$
                    b$ = b$ + CHR$(VAL(a$))
                END IF
            END IF
        WEND
        CLOSE
        KILL Info$(10) + "Temp.lsc"
    ELSE
        COLOR COl(7, 1), COl(7, 2)
        Finestra 4, 30, "Opzioni di invio file"
        COLOR COl(7, 1), COl(7, 2)
        Center 10, "Impossibile decomprimere", 28
        Center 11, "file.", 28
        Center 12, "Modalit… disattivata", 28
        CLOSE
        NAME Info$(10) + "Temp.lsc" AS Info$(10) + "\" + File$
        WHILE a$ <> "Fine"
            INPUT #2, a$
        WEND
    END IF
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

SUB Speciale
    Compiler$(4) = "Ditta Col S.r.L."
    Compiler$(9) = "QiukBASIC LSC 2.2"
    Tasti$(1) = ""
    WHILE Tasti$(1) = ""
        Tasti$(1) = INKEY$
    WEND
    IF Tasti$(1) = CHR$(27) THEN
        Tasti$(1) = ""
        WHILE Tasti$(1) = ""
            Tasti$(1) = INKEY$
        WEND
        IF Tasti$(1) = CHR$(0) + "ˆ" THEN
            Special(1) = 1
        ELSE
            GOTO No
        END IF
    ELSE
        GOTO No
    END IF
No:
    Tasti$(1) = "1"
    IF Special(1) = 0 THEN
        BEEP
    ELSE
        COLOR 15, 7
        LOCATE 23, 80
        PRINT "ú"
    END IF
    EXIT SUB
END SUB

SUB Tempo
    DIM a(10)
    COLOR COl(7, 1), COl(7, 2)
    Finestra 6, 30, "Testa il PC"
    COLOR COl(7, 1), COl(7, 2)
    Center 9, "Testa la velocit… del PC", 26
    Center 10, "Il Test pu• durare molto,", 26
    Center 11, "a seconda della velocit…", 26
    Center 12, "del computer", 26
    Center 13, "Premi un tasto per iniziare", 26
    DO: LOOP WHILE INKEY$ = ""
    Finestra 6, 30, "Testa il PC"
    COLOR COl(7, 1), COl(7, 2)
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
    COLOR COl(7, 1), COl(7, 2)
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
    EXIT SUB
END SUB

SUB Titolo
    COLOR COl(1, 1), COl(1, 2)
    LOCATE 1
    PRINT SPACE$(80)
    Tit$ = " L i n k   b y   S e r i a l   C o n n e c t o r   2 . 1 "
    LOCATE 1, 40 - (LEN(Tit$)) / 2
    PRINT Tit$
    LOCATE 23
    PRINT SPACE$(80)
    LOCATE 23, 5
    PRINT "F10=Men—"
END SUB

SUB Utente
    IF Conn(1) > 0 THEN
        COLOR COl(6, 1), COl(6, 2)
    ELSE
        COLOR COl(3, 1), COl(3, 2)
    END IF
    FOR x = 1 TO 8
        a = a + 1
        Center 7 + a, "               º               ", 30
    NEXT
    a = 0: a$ = ""
    COLOR COl(7, 1), COl(7, 2)
    Finestra 4, 30, "Opzioni utente"
    COLOR COl(7, 1), COl(7, 2)
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
        CASE CHR$(27)
            MSel(1) = 0
            Esecuzione
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
        CASE "#"
            IF Special(2) = 1 THEN
                Info$(1) = "Colonna & Co. Association"
                COLOR COl(7, 1), COl(7, 2)
                Special(4) = 1
            ELSE
                BEEP
            END IF
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

