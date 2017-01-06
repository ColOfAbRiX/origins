DECLARE FUNCTION Tempo! (Tipo!)
DECLARE SUB Azzeramento (Motore!)
DECLARE SUB Send (Dati!, Tipo!)
DECLARE SUB SInfor ()
DECLARE SUB Suona (Suono$)
DECLARE SUB Esci ()
DECLARE SUB Intestazioni (Descrizione$)
DECLARE SUB AggiornaInfo (Motore!)
DECLARE SUB PInfor ()
DECLARE SUB Pagina (Text$, Tipo!)
DECLARE SUB Menu ()
DECLARE SUB Finestra (Alt!, Larg!, Text$)
DECLARE SUB Center (Text$, Linea!)
DECLARE FUNCTION BinToDec! (Numero$)
DECLARE FUNCTION calcPosition$ (strPos$, Mov!, Velocity!)

DIM SHARED Errore$
DIM SHARED File$
DIM SHARED Porta
DIM SHARED Intro
DIM SHARED Music
DIM SHARED TempoGiro(1)
DIM SHARED Logica(1)
DIM SHARED Sequenza$(1)
DIM SHARED Ritardo(1)
DIM SHARED Incremento(1)
DIM SHARED Automatico(1)
DIM SHARED Verso(1)
DIM SHARED Inizio(1)
DIM SHARED Fine(1)
DIM SHARED Contatore(1)
DIM SHARED Conteggio(1)
DIM SHARED Compensazione(1)
DIM SHARED TempoCicloWhile(1, 1)

CONST TRUE = -1
CONST FALSE = 0

File$ = "port.ini"
CALL PInfor
Send 0, 0

COLOR 15, 1: CLS

IF Intro = TRUE THEN
    COLOR 14, 12
    Finestra 4, 32, "Motor Controller v1.0"
    COLOR 15, 12
    Center "Motor Controller ver1.0", 10
    Center "Sviluppato da Colonna Fabrizio", 11
    Center "QuickBASIC 4.5", 12
   
    IF Music = TRUE THEN
        Suona "O2L16B9N0BAAN0G-N0G-N0G-EEEN0O1b9N0BAAN0B"
    ELSE
        SLEEP 3
    END IF

END IF

Sequenza$(0) = "0001"
Sequenza$(1) = "0001"
Ritardo(0) = 0
Ritardo(1) = 0
Incremento(0) = 10
Incremento(1) = 10

COLOR 15, 1: CLS
Pagina "Movimento motori", 0
CALL Intestazioni("F10=Menu    ESC=Esci")

KeyInput:
    Key$ = "": CicloCorrente = 0

    WHILE Key$ = ""
        CicloPrecedente = CicloCorrente
        CicloCorrente = ABS(SGN(NOT (-CicloCorrente)))

        TempoCicloWhile(CicloCorrente, 0) = TIMER

        IF Automatico(0) = TRUE AND Conteggio(0) = -1 THEN

            IF Contatore(0) >= (Ritardo(0) / ((1 / Compensazione(1)) / (TempoCicloWhile(CicloPrecedente, 1) - TempoCicloWhile(CicloPrecedente, 0)))) THEN
                Sequenza$(0) = calcPosition(Sequenza$(0), SGN(Verso(0)), 0)
                Send 0, 1
                Contatore(0) = 0

                IF Sequenza$(0) = "0001" THEN
                    Fine(0) = TIMER
                    TempoGiro(0) = Fine(0) - Inizio(0)
                    Inizio(0) = TIMER
                END IF
                CALL AggiornaInfo(0)

            ELSEIF Contatore(0) < (Ritardo(0) / ((1 / Compensazione(1)) / (TempoCicloWhile(CicloPrecedente, 1) - TempoCicloWhile(CicloPrecedente, 0)))) THEN
                Contatore(0) = Contatore(0) + 1
            END IF

        ELSEIF Automatico(0) = FALSE AND TempoGiro(0) <> 0 THEN
            TempoGiro(0) = 0
            CALL AggiornaInfo(0)
       
        ELSEIF Automatico(0) = TRUE AND Conteggio(0) = 0 THEN
          
            Sequenza$(0) = calcPosition(Sequenza$(0), SGN(Verso(0)), Ritardo(0))
            Send 0, 1
            Contatore(0) = 0

            IF Sequenza$(0) = "0001" THEN
                Fine(0) = TIMER
                TempoGiro(0) = Fine(0) - Inizio(0)
                Inizio(0) = TIMER
            END IF
            CALL AggiornaInfo(0)

        END IF
       
        IF Automatico(1) = TRUE AND Conteggio(1) = -1 THEN
          
            IF Contatore(1) >= (Ritardo(1) / (1 / Compensazione(1) / (TempoCicloWhile(CicloPrecedente, 1) - TempoCicloWhile(CicloPrecedente, 0)))) THEN
                Sequenza$(1) = calcPosition(Sequenza$(1), SGN(Verso(1)), 0)
                Send 0, 1
                Contatore(1) = 0

                IF Sequenza$(1) = "0001" THEN
                    Fine(1) = TIMER
                    TempoGiro(1) = Fine(1) - Inizio(1)
                    Inizio(1) = TIMER
                END IF
                CALL AggiornaInfo(1)
            ELSEIF Contatore(1) < (Ritardo(1) / (1 / Compensazione(1) / (TempoCicloWhile(CicloPrecedente, 1) - TempoCicloWhile(CicloPrecedente, 0)))) THEN
                Contatore(1) = Contatore(1) + 1
            END IF

        ELSEIF Automatico(1) = FALSE AND TempoGiro(1) <> 0 THEN
            TempoGiro(1) = 0
            CALL AggiornaInfo(1)
      
        ELSEIF Automatico(1) = TRUE AND Conteggio(1) = 0 THEN
         
            Sequenza$(1) = calcPosition(Sequenza$(1), SGN(Verso(1)), Ritardo(1))
            Send 0, 1
            Contatore(1) = 0

            IF Sequenza$(1) = "0001" THEN
                Fine(1) = TIMER
                TempoGiro(1) = Fine(1) - Inizio(1)
                Inizio(1) = TIMER
            END IF
            CALL AggiornaInfo(1)

        END IF

        Key$ = INKEY$

        SELECT CASE LCASE$(Key$)

            CASE CHR$(0) + "D"
                CALL Menu
                Key$ = ""
                CALL Intestazioni("F10=Menu    ESC=Esci")

            CASE CHR$(27)
                CALL Esci
                COLOR 15, 1
                Pagina "Movimento motori", 0
                CALL Intestazioni("F10=Menu    ESC=Esci")
                Key$ = ""

            CASE "4"
                IF Automatico(0) <> TRUE THEN
                    Sequenza$(0) = calcPosition(Sequenza$(0), 1, 1)
                    Send 0, 1
                    CALL AggiornaInfo(0)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "1"
                IF Automatico(0) <> TRUE THEN
                    Sequenza$(0) = calcPosition(Sequenza$(0), -1, 1)
                    Send 0, 1
                    CALL AggiornaInfo(0)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""
           
            CASE "7"
                Suona "O4L16MSB"
                Automatico(0) = NOT (Automatico(0))

                Key$ = ""

            CASE "8"
                Suona "O4L16MSB"
                Verso(0) = NOT (Verso(0))
               
                IF Verso(0) = FALSE THEN
                    LOCATE 9, 24: PRINT "orario" + SPACE$(4)
                ELSEIF Verso(0) = TRUE THEN
                    LOCATE 9, 24: PRINT "antiorario" + SPACE$(4)
                END IF

                Key$ = ""

            CASE "6"
                COLOR 15, 1
                IF Incremento(0) <= 100000 THEN
                    Incremento(0) = Incremento(0) * 10
                    LOCATE 8, 23: PRINT STR$(Incremento(0)) + " ms" + SPACE$(6)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""
           
            CASE "3"
                COLOR 15, 1
                IF Incremento(0) > .1 THEN
                    Incremento(0) = Incremento(0) / 10
                    LOCATE 8, 23: PRINT STR$(Incremento(0)) + " ms" + SPACE$(6)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "5"
                COLOR 15, 1
                Ritardo(0) = Ritardo(0) + Incremento(0) * 10
                LOCATE 7, 19: PRINT STR$(Ritardo(0) / 10) + " ms" + SPACE$(10)
                Suona "O4L16MSB"
                Key$ = ""
          
            CASE "2"
                COLOR 15, 1
                IF Ritardo(0) > 0 AND Ritardo(0) - Incremento(0) * 10 >= 0 THEN
                    Ritardo(0) = Ritardo(0) - Incremento(0) * 10
                    LOCATE 7, 19: PRINT STR$(Ritardo(0) / 10) + " ms" + SPACE$(10)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "9"
                COLOR 15, 1
                CALL Azzeramento(0)
                CALL PInfor
                Automatico(0) = FALSE
                Incremento(0) = 10
                Ritardo(0) = 0
                TempoGiro(0) = 0
                Verso(0) = 0
                AggiornaInfo (0)
                LOCATE 9, 24: PRINT "orario" + SPACE$(4)
                LOCATE 7, 19: PRINT STR$(Ritardo(0) / 10) + " ms" + SPACE$(10)
                LOCATE 8, 23: PRINT STR$(Incremento(0)) + " ms" + SPACE$(6)
                Suona "O4L16MSBL10G"
                Key$ = ""

           
           
            CASE "a"
                IF Automatico(1) <> TRUE THEN
                    Sequenza$(1) = calcPosition(Sequenza$(1), 1, 1)
                    Send 0, 1
                    CALL AggiornaInfo(1)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "z"
                IF Automatico(1) <> TRUE THEN
                    Sequenza$(1) = calcPosition(Sequenza$(1), -1, 1)
                    Send 0, 1
                    CALL AggiornaInfo(1)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""
          
            CASE "q"
                Suona "O4L16MSB"
                Automatico(1) = NOT (Automatico(1))

                Key$ = ""

            CASE "w"
                Suona "O4L16MSB"
                Verso(1) = NOT (Verso(1))
              
                IF Verso(1) = FALSE THEN
                    LOCATE 9, 40 + 24: PRINT "orario" + SPACE$(4)
                ELSEIF Verso(1) = TRUE THEN
                    LOCATE 9, 40 + 24: PRINT "antiorario" + SPACE$(4)
                END IF

                Key$ = ""

            CASE "d"
                COLOR 15, 1
                IF Incremento(1) <= 100000 THEN
                    Incremento(1) = Incremento(1) * 10
                    LOCATE 8, 40 + 23: PRINT STR$(Incremento(1)) + " ms" + SPACE$(6)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""
          
            CASE "c"
                COLOR 15, 1
                IF Incremento(1) > .1 THEN
                    Incremento(1) = Incremento(1) / 10
                    LOCATE 8, 40 + 23: PRINT STR$(Incremento(1)) + " ms" + SPACE$(6)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "s"
                COLOR 15, 1
                Ritardo(1) = Ritardo(1) + Incremento(1) * 10
                LOCATE 7, 40 + 19: PRINT STR$(Ritardo(1) / 10) + " ms" + SPACE$(10)
                Suona "O4L16MSB"
                Key$ = ""
         
            CASE "x"
                COLOR 15, 1
                IF Ritardo(1) > 0 AND Ritardo(1) - Incremento(1) * 10 >= 0 THEN
                    Ritardo(1) = Ritardo(1) - Incremento(1) * 10
                    LOCATE 7, 40 + 19: PRINT STR$(Ritardo(1) / 10) + " ms" + SPACE$(10)
                    Suona "O4L16MSB"
                ELSE
                    Suona "O3L16MSB"
                END IF
                Key$ = ""

            CASE "e"
                COLOR 15, 1
                CALL Azzeramento(1)
                CALL PInfor
                Automatico(1) = FALSE
                Incremento(1) = 10
                Ritardo(1) = 0
                TempoGiro(1) = 0
                Verso(1) = 0
                AggiornaInfo (1)
                LOCATE 9, 24: PRINT "orario" + SPACE$(4)
                LOCATE 7, 19: PRINT STR$(Ritardo(1) / 10) + " ms" + SPACE$(10)
                LOCATE 8, 23: PRINT STR$(Incremento(1)) + " ms" + SPACE$(6)
                Suona "O4L16MSBL10G"
                Key$ = ""

           
            CASE ""
 
            CASE ELSE
                Suona "O3L16MSB"
                Key$ = ""

        END SELECT

        TempoCicloWhile(CicloCorrente, 1) = TIMER + .01
    WEND

GOTO KeyInput

Errori:
    SELECT CASE ERR
       
        CASE 53, 62

            SELECT CASE LCASE$(Errore$)

                CASE "pinfor"

                    COLOR 14, 1: CLS
                    Center "Attendere. Configurazione in corso...", 12
                    Center "La configurazione potrebbe richiedere alcuni minuti", 13
                    Center "Durante la configurazione si consiglia di disconnettere i motori", 14

                    CLOSE
                    OPEN File$ FOR OUTPUT AS #1
                  
                    PRINT #1, 888
                    PRINT #1, -1
                    PRINT #1, -1
                    PRINT #1, 10
                    PRINT #1, 10
                    PRINT #1, Tempo(0)
                    PRINT #1, Tempo(1)
                    PRINT #1, -1
                    PRINT #1, -1
                    PRINT #1, 0
                    PRINT #1, 0
                    PRINT #1, -1
                    PRINT #1, -1

                    CLOSE
                    COLOR 15, 1: CLS

            END SELECT

        CASE 52

            SELECT CASE LCASE$(Errore$)

                CASE "pinfor"

                    OPEN File$ FOR INPUT AS #1

            END SELECT

        CASE ELSE

            GOSUB ErrMsg

    END SELECT

RESUME

ErrMsg:
    COLOR 1, 1: CLS
    COLOR 14, 12
    Finestra 6, 30, "Errore!"
    COLOR 14, 12
    Center "Errore numero:" + STR$(ERR), 9
    Center "Riga di errore:" + STR$(ERL), 10
    Center "Errore non riconosciuto.", 11
    Center "Impossibile proseguire.", 12
    Center "Il programma verr… terminato", 13

    END

'Aggiorna le informazioni relite allo stato dei motori. 0 Š il motore A, 1 Š
'il motore B e 2 sono tutti e due i motori
SUB AggiornaInfo (Motore)
    
    COLOR 15, 1

    IF Motore = 0 THEN GOSUB Motore1

    IF Motore = 1 THEN GOSUB Motore2

    IF Motore = 2 THEN
        GOSUB Motore1
        GOSUB Motore2
    END IF

    EXIT SUB

Motore1:
    IF TempoGiro(0) <> 0 THEN
        LOCATE 5, 13: PRINT STR$(INT(60 / TempoGiro(0) * 100) / 100) + " rpm" + SPACE$(10)
    ELSEIF TempoGiro(0) = 0 THEN
        LOCATE 5, 14: PRINT "0 rpm" + SPACE$(10)
    END IF
    LOCATE 6, 27: PRINT Sequenza$(0) + SPACE$(8)
    RETURN

Motore2:
    IF TempoGiro(1) <> 0 THEN
        LOCATE 5, 40 + 13: PRINT STR$(INT(60 / TempoGiro(1) * 100) / 100) + " rpm" + SPACE$(10)
    ELSEIF TempoGiro(1) = 0 THEN
        LOCATE 5, 40 + 14: PRINT "0 rpm" + SPACE$(10)
    END IF
    LOCATE 6, 40 + 27: PRINT Sequenza$(1) + SPACE$(8)
    RETURN


END SUB

'Questa sub azzera lo stato dei motori specificati. 0 Š il motore A, 1 Š il
'motore B e 2 sono tutti e due i motori
SUB Azzeramento (Motore)

    SELECT CASE Motore
        CASE 0
            Sequenza$(0) = "0001"
            Send 0, 1
            Send 0, 0

        CASE 1
            Sequenza$(1) = "0001"
            Send 0, 1
            Send 0, 0

        CASE 2
            Sequenza$(0) = "0001"
            Sequenza$(1) = "0001"
            Send 0, 1
            Send 0, 0

    END SELECT
END SUB

'Questa funzione converte un numero binario in un numero decimale. Serve per
'portare i bit di sequenza nel numero decimale da inviare alla porta
FUNCTION BinToDec (Numero$)

    b$ = LTRIM$(STR$(VAL(Numero$)))

    FOR x = 0 TO LEN(b$) - 1

        a$ = LEFT$(RIGHT$(Numero$, x + 1), 1)
        IF a$ <> "0" THEN Totale = Totale + VAL(a$) * 2 ^ x

    NEXT
   
    BinToDec = Totale

END FUNCTION

'Questa funzione calcola le varie sequenze dei bit a seconda del movimento
'selezionato, attende un certo tempo e poi prosegue
FUNCTION calcPosition$ (strPos$, Mov, Velocity)
    DIM Bit(3)

    FOR x = 1 TO LEN(strPos$)

        Bit(x - 1) = VAL(LEFT$(RIGHT$(strPos$, x), 1))

    NEXT

    FOR x = 0 TO (ABS(Velocity) * Compensazione(0) / 100 * 94): NEXT

    SELECT CASE Mov

        CASE -2

        CASE -1
            FOR x = 0 TO LEN(strPos$) - 1
             
                IF Bit(x) = 1 THEN

                    IF x = 0 THEN
                        New = LEN(strPos$) - 1
                    ELSE
                        New = x - 1
                    END IF

                    Bit(New) = 1
                    Bit(x) = 0

                    EXIT FOR

                END IF

            NEXT

        CASE 0, 1
            FOR x = 0 TO LEN(strPos$) - 1
            
                IF Bit(x) = 1 THEN
            
                    IF x = LEN(strPos$) - 1 THEN
                        New = 0
                    ELSEIF x <> LEN(strPos$) - 1 THEN
                        New = x + 1
                    END IF
            
                    Bit(New) = 1
                    Bit(x) = 0
            
                    EXIT FOR
            
                END IF
            
            NEXT

        CASE 2

    END SELECT

EsciFunzione:
    x = 0
    FOR x = 0 TO UBOUND(Bit)
        Nuova$ = LTRIM$(STR$(Bit(x))) + Nuova$
    NEXT

    calcPosition = Nuova$
END FUNCTION

'Centra un messaggio nello schermo
SUB Center (Text$, Linea)

    LOCATE Linea, 40 - (LEN(Text$) / 2) + 1
    PRINT Text$

END SUB

'Questa funzione permette di uscire dal programma
SUB Esci
    COLOR 14, 3
    CALL Finestra(6, 24, "Esci")
    COLOR 14, 3
   
    Center "Vuoi veramente uscire", 9
    Center "dal programma ?", 10
    Center "ESC=Annulla", 12
    Center "INVIO=Esci ", 13
    CALL Intestazioni("ESC=Annulla   INVIO=Esci")

    Suona "O4L24EFG"

    Key$ = ""
    WHILE Key$ = ""
        Key$ = INKEY$

        SELECT CASE Key$
            CASE CHR$(13)
                Suona "MBO0L32EFGEFDC"
                CALL Azzeramento(2)
                COLOR 7, 0
                CLS
                END

            CASE CHR$(27)
                Suona "O4L24FED"
                EXIT SUB

            CASE ELSE
                IF Key$ <> "" THEN Suona "O3L16MSB"
                Key$ = ""

        END SELECT

    WEND

END SUB

'Visualizza una finestra sullo schermo
SUB Finestra (Alt, Larg, Text$)
   
    DisH = (11 - Alt / 2)' + 1
    DisB = 40 - (Larg / 2)
    LOCATE DisH, DisB

    PRINT "É" + STRING$(Larg, "Í") + "»"
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "º" + STRING$(Larg, " ") + "º"
    NEXT
    LOCATE DisH + a, DisB
    PRINT "È" + STRING$(Larg, "Í") + "¼"
    
    LOCATE DisH, DisB + Larg / 10
    PRINT " " + Text$ + " "

    COLOR 0, 1
    
    a = 0
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB + Larg + 2
        'PRINT "Û"
        PRINT "ÛÛ"
    NEXT
    LOCATE DisH + a + 1, DisB + 2
    'PRINT STRING$(Larg + 1, "ß")
    PRINT STRING$(Larg + 2, "Û")

END SUB

'Scrive le intestazioni del programma
SUB Intestazioni (Descrizione$)
    COLOR 4, 7
    LOCATE 1, 1: PRINT SPACE$(80)
    Center "MotorController v1.0", 1

    COLOR 0, 3
    LOCATE 2, 1: PRINT SPACE$(80)
    LOCATE 2, 1: PRINT SPACE$(10) + "<Movimento>" + SPACE$(30) + "<Impostazioni>"

    COLOR 4, 7
    LOCATE 22, 1: PRINT SPACE$(80)
    LOCATE 22, 4: PRINT Descrizione$
END SUB

'Mi permette di muovermi all'interno del menu
SUB Menu
    DIM Posizione
    CALL Intestazioni(CHR$(27) + "=Indietro   " + CHR$(26) + "=Avanti   INVIO=Seleziona   ESC=Esci")

    Suona "O4L24EFG"

    GOTO Menu1

Tastiera:
    Key$ = ""
    WHILE Key$ = ""
        Key$ = INKEY$

        SELECT CASE Key$

            CASE CHR$(27)
                COLOR 0, 3
                LOCATE 2, 11: PRINT "<Movimento>"
                LOCATE 2, 52: PRINT "<Impostazioni>"

                Suona "O4L24FED"

                EXIT SUB

            CASE CHR$(0) + "M"
                Suona "O4L32MSB"
                IF Posizione = 1 THEN
                    GOTO Menu2
                ELSEIF Posizione = 2 THEN
                    GOTO Menu1
                END IF

            CASE CHR$(0) + "K"
                Suona "O4L32MSB"
                IF Posizione = 1 THEN
                    GOTO Menu2
                ELSEIF Posizione = 2 THEN
                    GOTO Menu1
                END IF

            CASE ELSE
                IF Key$ <> "" THEN Suona "O3L16MSB"
                Key$ = ""

        END SELECT

    WEND
    GOTO Tastiera

Menu1:
    Posizione = 1
    COLOR 14, 3
    LOCATE 2, 11: PRINT CHR$(17) + "Movimento" + CHR$(16)

    COLOR 0, 3
    LOCATE 2, 52: PRINT "<Impostazioni>"
    
    GOTO Tastiera

Menu2:
    Posizione = 2
    COLOR 14, 3
    LOCATE 2, 52: PRINT CHR$(17) + "Impostazioni" + CHR$(16)

    COLOR 0, 3
    LOCATE 2, 11: PRINT "<Movimento>"
    
    GOTO Tastiera

END SUB

'Scrive la pagina principale di lavoro e, se specificato, le scritte della pagina
'relativa
SUB Pagina (Text$, Tipo)
    
    DisH = 3
    DisB = 1
    Larg = 78
    Alt = 18

    LOCATE DisH, DisB

    PRINT "É" + STRING$(Larg, "Í") + "»"
    FOR x = 1 TO Alt
        a = a + 1
        LOCATE DisH + a, DisB
        PRINT "º" + STRING$(Larg, " ") + "º"
    NEXT
    LOCATE DisH + a, DisB
    PRINT "È" + STRING$(Larg, "Í") + "¼"

    IF Tipo = 0 THEN

        LOCATE 3, 40: PRINT "Ñ"
        FOR x = 1 TO Alt - 1
            LOCATE 3 + x, 40
            PRINT "³"
        NEXT
        LOCATE 21, 40: PRINT "Ï"
       
        LOCATE 4, 3: PRINT "Motore A" + SPACE$(10)
   
        LOCATE 5, 4: PRINT "Velocit…: 0 rpm" + SPACE$(10)
        LOCATE 6, 4: PRINT "Sequenza di rotazione: " + Sequenza$(0) + SPACE$(8)
        LOCATE 7, 4: PRINT "Ritardo bobina:" + STR$(Ritardo(0) / 10) + " ms " + SPACE$(10)
        LOCATE 8, 4: PRINT "Incremento ritardo:" + STR$(Incremento(0)) + " ms" + SPACE$(4)
        IF Verso(0) = FALSE THEN
            LOCATE 9, 4: PRINT "Senso di rotazione: orario" + SPACE$(4)
        ELSEIF Verso(0) = TRUE THEN
            LOCATE 9, 4: PRINT "Senso di rotazione: antiorario" + SPACE$(4)
        END IF

        LOCATE 11, 3: PRINT "Comandi da tastiera"
        LOCATE 12, 4: PRINT "4-Avanti           1-Indietro"
        LOCATE 13, 4: PRINT "5-Aum. ritardo     2-Dim. ritardo"
        LOCATE 14, 4: PRINT "6-Aum. incremento  3-Dim. incremento"
        LOCATE 15, 4: PRINT "7-Ciclo continuo ON/OFF  8-Verso"
        LOCATE 16, 4: PRINT "9-Resetta motore"
        LOCATE 17, 4: PRINT "Tasti funzione da F1 a F4"

        LOCATE 4, 43: PRINT "Motore B" + SPACE$(10)
      
        LOCATE 5, 44: PRINT "Velocit…: 0 rpm" + SPACE$(10)
        LOCATE 6, 44: PRINT "Sequenza di rotazione: " + Sequenza$(1) + SPACE$(8)
        LOCATE 7, 44: PRINT "Ritardo bobina:" + STR$(Ritardo(1) / 10) + " ms " + SPACE$(10)
        LOCATE 8, 44: PRINT "Incremento ritardo:" + STR$(Incremento(1)) + " ms" + SPACE$(4)
        IF Verso(1) = FALSE THEN
            LOCATE 9, 44: PRINT "Senso di rotazione: orario" + SPACE$(4)
        ELSEIF Verso(1) = TRUE THEN
            LOCATE 9, 44: PRINT "Senso di rotazione: antiorario" + SPACE$(4)
        END IF

        LOCATE 11, 43: PRINT "Comandi da tastiera"
        LOCATE 12, 44: PRINT "A-Avanti           Z-Indietro"
        LOCATE 13, 44: PRINT "S-Aum. ritardo     X-Dim. ritardo"
        LOCATE 14, 44: PRINT "D-Aum. incremento  C-Dim. incremento"
        LOCATE 15, 44: PRINT "Q-Ciclo continuo ON/OFF  W-Verso"
        LOCATE 16, 44: PRINT "E-Resetta motore"
        LOCATE 17, 44: PRINT "Tasti funzione da F5 a F8"

        CALL AggiornaInfo(2)

    END IF

    Center " " + Text$ + " ", DisH
END SUB

'Preleva le informazioni di configurazione da un file esterno
SUB PInfor
    ON ERROR GOTO Errori

    Errore$ = "PInfor"
    OPEN File$ FOR INPUT AS #1

    INPUT #1, Porta
    INPUT #1, Logica(0)
    INPUT #1, Logica(1)
    INPUT #1, Ritardo(0)
    INPUT #1, Ritardo(0)
    INPUT #1, Compensazione(0)
    INPUT #1, Compensazione(1)
    INPUT #1, Intro
    INPUT #1, Music
    INPUT #1, Verso(0)
    INPUT #1, Verso(1)
    INPUT #1, Conteggio(0)
    INPUT #1, Conteggio(1)
    CLOSE
    
    Errore$ = ""

    ON ERROR GOTO 0
END SUB

'Spedisce sulle porte i valori giusti per la sequenza dei motori. Tiene conto
'della logica in cui si sta lavorando e di ogni singolo motore a partire da
'due sequenze di bit separate
SUB Send (Dati, Tipo)

    IF Tipo <> 0 THEN
     
        Dati = BinToDec(Sequenza$(1) + Sequenza$(0))

    END IF

    IF Logica = 0 THEN
    
        Dati = 255 - Dati
    
    END IF

    OUT Porta, Dati
END SUB

'Salva le informazioni dentro il file di configurazione
SUB SInfor
    OPEN File$ FOR OUTPUT AS #1
   
    PRINT #1, Porta
    PRINT #1, Logica
    PRINT #1, Ritardo(0)
    PRINT #1, Ritardo(0)
    PRINT #1, Compensazione
    PRINT #1, Intro
    PRINT #1, Music
    PRINT #1, Verso(0)
    PRINT #1, Verso(1)

    CLOSE
END SUB

SUB Suona (Suono$)
   
    IF Music = TRUE THEN

        PLAY Suono$

    END IF

END SUB

'Permette di standardizzare il tempo di un ciclo secondo le prestazioni del
'computer su cui sta girando il programma
FUNCTION Tempo (Tipo)
   
    IF Tipo = 1 THEN
        DIM TempoImpiegato(5)
        DIM TempoMedio

        Misure = 80

        REDIM Valori$(Misure)

        FOR x = 1 TO Misure
            RANDOMIZE TIMER
            FOR y = 1 TO 4
                Valori$(x) = Valori$(x) + LTRIM$(STR$(INT(RND(1) * 2)))
           NEXT
        NEXT
      
        Sequenza$(1) = "0001"
        Porta = &H378

        FOR y = 1 TO UBOUND(TempoImpiegato)

            TempoIniziale = TIMER
            FOR x = 1 TO Misure
              
                Casuale = INT(RND(1) * 2)
                Sequenza$(Casuale) = Valori$(INT(RND(1) * Misure) + 1)
                Sequenza$(ABS(NOT (-Casuale))) = calcPosition(Sequenza$(1), 1, 0)
                Send 0, 1

            NEXT
            TempoFinale = TIMER

            TempoImpiegato(y) = TempoFinale - TempoIniziale
        NEXT

         FOR x = 1 TO UBOUND(TempoImpiegato)
            tmp = tmp + TempoImpiegato(x)
        NEXT
        TempoMedio = tmp / UBOUND(TempoImpiegato)
        TempoMedio = TempoMedio / (Misure / 4)

        Costante = TempoMedio
       
        Scarto = 2
          
        WHILE Scarto > 1

            TempoIniziale = TIMER
            FOR x = 1 TO 40 * Costante
                  
                'Casuale = INT(RND(1) * 2)
                'Sequenza$(Casuale) = Valori$(INT(RND(1) * Misure) + 1)
                'Sequenza$(ABS(NOT (-Casuale))) = calcPosition(Sequenza$(1), 1, 0)
                Sequenza$(0) = Valori$(INT(RND(1) * Misure) + 1)
                Send 0, 1

            NEXT
            TempoFinale = TIMER

            Scarto = (1 - (TempoFinale - TempoIniziale)) / 1
            Costante = Costante + (Costante * Scarto / 100)

        WEND

        Tempo = Costante
    ELSE

        DIM a(5)
    
        aa = TIMER
        FOR x = 1 TO UBOUND(a)
            a = a + 1
            b = TIMER
            FOR y = 1 TO 10000: NEXT
            C = TIMER
            a(a) = 1 / (C - b)
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
    
        Scarto = (10 - TT) * 10
        TM = TM + (TM * Scarto / 100)
    
        Tempo = TM
    END IF

END FUNCTION

