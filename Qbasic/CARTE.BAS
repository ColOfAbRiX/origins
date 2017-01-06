DECLARE SUB PInfor ()
DECLARE SUB CarteS ()
DECLARE SUB Gioco ()
DECLARE SUB CarteC ()
SCREEN 0
CLS
DIM SHARED Carte(3)
DIM SHARED a$(1)
DIM SHARED a(1)
DIM SHARED b(1)
DIM SHARED c(1)
DIM SHARED Info$(16)
PInfor
CLOSE
Porta$ = "COM" + Info$(9) + ":4800," + Info$(6) + "," + Info$(7) + "," + Info$(8) + ",CD0,CS0,DS0,OP0,RS,TB2048,RB2048"
OPEN Porta$ FOR RANDOM AS #2
COM(VAL(Info$(9))) ON
CarteC
Gioco

SUB CarteC
    LOCATE 2: PRINT "Per uscire scrivere 1234 e premere INVIO"
    COLOR 14, 3
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 12 - 3
        PRINT " 1" + STRING$(12, " ") + "1 "
    NEXT
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 36 - 3
        PRINT " 2" + STRING$(12, " ") + "2 "
    NEXT
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 60 - 3
        PRINT " 3" + STRING$(12, " ") + "3 "
    NEXT
END SUB

SUB CarteS
    LOCATE 2: PRINT "Per uscire scrivere 1234 e premere INVIO"
    COLOR 0, 7
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 12 - 3
        PRINT STRING$(16, " ")
    NEXT
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 36 - 3
        PRINT STRING$(16, " ")
    NEXT
    a = 6
    FOR x = 1 TO 10
        a = a + 1
        LOCATE a, 60 - 3
        PRINT STRING$(16, " ")
    NEXT
    IF a(1) = 1 THEN
        LOCATE 11, 16
        PRINT "Asso"
    END IF
    IF b(1) = 1 THEN
        LOCATE 11, 17 + 22
        PRINT "Asso"
    END IF
    IF c(1) = 1 THEN
        LOCATE 11, 15 + 22 + 25
        PRINT "Asso"
    END IF

END SUB

SUB Gioco
    COLOR 12, 0
    RANDOMIZE TIMER
    a(1) = INT((RND(1) * 3) + 1)
    b(1) = INT((RND(1) * 3) + 1)
    WHILE b(1) = a(1): b(1) = INT((RND(1) * 3) + 1): WEND
    c(1) = INT((RND(1) * 3) + 1)
    WHILE c(1) = b(1) OR c(1) = a(1): c(1) = INT((RND(1) * 3) + 1): WEND
    IF a$(1) = "1" THEN
        INPUT "In quale carta c'Š l'asso "; Carta
        IF Carta = 1234 THEN
            PRINT #2, 1234
            RUN "comuni22"
        END IF
        IF Carta > 3 THEN RUN
        INPUT "Quanto punti($) "; Punti
        IF Punti = 1234 THEN
            PRINT #2, 1234
            RUN "comuni22"
        END IF
        SLEEP 1
        PRINT #2, Carta
        SLEEP 1
        PRINT #2, Punti
        INPUT #2, Carta1
        INPUT #2, Punti1
        PRINT "L'avversazio punta "; Punti; " sulla carta "; Carta1
    ELSE
        INPUT #2, Carta1
        IF Carta1 = 1234 THEN RUN "comuni22.bas"
        INPUT #2, Punti1
        IF Carta1 = 1234 THEN RUN "comuni22.bas"
        PRINT "L'avversazio punta "; Punti1; " sulla carta "; Carta1
        INPUT "In quale carta c'Š l'asso "; Carta
        IF Carta > 3 THEN RUN
        INPUT "Quanto punti($) "; Punti
        SLEEP 1
        PRINT #2, Carta
        SLEEP 1
        PRINT #2, Punti
    END IF
    IF Carta = 1 AND Carta1 = 2 THEN Carta2 = 3
    IF Carta = 1 AND Carta1 = 3 THEN Carta2 = 2
    IF Carta = 2 AND Carta1 = 1 THEN Carta2 = 3
    IF Carta = 2 AND Carta1 = 3 THEN Carta2 = 1
    IF Carta = 3 AND Carta1 = 2 THEN Carta2 = 1
    IF Carta = 3 AND Carta1 = 1 THEN Carta2 = 2
    Punti2 = 100
    PRINT "In quale carta c'Š l'asso "; Carta2
    PRINT "Quanto punti($) "; Punti2

    IF Carta = 1 THEN
        IF a(1) = 1 THEN
            Vinci = Punti1 + Punti2
            Vinci1 = 0
            Vinci2 = 0
        ELSE
            Vinci = 0
        END IF
    END IF
    IF Carta = 2 THEN
        IF b(1) = 1 THEN
            Vinci = Punti1 + Punti2
            Vinci1 = 0
            Vinci2 = 0
        ELSE
            Vinci = 0
        END IF
    END IF
    IF Carta = 3 THEN
        IF c(1) = 1 THEN
            Vinci = Punti1 + Punti2
            Vinci1 = 0
            Vinci2 = 0
        ELSE
            Vinci = 0
        END IF
    END IF

    IF Carta1 = 1 THEN
        IF a(1) = 1 THEN
            Vinci1 = Punti1 + Punti2
            Vinci = 0
            Vinci2 = 0
        ELSE
            Vinci1 = 0
        END IF
    END IF
    IF Carta1 = 2 THEN
        IF b(1) = 1 THEN
            Vinci1 = Punti1 + Punti2
            Vinci = 0
            Vinci2 = 0
        ELSE
            Vinci1 = 0
        END IF
    END IF
    IF Carta1 = 3 THEN
        IF c(1) = 1 THEN
            Vinci1 = Punti1 + Punti2
            Vinci = 0
            Vinci2 = 0
        ELSE
            Vinci1 = 0
        END IF
    END IF
   
    IF Carta2 = 1 THEN
        IF a(1) = 1 THEN
            Vinci2 = Punti1 + Punti2
            Vinci1 = 0
            Vinci = 0
        ELSE
            Vinci2 = 0
        END IF
    END IF
    IF Carta2 = 2 THEN
        IF b(1) = 1 THEN
            Vinci2 = Punti1 + Punti2
            Vinci1 = 0
            Vinci = 0
        ELSE
            Vinci2 = 0
        END IF
    END IF
    IF Carta2 = 3 THEN
        IF c(1) = 1 THEN
            Vinci2 = Punti1 + Punti2
            Vinci1 = 0
            Vinci = 0
        ELSE
            Vinci2 = 0
        END IF
    END IF
      
    CarteS
    LOCATE 1
    IF Vinci <> 0 THEN PRINT "Il primo giocatore ha vinto con $"; Vinci
    IF Vinci1 <> 0 THEN PRINT "Il secondo giocatore ha vinto con$"; Vinci1
    IF Vinci2 <> 0 THEN PRINT "Il terzo giocatore ha vinto con$"; Vinci2
    SLEEP 10
    RUN
END SUB

SUB PInfor
    OPEN "info2.lsc" FOR INPUT AS #1
    a = 0
    WHILE EOF(1) = 0
        a = a + 1
        LINE INPUT #1, Info$(a)
    WEND
    CLOSE
    OPEN "temp.lsc" FOR INPUT AS #1: INPUT #1, a$(1): CLOSE
END SUB

