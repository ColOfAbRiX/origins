CLS
CLOSE
OPEN "COM2:75,N,8,1,CD0,CS0,DS0,OP0,RS,TB2048,RB2048" FOR RANDOM AS #1
ON COM(2) GOSUB Porta
COM(2) ON
                                                                              
Inizio:                                                                     
    SLEEP 1
    PRINT #1, CHR$(160) + CHR$(100) + CHR$(190) + CHR$(130) ' d¾‚
    SLEEP 10
    GOTO Lavoro
                                                                              
Porta:                                                                      
    IF Conn = 0 THEN
        INPUT #1, a$
        PRINT "Nome del Software="; a$
        INPUT #1, b$
        PRINT "Nome di chi lo usa="; b$
        INPUT #1, c$
        PRINT "Velocit…="; c$
        SLEEP 1
        PRINT #1, "7351 a.C."
        SLEEP 1
        PRINT #1, "Scherzo !!"
        SLEEP 1
        PRINT #1, "AHAH"
        SLEEP 1
        PRINT #1, "AHAH"
        CLS
        PRINT "Connesso"
        Conn = 1
        CLOSE
        OPEN "COM2:9600,N,8,1,CD0,CS0,DS0,OP0,RS,TB10000,RB10240" FOR RANDOM AS #1
        ON COM(2) GOSUB Porta
        COM(2) ON
    ELSE
        INPUT #1, a$
        IF a$ <> "File" AND f = 0 THEN
            LOCATE 2 + a: PRINT b$ + ": " + a$
            a = a + 1
            SLEEP 1
        ELSE
            f = 1
            PRINT a$
            IF a$ = "Fine" THEN f = 0
        END IF
    END IF
    RETURN
                                                                              
Lavoro:                                                                     
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(13)
        PRINT #1, aa$
            PRINT #1, aa$
            aa$ = ""
            a = a + 1
        CASE CHR$(8)
            aa$ = LEFT$(aa$, LEN(aa$) - 1)
        CASE ELSE
            aa$ = aa$ + Tasti$
        END SELECT
        LOCATE 2 + a: PRINT aa$ + SPACE$(80)
GOTO Lavoro
                                                                          

