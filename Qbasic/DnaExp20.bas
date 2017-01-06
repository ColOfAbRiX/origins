DECLARE SUB Esecuzione ()
DECLARE SUB Colori ()
DECLARE SUB Assegnazione ()
DECLARE SUB Finestra (LargP, AltP)
DECLARE SUB Center (AltP, Text$, Cl)
DECLARE SUB Sinistra (AltP, Text$, Cl)
DECLARE SUB Immissione (AltP, Text$, Cl)
DECLARE SUB Expander (DaFile$, InFile$)
DECLARE SUB Barra (Valore, Percentuale)

REM    D N A  E X P A N D E R

CLS
SCREEN 0
'ON ERROR GOTO Errori

'$STATIC
DIM SHARED ParoleC$(250)
DIM SHARED Cl(7)

'$DYNAMIC
DIM SHARED Dna$(2, 700)
CONST Ascii = 256
CONST Lsch = 80
CONST Asch = 22
CONST LargP = 60
CONST AltP = 8
CONST AltezzaP = ((Asch / 2) - (AltP / 2))
      
       'Scritte Sfondo Bordo Errore Barre BordoExt BarreDis
Col:
    DATA   15,     1,     7,    14,    14,     0,     4
BN:
    DATA    7,     0,     7,     4,     7,     0,     0

Assegnazione
Colori
COLOR Cl(1), Cl(2)
Finestra LargP, AltP
Center AltezzaP, " DNA Expander 2.0 ", 2
Esecuzione
END

Errori:
    COLOR Cl(5), Cl(6)
    CLS
    COLOR Cl(1), Cl(7)
    Finestra 26, 4
    Center AltezzaP + 3, "Errore numero:" + STR$(ERR), 4
    SELECT CASE ERR
        CASE IS = 52, IS = 53: TipoErr$ = "File non trovato"
        CASE IS = 61, IS = 71, IS = 72: TipoErr$ = "Disco pieno o rovinato"
        CASE 76: TipoErr$ = "Percorso non trovato"
        CASE ELSE: TipoErr$ = "Errore generico"
    END SELECT
    Center AltezzaP + 4, TipoErr$, 4
    Center AltezzaP + 5, "Riavviare DNA 2.0", 4
END
RESUME

REM $STATIC
SUB Assegnazione STATIC

    FOR x = 1 TO UBOUND(Dna$, 2)
        a = a + 1
        Dna$(1, a) = LTRIM$(STR$(a))
    NEXT
    a = 0

     'Assegnazione delle parole chiave
    Dna$(2, 1) = "ABS"
    Dna$(2, 2) = "ABSOLUTE"
    Dna$(2, 3) = "ACCES"
    Dna$(2, 4) = "AND"
    Dna$(2, 5) = "ANY"
    Dna$(2, 6) = "APPEND"
    Dna$(2, 7) = "AS"
    Dna$(2, 8) = "ASC"
    Dna$(2, 9) = "ATN"
    Dna$(2, 10) = "BASE"
    Dna$(2, 11) = "BASIC"
    Dna$(2, 12) = "BEEP"
    Dna$(2, 13) = "BINARY"
    Dna$(2, 14) = "BLOAD"
    Dna$(2, 15) = "BSAVE"
    Dna$(2, 16) = "CALL"
    Dna$(2, 17) = "CALL ABSOLUTE"
    Dna$(2, 18) = "CASE"
    Dna$(2, 19) = "CDBL"
    Dna$(2, 20) = "CHAIN"
    Dna$(2, 21) = "CHDIR"
    Dna$(2, 22) = "CHR$"
    Dna$(2, 23) = "CINT"
    Dna$(2, 24) = "CIRCLE"
    Dna$(2, 25) = "CLEAR"
    Dna$(2, 26) = "CLNG"
    Dna$(2, 27) = "CLOSE"
    Dna$(2, 28) = "CLS"
    Dna$(2, 29) = "COLOR"
    Dna$(2, 30) = "COM"
    Dna$(2, 31) = "COMMON"
    Dna$(2, 32) = "CONST"
    Dna$(2, 33) = "COS"
    Dna$(2, 34) = "CSNG"
    Dna$(2, 35) = "CSRLIN"
    Dna$(2, 36) = "CVD"
    Dna$(2, 37) = "CVDMBF"
    Dna$(2, 38) = "CVI"
    Dna$(2, 39) = "CVL"
    Dna$(2, 40) = "CVS"
    Dna$(2, 41) = "CVSMBF"
    Dna$(2, 42) = "DATA"
    Dna$(2, 43) = "DATE$"
    Dna$(2, 44) = "DECLARE"
    Dna$(2, 45) = "DEF FN"
    Dna$(2, 46) = "DEF SEG"
    Dna$(2, 47) = "DEFDBL"
    Dna$(2, 48) = "DEFINIT"
    Dna$(2, 49) = "DEFLNG"
    Dna$(2, 50) = "DEFSNG"
    Dna$(2, 51) = "DEFSTR"
    Dna$(2, 52) = "DIM"
    Dna$(2, 53) = "DO"
    Dna$(2, 54) = "DOUBLE"
    Dna$(2, 55) = "DRAW"
    Dna$(2, 56) = "'$DYNAMIC"
    Dna$(2, 57) = "ELSE"
    Dna$(2, 58) = "ELSEIF"
    Dna$(2, 59) = "END"
    Dna$(2, 60) = "ENVIRON"
    Dna$(2, 61) = "ENVIRON$"
    Dna$(2, 62) = "EOF"
    Dna$(2, 63) = "EQV"
    Dna$(2, 64) = "ERASE"
    Dna$(2, 65) = "ERDEV"
    Dna$(2, 66) = "ERDEV$"
    Dna$(2, 67) = "ERL"
    Dna$(2, 68) = "ERR"
    Dna$(2, 69) = "ERROR"
    Dna$(2, 70) = "EXIT"
    Dna$(2, 71) = "EXP"
    Dna$(2, 72) = "FIELD"
    Dna$(2, 73) = "FILEATTR"
    Dna$(2, 74) = "FILES"
    Dna$(2, 75) = "FIX"
    Dna$(2, 76) = "FOR"
    Dna$(2, 77) = "FRE"
    Dna$(2, 78) = "FREEFILE"
    Dna$(2, 79) = "FUNCTION"
    Dna$(2, 80) = "GET"
    Dna$(2, 81) = "GOSUB"
    Dna$(2, 82) = "GOTO"
    Dna$(2, 83) = "HEX$"
    Dna$(2, 84) = "IF"
    Dna$(2, 85) = "IMP"
    Dna$(2, 86) = "INP"
    Dna$(2, 87) = "INPUT"
    Dna$(2, 88) = "INPUT$"
    Dna$(2, 89) = "INSTR"
    Dna$(2, 90) = "INT"
    Dna$(2, 91) = "INTEGER"
    Dna$(2, 92) = "IOCTIL"
    Dna$(2, 93) = "IOCTIL$"
    Dna$(2, 94) = "IS"
    Dna$(2, 95) = "KEY"
    Dna$(2, 96) = "KILL"
    Dna$(2, 97) = "LBOUND"
    Dna$(2, 98) = "LCASE"
    Dna$(2, 99) = "LEFT$"
    Dna$(2, 100) = "LEN"
    Dna$(2, 101) = "LET"
    Dna$(2, 102) = "LINE"
    Dna$(2, 103) = "LINE INPUT"
    Dna$(2, 104) = "LIST"
    Dna$(2, 105) = "LOC"
    Dna$(2, 106) = "LOCATE"
    Dna$(2, 107) = "LOCK"
    Dna$(2, 108) = "LOF"
    Dna$(2, 109) = "LOG"
    Dna$(2, 110) = "LONG"
    Dna$(2, 111) = "LOOP"
    Dna$(2, 112) = "LPOS"
    Dna$(2, 113) = "LPRITNT"
    Dna$(2, 114) = "LPRINT USING"
    Dna$(2, 115) = "LSET"
    Dna$(2, 116) = "LTRIM$"
    Dna$(2, 117) = "MID$"
    Dna$(2, 118) = "MKD$"
    Dna$(2, 119) = "MKDIR"
    Dna$(2, 120) = "MKDMBF$"
    Dna$(2, 121) = "MOD"
    Dna$(2, 122) = "NAME"
    Dna$(2, 123) = "NEXT"
    Dna$(2, 124) = "NOT"
    Dna$(2, 125) = "OCT$"
    Dna$(2, 126) = "OFF"
    Dna$(2, 127) = "ON"
    Dna$(2, 128) = "OPEN"
    Dna$(2, 129) = "OPTION BASE"
    Dna$(2, 130) = "OR"
    Dna$(2, 131) = "OUT"
    Dna$(2, 132) = "OUTPUT"
    Dna$(2, 133) = "PAINT"
    Dna$(2, 134) = "PALETTE"
    Dna$(2, 135) = "PCOPY"
    Dna$(2, 136) = "PEEK"
    Dna$(2, 137) = "PEN"
    Dna$(2, 138) = "PLAY"
    Dna$(2, 139) = "PMAP"
    Dna$(2, 140) = "POINT"
    Dna$(2, 141) = "POKE"
    Dna$(2, 142) = "POS"
    Dna$(2, 143) = "PRESET"
    Dna$(2, 144) = "PRINT"
    Dna$(2, 145) = "PRINT USING"
    Dna$(2, 146) = "PSET"
    Dna$(2, 147) = "PUT"
    Dna$(2, 148) = "RANDOM"
    Dna$(2, 149) = "RANDOMIZE"
    Dna$(2, 150) = "READ"
    Dna$(2, 151) = "REDIM"
    Dna$(2, 152) = "REM"
    Dna$(2, 153) = "RESET"
    Dna$(2, 154) = "RESTORE"
    Dna$(2, 155) = "RESUME"
    Dna$(2, 156) = "RETURN"
    Dna$(2, 157) = "RIGHT$"
    Dna$(2, 158) = "RMDIR"
    Dna$(2, 159) = "RND"
    Dna$(2, 160) = "RSET"
    Dna$(2, 161) = "RTRIM$"
    Dna$(2, 162) = "RUN"
    Dna$(2, 163) = "SCREEN"
    Dna$(2, 164) = "SEEK"
    Dna$(2, 165) = "SELECT CASE"
    Dna$(2, 166) = "SGN"
    Dna$(2, 167) = "SHARED"
    Dna$(2, 168) = "SHELL"
    Dna$(2, 169) = "SIN"
    Dna$(2, 170) = "SINGLE"
    Dna$(2, 171) = "SLEEP"
    Dna$(2, 172) = "SOUND"
    Dna$(2, 173) = "SPACE$"
    Dna$(2, 174) = "SPC"
    Dna$(2, 175) = "SQR"
    Dna$(2, 176) = "STATIC"
    Dna$(2, 177) = "'$STATIC"
    Dna$(2, 178) = "STEP"
    Dna$(2, 179) = "STICK"
    Dna$(2, 180) = "STOP"
    Dna$(2, 181) = "STR$"
    Dna$(2, 182) = "STRIG"
    Dna$(2, 183) = "STRING"
    Dna$(2, 184) = "STRING$"
    Dna$(2, 185) = "SUB"
    Dna$(2, 186) = "SWAP"
    Dna$(2, 187) = "SYSTEM"
    Dna$(2, 188) = "TAB"
    Dna$(2, 189) = "TAN"
    Dna$(2, 190) = "THEN"
    Dna$(2, 191) = "TIME$"
    Dna$(2, 192) = "TIMER"
    Dna$(2, 193) = "TO"
    Dna$(2, 194) = "TROFF"
    Dna$(2, 195) = "TRON"
    Dna$(2, 196) = "TYPE"
    Dna$(2, 197) = "UBOUND"
    Dna$(2, 198) = "UCASE$"
    Dna$(2, 199) = "UNLOCK"
    Dna$(2, 200) = "UNTIL"
    Dna$(2, 201) = "USING"
    Dna$(2, 202) = "VAL"
    Dna$(2, 203) = "VARPTR"
    Dna$(2, 204) = "VARPTR$"
    Dna$(2, 205) = "VARSEG"
    Dna$(2, 206) = "VIEW"
    Dna$(2, 207) = "VIEW PRINT"
    Dna$(2, 208) = "WAIT"
    Dna$(2, 209) = "WEND"
    Dna$(2, 210) = "WHILE"
    Dna$(2, 211) = "WIDTH"
    Dna$(2, 212) = "WINDOW"
    Dna$(2, 213) = "WRITE"
    Dna$(2, 214) = "XOR"
           
END SUB

SUB Barra (Valore, Percentuale)
    IF Valore < 100 THEN Grandezza = 2400
    IF Valore < 300 AND Valore > 100 THEN Grandezza = 1200
    IF Valore < 600 AND Valore > 300 THEN Grandezza = 600
    IF Valore < 1200 AND Valore > 600 THEN Grandezza = 300
    IF Valore < 3000 AND Valore > 1200 THEN Grandezza = 150
    IF Valore < 3000 AND Valore > 5000 THEN Grandezza = 1
    IF Valore < 3000 THEN Grandezza = .02
    FOR x = 1 TO (Valore / 50) * Grandezza
    NEXT
    LungBarra = (Percentuale / Valore) * 50
    LOCATE AltezzaP + 6, (Lsch / 2) - ((LargP - 4) / 2) + 3
    COLOR Cl(5), Cl(4)
    PRINT STRING$(LungBarra, " ")
    COLOR Cl(1), Cl(2)
    Center AltezzaP + 7, "   " + STR$(CINT(LungBarra * 2)) + "%   ", 2
END SUB

SUB Center (AltP, Text$, Cl)
    Larghezza = (Lsch / 2) - (LEN(Text$) / 2)
    LOCATE AltP, Larghezza
    COLOR Cl(1), Cl(Cl)
    PRINT Text$
END SUB

SUB Colori
    OPEN "Info.dna" FOR INPUT AS #1
        INPUT #1, SceltaCol$
    CLOSE #1
    IF SceltaCol$ = "BN" THEN
        RESTORE BN
        READ Cl(1), Cl(2), Cl(3), Cl(4), Cl(5), Cl(6), Cl(7)
    END IF
    IF SceltaCol$ = "CL" THEN
        RESTORE Col
        READ Cl(1), Cl(2), Cl(3), Cl(4), Cl(5), Cl(6), Cl(7)
    END IF
    COLOR Cl(1), Cl(6)
    CLS
END SUB

SUB Disinst

END SUB

SUB Esecuzione
    STATIC DaEx$, Ex$
    F1$ = "Nome del file da espandere: "
    F2$ = "Nome del file espanso: "
    F3$ = SPACE$(50)
    Center AltezzaP + 6, F3$, 1
    Center AltezzaP + 7, "0%", 2
    Sinistra AltezzaP + 2, F1$, 2
    Sinistra AltezzaP + 4, F2$, 2
    LOCATE AltezzaP + 2, (Lsch / 2) - ((LargP - 4) / 2) + LEN(F1$)
    INPUT DaEx$
    LOCATE AltezzaP + 2, (Lsch / 2) - ((LargP - 4) / 2) + LEN(F1$)
    PRINT DaEx$ + "    "
    LOCATE AltezzaP + 4, (Lsch / 2) - ((LargP - 4) / 2) + LEN(F2$)
    INPUT Ex$
    LOCATE AltezzaP + 4, (Lsch / 2) - ((LargP - 4) / 2) + LEN(F2$)
    PRINT Ex$ + "    "
    Expander DaEx$, Ex$
    SLEEP 2
    COLOR Cl(1), Cl(6): CLS
    COLOR Cl(1), Cl(2)
    Finestra 30, 4
    OPEN DaEx$ FOR INPUT AS #1: GDaEx$ = STR$(LOF(1)) + " Bytes": CLOSE
    OPEN Ex$ FOR INPUT AS #1: GEx$ = STR$(LOF(1)) + " Bytes": CLOSE
    Center AltezzaP + 3, " Espansione terminata", 2
    Center AltezzaP + 4, GDaEx$ + " ->" + GEx$, 2
    Center AltezzaP + 5, STR$(CINT(((VAL(GEx$) - VAL(GDaEx$)) / VAL(GDaEx$)) * 100)) + "%", 2
    SLEEP 8
    RUN
END SUB

SUB Expander (DaFile$, InFile$)
    OPEN DaFile$ FOR INPUT AS #1
    OPEN InFile$ FOR OUTPUT AS #2
        File = LOF(1)
        WHILE EOF(1) = 0
            LINE INPUT #1, a$
            Fatto = Fatto + LEN(a$)
            a = 0
            FOR y = 1 TO LEN(a$) / 3
                a = a + 3
                b$ = RIGHT$(LEFT$(a$, a), 3)
                b = VAL(b$)
                IF b = 0 THEN EXIT FOR
                IF b < Ascii THEN
                    IF b = 13 THEN
                        PRINT #2, Frase$
                        Frase$ = ""
                    ELSE
                        Frase$ = Frase$ + CHR$(b)
                    END IF
                END IF
                c = 0
                IF b >= Ascii THEN
                    FOR x = 1 TO UBOUND(Dna$, 2)
                        c = c + 1
                        IF b - Ascii = VAL(Dna$(1, c)) THEN Frase$ = Frase$ + Dna$(2, c) + CHR$(32)
                    NEXT
                END IF
                Barra File, Fatto + 2
            NEXT
        WEND
    CLOSE
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
    COLOR Cl(3), Cl(6)
    FOR x = 1 TO AltP
        a = a + 1
        LOCATE DisH + a, DisB + LargP + 2
        PRINT "Û"
    NEXT
    LOCATE DisH + a + 1, DisB + 2
    PRINT STRING$(LargP + 1, "ß")
END SUB

SUB Sinistra (AltP, Text$, Cl)
    Larghezza = (Lsch / 2) - ((LargP - 4) / 2)
    LOCATE AltP, Larghezza
    COLOR Cl(1), Cl(Cl)
    PRINT Text$
END SUB

