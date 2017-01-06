DECLARE SUB Quanti ()
DECLARE SUB OndeEm ()
DECLARE SUB Costanti ()
DECLARE SUB Hubble ()
DECLARE SUB Gravita ()
DECLARE SUB Varie ()
DECLARE SUB Gravitazione ()
DECLARE SUB Keplero ()
DECLARE SUB Stefan ()
DECLARE SUB Wien ()
DECLARE SUB Doppler ()
DECLARE SUB Distanza ()
DECLARE SUB mapp ()
DECLARE SUB MASS ()
DECLARE SUB enemas ()
DECLARE SUB LHubble ()
DECLARE SUB Leve ()
DECLARE SUB Pendolo ()
DECLARE SUB Accellerazione ()
DECLARE SUB Formule ()
DECLARE SUB Astronomia ()
DECLARE SUB Fisica ()
DECLARE SUB Conversioni ()

SCREEN 0
COLOR 15, 1
CLS

TYPE Variabili
    m AS LONG
END TYPE

CostantiNumeriche:
 
    CONST VelocitaLuce = 299792458 'km/s
    CONST VelocitaSuono = 1220 'km/h
    CONST hb = 6.625E-34 'Js   
    CONST G = 6.67259E-11 'N mý/kgý
    CONST CHubble = 75 'km/s Mpc
    CONST Pi = 3.141592654#
    CONST MProt = 1.6726231D-27 'g
    CONST MNeut = 1.6726231D-27 'g
    CONST MElet = 9.109389699999999D-31 'g
    CONST AL = 9.463E+12 'km
    CONST Parsec = 3.09E+13 'km
    CONST UA = 149600000 'km
    CONST CStefan = 5.67E-08'J/mýK^4
    CONST MagnCamp = 2 'ma
    CONST Grav = 9.81 'm/sý
    CONST Rad = .000004848132257# '"
    CONST Sec = 206256'rad
    CONST N = 9.81 'kg
    CONST NAvogadro = 6.02E+23
    CONST K = 2.634232945D-09 '4ãýG
    CONST MassaTerra = 5.97E+24
    CONST RaggioTerr = 6367000'm
    CONST MassaSol = 2E+30
    CONST LumSolare = 3.86E+36'J/s
    CONST RaggioSol = 700000000 'm
    CONST TempSole = 5780'K
    CONST AnnoSid = 365.2546 'giorni solari medi
    CONST AnnoTro = 365.2422 'giorni solari medi
    CONST GiornoSid = 23.9344 'ore
    CONST GiornSMed = 24 'ore

Inizio:

    LOCATE 10, 10
    PRINT "1- Formule Astronomiche"
    LOCATE 12, 10
    PRINT "2- Formule Varie"
    LOCATE 14, 10
    PRINT "3- Conversioni"

    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    SELECT CASE Tasti$
        CASE CHR$(49)
            CALL Astronomia
        CASE CHR$(50)
            CALL Varie
        CASE CHR$(51)
            CALL Conversioni
        CASE CHR$(27)
            STOP
        CASE ELSE
            BEEP
            GOSUB Inizio
    END SELECT
    GOSUB Inizio

SUB Astronomia
CLS
LOCATE 2, 22
PRINT "Scegli la formula che vuoi calcolare"
LOCATE 4, 10
PRINT "1- Legge di gravitazione universale"
LOCATE 6, 10
PRINT "2- Terza legge di Keplero"
LOCATE 8, 10
PRINT "3- Legge di Stefan"
LOCATE 10, 10
PRINT "4- ! Legge di Wien ! "
LOCATE 12, 10
PRINT "5- Effetto doppler"
LOCATE 14, 10
PRINT "6- Distanza con parallasse"
LOCATE 16, 10
PRINT "7- Magnitudine apparente"
LOCATE 18, 10
PRINT "8- Magnitudine assoluta"
LOCATE 20, 10
PRINT "9- Relazione Energia-Massa"
LOCATE 22, 10
PRINT "A- Legge di Hubble"
LOCATE 23, 70
PRINT "Pagina 1"

ImmissioneA:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(0) + "P" THEN GOSUB Facciata2
    IF Tasti$ = CHR$(27) THEN STOP
    IF Tasti$ = CHR$(8) THEN RUN
    SELECT CASE Tasti$
        CASE CHR$(49): CALL Gravitazione
        CASE CHR$(50): CALL Keplero
        CASE CHR$(51): CALL Stefan
        CASE CHR$(52): CALL Wien
        CASE CHR$(53): CALL Doppler
        CASE CHR$(54): CALL Distanza
        CASE CHR$(55): CALL mapp
        CASE CHR$(56): CALL MASS
        CASE CHR$(57): CALL enemas
        CASE CHR$(65): CALL LHubble
        CASE ELSE
            BEEP
            GOSUB ImmissioneA
    END SELECT

Facciata2:
    CLS
    LOCATE 2, 22
    PRINT "Scegli la formula che vuoi calcolare"
    LOCATE 4, 10
    PRINT "B- Calcolo della gravit…"
    LOCATE 6, 10
    PRINT "C- Et… dell'universo"
    LOCATE 8, 10
    PRINT "D- Costanti"
    LOCATE 10, 10
    PRINT "E- Onde elettromagnetiche"
    LOCATE 23, 70
    PRINT "Pagina 2"
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(0) + "P" THEN GOSUB Facciata2
    IF Tasti$ = CHR$(27) THEN STOP
    IF Tasti$ = CHR$(8) THEN RUN
    SELECT CASE Tasti$
        CASE CHR$(66): CALL Gravita
        CASE CHR$(67): CALL Hubble
        CASE CHR$(68): CALL Costanti
        CASE CHR$(69): CALL OndeEm
        CASE CHR$(0) + "H": CALL Astronomia
        CASE CHR$(0) + "P": BEEP: GOSUB Facciata2
    END SELECT
    GOSUB Facciata2
END SUB

SUB Conversioni
END SUB

SUB Costanti
CLS
LOCATE 1, 30
PRINT "Costanti numeriche"
PRINT "Velocit… della luce="; VelocitaLuce; "km/s"
PRINT "Costante di gravitazione universale="; G; "Númý/kgý"
PRINT "Costante di Hubble="; CHubble; "km/súMPc"
PRINT "Pi greco="; Pi
PRINT "Anno luce="; CDBL(AL); "km"
PRINT "Parsec="; Parsec; "km"
PRINT "Unit… astronomica="; UA; "km"
PRINT "Costante di Stefan="; CStefan
PRINT "Gravit… terrestre="; Grav; "m/sý"
PRINT "1kg="; N; "Newton"
PRINT "Numero di Avogadro="; NAvogadro
PRINT "Massa della Terra="; CDBL(MassaTerra); "kg"
PRINT "Raggio della Terra="; RaggioTerr; "m   "
PRINT "Massa del Sole"; MassaSol; "kg"
PRINT "Luminosit… del Sole="; LumSolare; "j/s"
PRINT "Raggio del Sole="; RaggioSol; "m"
PRINT "Temperatura del Sole="; TempSol; "K"
PRINT "Giorno siderale="; GiornoSid; "ore"
PRINT "Giorno solare medio"; GiornSMed; "ore"
PRINT "Anno siderale="; AnnoSid; "giorni solari medi"
PRINT "Anno Tropico="; AnnoTro; "giorni solari medi"
PRINT "Velocit… del suono "; VelocitaSuono; "km/h"
Tasti$ = ""
WHILE Tasti$ = ""
    Tasti$ = INKEY$
WEND
CALL Astronomia
END SUB

SUB Distanza
CLS
LOCATE 2, 20
PRINT "Calcolare la distanza mediante la parallasse"
LOCATE 6, 36
PRINT "d=b/p"
LOCATE 10, 26
PRINT "________=________m:________rad"
Immissione5:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        d = 0
        b = 0
        p = 0
        LOCATE 10, 26
        PRINT "________ m=________ m:________ rad"
        GOSUB Immissione5
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Base=                 "
    LOCATE 14, 11
    INPUT b
    IF b = 1 THEN
        LOCATE 10, 35
        b = RaggioTerr
        PRINT b; "m:________ rad                  "
    END IF
    IF b = 2 THEN
        LOCATE 10, 35
        b = UA
        PRINT b; "m:________ rad                  "
    END IF
    LOCATE 14, 5
    PRINT "Parallasse=              "
    LOCATE 14, 17
    INPUT p
    LOCATE 15, 5
    INPUT "Um="; Un
    LOCATE 15, 5: PRINT "                      "
    IF p <> 0 THEN
        IF Un = 1 THEN
            LOCATE 10, 46
            PRINT p; "sec              "
        END IF
        IF Un = 2 THEN
            LOCATE 10, 46
            PRINT p; "rad              "
        END IF
    END IF
    LOCATE 14, 5
    PRINT "Distanza=                "
    LOCATE 14, 15
    INPUT d
    LOCATE 10, 25
    PRINT "        "
    LOCATE 10, 33 - LEN(LTRIM$(RTRIM$(STR$(d)))) - 2
    PRINT d; "m"
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola5
    d = 0
    b = 0
    p = 0
    GOSUB Immissione5
   
Calcola5:
    IF d = 0 THEN
        IF Un = 1 THEN
            d = Sec * b / p
        ELSE
            d = b / p
        END IF
        LOCATE 16, 30
        PRINT "Distanza="; CDBL(d); "m                     "
    END IF
    IF b = 0 THEN
        IF Un = 1 THEN
            b = (1 / 2) * d * p
        ELSE
            b = d * p
        END IF
        LOCATE 16, 30
        PRINT "Base="; CDBL(b); "m                 "
    END IF
    IF p = 0 THEN
        IF Un = 1 THEN
            p = Sec * b / d
        ELSE
            p = b / d
        END IF
        LOCATE 16, 30
        PRINT "Parallasse="; p; "rad                 "
    END IF
    RETURN
END SUB

SUB Doppler
CLS
LOCATE 2, 30
PRINT "Effetto doppler"
LOCATE 6, 35
PRINT "Dç   v"
LOCATE 7, 35
PRINT "ÄÄ = Ä"
LOCATE 8, 35
PRINT "ç0   c"
LOCATE 10, 24
PRINT " ________ Hz    ________ m/s"
LOCATE 11, 24
PRINT "ÄÄÄÄÄÄÄÄÄÄÄ = ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 12, 24
PRINT " ________ Hz  "; VelocitaLuce; "Km/s"
Immissione4:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        LOCATE 10, 24
        PRINT " ________ Hz    ________ m/s"
        LOCATE 12, 24
        PRINT " ________ Hz  "; VelocitaLuce; "Km/s"
        F1 = 0
        F2 = 0
        v = 0
        GOSUB Immissione4
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Frequenza a riposo=           "
    LOCATE 14, 24
    INPUT F1
    LOCATE 10, 25
    PRINT "        "
    LF1 = LEN(RTRIM$(LTRIM$(STR$(F1))))
    LOCATE 10, 32 - LF1
    PRINT F1
    LOCATE 14, 5
    PRINT "Frequenza misurata=           "
    LOCATE 14, 24
    INPUT F2
    LOCATE 12, 25
    PRINT "        "
    LF2 = LEN(RTRIM$(LTRIM$(STR$(F2))))
    LOCATE 12, 32 - LF2
    PRINT F2
    LOCATE 14, 5
    PRINT "Velocita radiale=             "
    LOCATE 14, 22
    INPUT v
    LOCATE 10, 38
    PRINT "        "
    LV = LEN(RTRIM$(LTRIM$(STR$(v))))
    LOCATE 10, 46 - LV
    PRINT v
    LOCATE 14
    PRINT STRING$(50, " ")

Calcola4:
    IF v = 0 THEN
        v = (F1 * VelocitaLuce) / F2
        LOCATE 16, 20
        PRINT "Velocit… radiale="; CDBL(v); "m/s              "
    END IF
    IF F1 = 0 THEN
        F1 = (F2 * v) / VelocitaLuce
        LOCATE 16, 20
        PRINT "Freuqenza reale= "; CDBL(F1); "Hz              "
    END IF
    IF F2 = 0 THEN
        F2 = (F1 * VelocitaLuce) / v
        LOCATE 16, 20
        PRINT "Frequenza misurata="; CDBL(F2); "Hz            "
    END IF
    GOSUB Immissione4
END SUB

SUB enemas
CLS
LOCATE 2, 22
PRINT "Relazione tra massa ed energia"
LOCATE 6, 36
PRINT "E=mcý"
LOCATE 10, 20
PRINT "________ j/s= ________ kg*"; VelocitaLuce; "ý"

Immissione9:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        m = 0
        e = 0
        LOCATE 10, 20
        PRINT "________ j/s= ________ kg*"; VelocitaLuce; "ý"
        GOSUB Immissione9
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Energia=              "
    LOCATE 14, 13
    INPUT e
    LOCATE 10, 20
    PRINT e; "j/s= ________ kg*"; VelocitaLuce; "ý             "
    LOCATE 14, 5
    PRINT "Massa=               "
    LOCATE 14, 12
    INPUT m
    LOCATE 10, 41 - LEN(LTRIM$(RTRIM$(STR$(e))))
    PRINT m; "kg*"; VelocitaLuce; "ý                         "
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola9
    m = 0
    e = 0
    GOSUB Immissione9

Calcola9:
    IF e = 0 THEN
        e = m * VelocitaLuce ^ 2
        LOCATE 16, 20
        PRINT "Energia="; CDBL(e); "J             "
    END IF
    IF m = 0 THEN
        m = e / VelocitaLuce
        LOCATE 16, 20
        PRINT "Massa="; CDBL(m); "kg               "
    END IF
    RETURN
END SUB

SUB Gravita
CLS
LOCATE 2, 20
PRINT "Calcolare l'attrazione di gravit…"
LOCATE 5, 34
PRINT "     m"
LOCATE 6, 34
PRINT "g =GúÄÄÄ"
LOCATE 7, 34
PRINT "     r"
LOCATE 10, 28
PRINT "     ________ kg"
LOCATE 11, 18
PRINT G; "ú ÄÄÄÄÄÄÄÄÄ"
LOCATE 12, 28
PRINT "     ________ m"

Immissione11:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        r = 0
        m = 0
        LOCATE 10, 28
        PRINT "     ________ kg"
        LOCATE 12, 28
        PRINT "     ________ m"
        GOSUB Immissione11
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Massa=              "
    LOCATE 14, 11
    INPUT m
    LOCATE 10, 33
    PRINT m; "kg           "
    LOCATE 14, 5
    PRINT "Raggio=            "
    LOCATE 14, 11
    INPUT r
    LOCATE 12, 33
    PRINT r; "m            "
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola11
    r = 0
    m = 0
    GOSUB Immissione11

Calcola11:
    gra = G * (m / r ^ 2)
    LOCATE 16, 20
    PRINT "Gravit…="; CDBL(gra); "m/sý                     "
    RETURN
END SUB

SUB Gravitazione
CLS
LOCATE 2, 15
PRINT "Legge di gravitazione universale di Newton"
LOCATE 5, 30
PRINT "    m1 m2"
LOCATE 6, 30
PRINT "F= GÄÄÄÄÄÄ"
LOCATE 7, 30
PRINT "      dý"
LOCATE 10, 15
PRINT "              ________kg ú ________kg"
LOCATE 11, 15
PRINT G; "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 12, 15
PRINT "                   __________ý m"
Immissione1:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        m1# = 0
        m2# = 0
        d# = 0
        GOSUB Immissione1
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Massa1=                         "
    LOCATE 14, 12
    INPUT m1#
    LNum = LEN(LTRIM$(RTRIM$(STR$(m1#))))
    LSpac = 8 - LNum
    LSpac$ = STRING$(LSpac, " ")
    m1$ = LTRIM$(RTRIM$(STR$(m1#)))
    LOCATE 10, 29
    PRINT LSpac$ + m1$
    LOCATE 14, 5
    PRINT "Massa2=                         "
    LOCATE 14, 12
    INPUT m2#
    LNum = LEN(LTRIM$(RTRIM$(STR$(m2#))))
    LOCATE 10, 42
    m2$ = LTRIM$(RTRIM$(STR$(m2#)))
    PRINT m2$ + "kg                       "
    LOCATE 14, 5
    PRINT "Distanza=                       "
    LOCATE 14, 14
    INPUT d#
    LNum = LEN(LTRIM$(RTRIM$(STR$(d#))))
    LOCATE 12, 34
    PRINT "             "
    d$ = LTRIM$(RTRIM$(STR$(d#)))
    LOCATE 12, 40 - (LNum + 2 / 2)
    PRINT d$ + "ý m"
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola1
    GOSUB Immissione1
Calcola1:
    IF m1# = 0 OR m2# = 0 OR d# = 0 THEN GOSUB Immissione1
    f = G * ((m1# * m2#) / d# ^ 2)
    LOCATE 15, 28
    PRINT "F="; f; "N"
    RETURN
END SUB

SUB Hubble
CLS
LOCATE 2, 16
PRINT "Calcolare l'et… dell'universo mediante la costante di Hubble"
LOCATE 6, 36
PRINT "T=1/H"
LOCATE 10, 30
PRINT "T= 1/________ km/súMPc"
Immissione12:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        h = 0
        LOCATE 10, 30
        PRINT "T= 1/________ km/súMPc"
        GOSUB Immissione12
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Costante di Hubble="
    LOCATE 14, 24
    INPUT h
    LOCATE 10, 35
    PRINT h; "km/súMPc                  "
    IF h < 50 OR h > 100 THEN
        COLOR 14, 1
        LOCATE 15, 5
        PRINT "La costante deve essere compresa tra 50km/súMPc e 100km/súMPc"
        SLEEP 5
        COLOR 15, 1
        LOCATE 15
        PRINT STRING$(70, " ")
        LOCATE 14
        PRINT STRING$(50, " ")
        GOSUB Immissione12
    END IF
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola12
    h = 0
    GOSUB Immissione12

Calcola12:
    T = 1 / h
    'T$ = LEFT$(RTRIM$(LTRIM$(STR$(CDBL(T)))), 15)
    LOCATE 16, 30
    PRINT "Et…÷"; INT(T * 1000000000) + 1; "anni          "
    RETURN
END SUB

SUB Keplero
'$STATIC
DIM p(8)
DIM ax(8)
CLS
LOCATE 2, 15
PRINT "Terza legge di Keplero generalizzata da Newton"
LOCATE 6, 30
PRINT "pý=k a^3"
LOCATE 10, 25
PRINT "________ýg= ________^3 UA"
a = -1
b = 1
Tastiera2:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN CALL Astronomia
    IF Tasti$ = CHR$(78) THEN
        p$ = ""
        a = 0
        aa = 0
        aa$ = ""
        Ident = 0
        LOCATE 10, 25
        PRINT "________ýg= ________^3 UA"
        GOSUB Tastiera2
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Periodo di rivoluzione=       "
    LOCATE 14, 27
    INPUT p$
    p = VAL(RTRIM$(LTRIM$(p$)))
    FOR x = 1 TO LEN(p$)
        aa = aa + 1
        aa$ = RIGHT$(LEFT$(p$, aa), 1)
        IF aa$ < CHR$(48) OR aa$ > CHR$(58) THEN
            Ident = aa
            Ident$ = aa$
            p$ = LEFT$(p$, aa - 1)
            EXIT FOR
        END IF
    NEXT
    LNum = LEN(RTRIM$(LTRIM$(STR$(p))))
    LSpc$ = STRING$(8 - LNum, " ")
    LOCATE 10, 25
    PRINT LSpc$ + p$
    LOCATE 14, 5
    PRINT "Semiasse maggiore dell'orbita="
    LOCATE 14, 34
    INPUT a
    LOCATE 10, 37
    a$ = RTRIM$(LTRIM$(STR$(a)))
    PRINT a$ + "^3 UA            "
    LOCATE 14, 5
    PRINT STRING$(50, " ")
    GOSUB Calcola2
        p$ = ""
        a = 0
        aa = 0
        aa$ = ""
        Ident$ = ""
    GOSUB Tastiera2

Calcola2:
    IF a = 0 THEN
        IF LCASE$(Ident$) = "g" THEN
            p1 = p / AnnoSid
        ELSE
            p1 = p
        END IF
        ac = ac + 1
        p2 = (p1 ^ 2)
        a = SQR(SQR(p2))
        LOCATE 16, 22
        PRINT "Semiasse maggiore= "; a; " UA"
    END IF
    IF p = 0 THEN
        a1 = a ^ 3
        p1 = SQR(a1)
        IF p1 < 1 THEN
            p = p1 * AnnoSid
            Unit$ = "giorni      "
        ELSE
            p = p1
            Unit$ = "Anni        "
        END IF
        LOCATE 16, 26
        PRINT "Rivoluzione= "; p; Unit$
    END IF
    RETURN
END SUB

SUB Leve
CLS
LOCATE 2, 16
PRINT "Calcolare le varie formule applicabili alle leve"
LOCATE 6, 36
PRINT "Fp:Bp=Br:Fr"
LOCATE 10, 20
PRINT "________ kg: ________ m= ________ m: ________ kg"
Immissione13:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        br = 0
        bp = 0
        fr = 0
        fp = 0
        LOCATE 10, 20
        PRINT "________ kg: ________ m= ________ m: ________ kg"
        GOSUB Immissione13
        END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Forza potenza=           "
    LOCATE 14, 19
    INPUT fp
    LOCATE 10, 20
    PRINT fp; "kg: ________ m= ________ m: ________ kg             "
    LOCATE 14, 5
    PRINT "Braccio potenza=           "
    LOCATE 14, 21
    INPUT bp
    LOCATE 10, 25 + LEN(RTRIM$(LTRIM$(STR$(fp))))
    PRINT bp; "m= ________ m: ________kg                 "
    LOCATE 14, 5
    PRINT "Braccio resistenza=             "
    LOCATE 14, 24
    INPUT br
    LOCATE 10, 29 + LEN(RTRIM$(LTRIM$(STR$(fp)))) + LEN(RTRIM$(LTRIM$(STR$(bp))))
    PRINT br; "m: ________ kg                  "
    LOCATE 14, 5
    PRINT "Forza resistenza=           "
    LOCATE 14, 22
    INPUT fr
    LOCATE 10
    PRINT STRING$(79, " ")
    'LOCATE 10, 33 + LEN(RTRIM$(LTRIM$(STR$(fp)))) + LEN(RTRIM$(LTRIM$(STR$(bp)))) + LEN(RTRIM$(LTRIM$(STR$(br))))
    'PRINT fr; "kg                  "
    l = 36 - ((LEN(RTRIM$(LTRIM$(STR$(bp)))) + LEN(RTRIM$(LTRIM$(STR$(fp)))) + LEN(RTRIM$(LTRIM$(STR$(fr)))) + LEN(RTRIM$(LTRIM$(STR$(br)))) + 9) / 2)
    LOCATE 10, l
    PRINT fp; "kg:"; bp; "m="; br; "m:"; fr; "kg"
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola13
    br = 0
    bp = 0
    fr = 0
    fp = 0
    GOSUB Immissione13

Calcola13:
    IF bp = 0 THEN
        bp = (fp * fr) / br
        LOCATE 16, 26
        PRINT "Braccio potenza="; bp; "m"
    END IF
    IF fp = 0 THEN
        fp = (bp * br) / fr
        LOCATE 16, 26
        PRINT "Forza potenza="; fp; "kg"
    END IF
    IF fr = 0 THEN
        fr = (bp * br) / fp
        LOCATE 16, 26
        PRINT "Forza resistenza="; fr; "kg"
    END IF
    IF bp = 0 THEN
        br = (fp * fr) / bp
        LOCATE 16, 26
        PRINT "Braccio resistenza="; br; "m"
    END IF
    RETURN

END SUB

SUB LHubble
CLS
LOCATE 2, 30
PRINT "Legge di Hubble"
LOCATE 4
LOCATE 6, 36
PRINT "V=Húd"
LOCATE 10, 20
PRINT "________ km/s="; CHubble; "km/sú________ MParsec"

Immissione10:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        v = 0
        d = 0
        LOCATE 10, 20
        PRINT "________ km/s="; CHubble; "km/sú________ MParsec         "
        GOSUB Immissione10
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Velocit…=           "
    LOCATE 14, 15
    INPUT v
    LOCATE 10, 20
    PRINT v; "km/s= "; CHubble; "km/sú________ MParsec               "
    LOCATE 14, 5
    PRINT "Distanza=           "
    LOCATE 14, 15
    INPUT d
    LOCATE 10, 37 + LEN(LTRIM$(RTRIM$(STR$(v))))
    PRINT d; "MParsec                "
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola10
        v = 0
        d = 0
    GOSUB Immissione10

Calcola10:
    IF v = 0 THEN
        v = CHubble * d
        LOCATE 16, 24
        PRINT CDBL(v); "km/s              "
    END IF
    IF d = 0 THEN
        d = v / CHubble
        LOCATE 16, 24
        PRINT CDBL(d); "MParsec        "
    END IF
    RETURN
END SUB

SUB mapp
CLS
LOCATE 2, 15
PRINT "Calcolare la magnitudine apparente di una stella"
LOCATE 5, 30
PRINT "             I2"
LOCATE 6, 30
PRINT "m2-m1= -5log ÄÄ"
LOCATE 7, 30
PRINT "             I1"
LOCATE 10, 26
PRINT "                ________"
LOCATE 11, 26
PRINT "m1-"; MagnCamp; "= -5úlog ÄÄÄÄÄÄÄÄÄÄ"
LOCATE 12, 26
PRINT "                ________"
Immissione6:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        i1 = 0
        i2 = 0
        LOCATE 10, 26
        PRINT "                ________"
        LOCATE 12, 26
        PRINT "                ________"
        GOSUB Immissione6
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Illuminamento campione=           "
    LOCATE 14, 28
    INPUT i1
    LOCATE 10, 41
    PRINT i1; "         "
    LOCATE 14, 5
    PRINT "Illuminamento misurato=           "
    LOCATE 14, 27
    INPUT i2
    LOCATE 12, 41
    PRINT i2; "         "
    LOCATE 14, 5
    PRINT STRING$(50, " ")
    GOSUB Calcola6
    i1 = 0
    i2 = 0
    GOSUB Immissione6

Calcola6:
    m1 = -5 * LOG(i2 / i1) + MagnCamp
    LOCATE 16, 26
    PRINT "Magnitunine apparente="; CDBL(m1)
    RETURN
END SUB

SUB MASS
CLS
LOCATE 2, 15
PRINT "Calcolare la magnitunine assoluta di una stella"
LOCATE 6, 30
PRINT "M= m+5-5log d"
LOCATE 10, 20
PRINT "M=________ +5 -5úlog ________ parsec"
Immissione7:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        m = 0
        d = 0
        LOCATE 10, 22
        PRINT "________ +5 -5úlog ________ parsec"
        GOSUB Immissione7
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Magnitudine apparente=           "
    LOCATE 14, 27
    INPUT m
    LOCATE 10, 22
    PRINT m; "+5 -5úlog ________ parsec      "
    LOCATE 14, 5
    PRINT "Distanza=                        "
    LOCATE 14, 14
    INPUT d
    LOCATE 10, 34
    PRINT d; "parsec             "
    LOCATE 14, 5
    PRINT STRING$(50, " ")
    GOSUB Calcola7

Calcola7:
    PRINT m, d, LOG(d)
    MA = m + 5 - 5 * LOG(d)
    LOCATE 16, 18
    PRINT "Magnitunine assoluta="; MA; "               "
    m = 0
    d = 0
    GOSUB Immissione7

END SUB

SUB OndeEm
CLS
LOCATE 2, 25
PRINT "Onde Elettromagnetiche"
LOCATE 6, 38
PRINT "ç=v/f"
LOCATE 10, 22
PRINT "________ m= ________ m/s / ________ hz"

Immissione15:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        v = 0
        f = 0
        c = 0
        LOCATE 10, 22
        PRINT "________ m= ________ m/s / ________ hz"
        GOSUB Immissione15
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 10
    PRINT "Velocit…=                            "
    LOCATE 14, 19
    INPUT v
    IF v = 1 THEN v = VelocitaLuce
    IF v = 2 THEN v = VelocitaSuono * 1000
    LOCATE 10, 34
    PRINT CDBL(v); "m/s / ________ hz         "
    LOCATE 14, 10
    PRINT "Frequenza=                                "
    LOCATE 14, 20
    INPUT f
    LOCATE 10, 51 + (8 - LEN(LTRIM$(RTRIM$(STR$(CDBL(v))))))
    PRINT f; "hz             "
    LOCATE 14, 10
    PRINT "Lunghezza d'onda=                   "
    LOCATE 14, 27
    INPUT l
    LOCATE 10, 22
    PRINT l; "m="; CDBL(v); "m/s /"; f; "hz                "
    LOCATE 14
    PRINT STRING$(60, " ")
    GOSUB Calcola15
    v = 0
    l = 0
    f = 0
    GOSUB Immissione15

Calcola15:
    IF v = 0 THEN
        v = f * l
        LOCATE 16, 20
        PRINT "Velocit…="; v; "m/s                         "
    END IF
    IF f = 0 THEN
       f = v / l
       LOCATE 16, 20
       PRINT "Frequenza="; f; "hz                                 "
    END IF
    IF l = 0 THEN
        l = v / f
        LOCATE 16, 20
        PRINT "Lunghezza d'onda="; l; "m                        "
    END IF
    RETURN
END SUB

SUB Pendolo
CLS
LOCATE 2, 20
PRINT "Calcolare il periodo di oscillazione di un pendolo"
LOCATE 5, 34
PRINT "        l m"
LOCATE 6, 34
PRINT "T= 2ãû ÄÄÄÄÄ"
LOCATE 7, 34
PRINT "       "; Grav
LOCATE 9, 30
PRINT "       ________ m"
LOCATE 10, 30
PRINT "T=2ãúûÄÄÄÄÄÄÄÄÄÄÄÄ"
LOCATE 11, 30
PRINT "         "; Grav
Immissione14:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Varie
    END IF
    IF Tasti$ = CHR$(78) THEN
        l = 0
        LOCATE 9, 30
        PRINT "       ________ m"
        GOSUB Immissione14
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Lunghezza="
    LOCATE 14, 15
    INPUT l
    LOCATE 9, 37
    PRINT l; "m      "
    LOCATE 14
    PRINT STRING$(70, " ")
    GOSUB Calcola14
    l = 0
    GOSUB Immissione14

Calcola14:
    T = 2 * Pi * SQR(l / Grav)
    LOCATE 16, 20
    WHILE T > 60 AND a < 2
        T = T / 60
        a = a + 1
    WEND
    IF a = 0 THEN Um$ = "secondi        "
    IF a = 1 THEN Um$ = "minuti         "
    IF a > 1 THEN Um$ = "ore            "

    PRINT "Tempo="; T; Um$
    RETURN

END SUB

SUB Quanti
CLS
LOCATE 2, 22
PRINT "Energia di un'onda elettromagnetica"
LOCATE 6, 36
PRINT "E=fh"
LOCATE 10, 20
PRINT "________ j  = ________ hz*"; hb

Immissione16:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        f = 0
        e = 0
        LOCATE 10, 20
        PRINT "________ j  = ________ hz*"; hb; "              "
        GOSUB Immissione16
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Energia=              "
    LOCATE 14, 13
    INPUT e
    LOCATE 10, 20
    PRINT e; "j/s= ________ hz*"; hb; "            "
    LOCATE 14, 5
    PRINT "Frequenza=               "
    LOCATE 14, 15
    INPUT f
    LOCATE 10, 41 - LEN(LTRIM$(RTRIM$(STR$(e))))
    PRINT f; "hz*"; hb; "                         "
    LOCATE 14
    PRINT STRING$(50, " ")
    GOSUB Calcola16
    f = 0
    e = 0
    GOSUB Immissione16

Calcola16:
    IF e = 0 THEN
        e = f * hb
        LOCATE 16, 24
        PRINT "Energia="; e; "J             "
    END IF
    IF f = 0 THEN
        f = e / hb
        LOCATE 16, 24
        PRINT "Frequenza="; f; "hz               "
    END IF
    RETURN
END SUB

SUB Stefan
CLS
LOCATE 2, 30
PRINT "Legge di Stefan"
LOCATE 6, 32
PRINT "L= 4ãRýåT^4"
LOCATE 10, 18
PRINT "L j/s= 4ã________ý km"; CStefan; "________ øK^4"
Immissione3:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(8) THEN
        CALL Astronomia
    END IF
    IF Tasti$ = CHR$(78) THEN
        T = 0
        r = 0
        GOSUB Immissione3
    END IF
    IF Tasti$ = CHR$(27) THEN STOP
    LOCATE 14, 5
    PRINT "Raggio=           "
    LOCATE 14, 12
    INPUT r
    LOCATE 10, 28
    r$ = LTRIM$(RTRIM$(STR$(r)))
    Lr = LEN(r$)
    PRINT "        "
    LOCATE 10, 27
    PRINT r; "ýkm"; CStefan; " ________ øK^4               "
    LOCATE 14, 5
    PRINT "Temperatura=      "
    LOCATE 14, 17
    INPUT T
    LOCATE 10, 41 + Lr
    PRINT T; "øK^4         "
    LOCATE 14, 5
    PRINT STRING$(50, " ")

Calcola3:
    l = 4 * Pi * (r * 1000) ^ 2 * CStefan * T ^ 4
    LOCATE 16, 20
    PRINT "Luminosit…="; l; "J/s"
    T = 0
    r = 0
    GOSUB Immissione3
END SUB

SUB Varie
CLS
LOCATE 2, 22
PRINT "Scegli la formula che vuoi calcolare"
LOCATE 4, 10
PRINT "1- Pendolo"
LOCATE 6, 10
PRINT "2- Leve"
LOCATE 8, 10
PRINT "3- Onde elettromagnetiche"
LOCATE 10, 10
PRINT "4- Energia delle onde e.m."

ImmissioneF:
    Tasti$ = ""
    WHILE Tasti$ = ""
        Tasti$ = INKEY$
    WEND
    IF Tasti$ = CHR$(27) THEN STOP
    IF Tasti$ = CHR$(8) THEN RUN
    SELECT CASE Tasti$
        CASE CHR$(49): CALL Pendolo
        CASE CHR$(50): CALL Leve
        CASE CHR$(51): CALL OndeEm
        CASE CHR$(52): CALL Quanti
        CASE ELSE
            BEEP
            GOSUB ImmissioneF
    END SELECT
END SUB

SUB Wien
CLS
LOCATE 2, 16
PRINT "Legge di Wien"
LOCATE 6, 30
PRINT "çúT= 2,9ú10^23 múK"
LOCATE 10, 20
PRINT
END
END SUB

