CLS
PRINT "Calcolo dei fronti di salita di un segnale digitale"
PRINT ""
'Chiedo all'utente quanti sono i valori che deve immettere
INPUT "Numero di inputs: "; lCount
CLS

'Ripeto il ciclo finchŠ ci sono dei valori da leggere
WHILE lCount > 0

    'Do al carattere precedente il valore dell'ultimo carattere letto
    lChar$ = cChar$
    'Leggo il valore corrente del bit del segnale
    INPUT "Valore corrente del bit: "; cChar$

    'Se il valore del bit corrente Š valido...
    IF cChar$ = "1" OR cChar$ = "0" THEN

        'Se il carattere corrente e il carattere precedente sono diversi
        'significa che c'Š un fronte (di salita o di discesa)
        IF cChar$ <> lChar$ AND lChar$ <> "" THEN

            'Se il carattere corrente Š 1 significa che il carattere
            'precedente Š zero, quindi ci troviamo in un fronte di salita...
            IF cChar$ = "1" THEN

                '...allora incremento il contatore dei fronti di salita
                nFs = nFs + 1

            ELSE

                'Se invece il carattere corrente non Š 1 (quindi Š zero) il
                'carattere precedente Š 1. Allora questo Š un fronte di
                'discesa e faccio avanzare il contatore relativo
                nFd = nFd + 1


            END IF

        END IF

        'Decremento il contatore dei valori ancora da leggere
        lCount = lCount - 1

    END IF


WEND

'Scrivo i valori dei vari fronti che ho trovato sul mio precorso
PRINT ""
PRINT "Numero fronti di salita: " + LTRIM$(STR$(nFs))
PRINT "Numero fronti di discesa: " + LTRIM$(STR$(nFd))

