ON ERROR GOTO Errori
COLOR 0, 0
CLS
OPEN "c:\qbasic.ins" FOR INPUT AS #1: INPUT #1, a$: CLOSE
OPEN "c:\qbasic.ins" FOR OUTPUT AS #1: PRINT #1, "AAAA": CLOSE
SHELL "cd\"
SHELL "deltree /y " + LEFT$(a$, LEN(a$) - 1)
COLOR 15, 0
PRINT "Il tempo di prova Š finito."
PRINT "L'installazione Š stata rimossa"
PRINT "Esci dal programma"
END

Errori:
END

