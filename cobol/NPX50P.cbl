       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPX50P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPX50R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPX50R
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *                                                               *
      *  *******  **   **  **  *****  **  **                          *
      *  **   **  **   **  **  **     ** **          PRODOTTO         *
      *  **   **  **   **  **  **     ***       ANTICIPO DOCUMENTI    *
      *  ** * **  **   **  **  **     ** **                           *
      *  *******  *******  **  *****  **  **                          *
      *       *                                  INFOSER S.R.L.       *
      *                                                               *
      *---------------------------------------------------------------*
      *  NPX50R  ROUTINE ACCESSO TABELLA NPERRT TABELLA ERRORI.       *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WKS-DESC1.
           05 WKS-CAMPO             PIC X(12).
           05 FILLER                PIC X(14) VALUE ' NON NUMERICO.'.
       01  WKS-CHIAVE-CUR1.
           05 WKS-ABIUT-CUR1        PIC X(05).
           05 WKS-CDOPE-CUR1        PIC X(08).
           05 WKS-CTERM-CUR1        PIC X(08).
           05 WKS-PGMNM-CUR1        PIC X(08).
           05 WKS-DTOPE-CUR1        PIC 9(09).
           05 WKS-CFILI-CUR1        PIC X(05).
           05 WKS-ORA-DA-CUR1       PIC 9(07).
           05 WKS-ORA-A-CUR1        PIC 9(07).
       01  WKS-CHIAVE-XX.
           05 WKS-ABIUT-XX          PIC X(05).
           05 WKS-CDOPE-XX          PIC X(08).
           05 WKS-CTERM-XX          PIC X(08).
           05 WKS-DTOPE-XX          PIC 9(09).
           05 WKS-ORAOP-XX          PIC 9(07).
       01  WKS-ORA-DA            PIC S9(007)  COMP-3.
       01  WKS-ORA-A             PIC S9(007)  COMP-3.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE NPERRT  END-EXEC.
           EXEC SQL DECLARE CUR-ERR1 CURSOR FOR
                    SELECT  ERR_ABIUT,
                            ERR_SERVI,
                            ERR_NOPER,
                            ERR_CDOPE,
                            ERR_CTERM,
                            ERR_CTRAN,
                            ERR_DTOPE,
                            ERR_ORAOP,
                            ERR_CFILI,
                            ERR_MODNM,
                            ERR_PGMDR,
                            ERR_PGMNM,
                            ERR_ROUNM,
                            ERR_LABEL,
                            ERR_ARCHI,
                            ERR_CDERR,
                            ERR_DESC1,
                            ERR_DESC2,
                            ERR_DESC3,
                            ERR_DESC4,
                            ERR_DESC5,
                            ERR_FILLE,
                            ERR_SQLCA
                    FROM    NPERRT
                    WHERE   ERR_ABIUT  = :ERR-ABIUT
                      AND   ERR_CDOPE  = :ERR-CDOPE
                      AND   ERR_CTERM  = :ERR-CTERM
                      AND   ERR_PGMNM LIKE :ERR-PGMNM
                      AND   ERR_DTOPE  = :ERR-DTOPE
                      AND   ERR_CFILI  = :ERR-CFILI
                      AND   ERR_ORAOP >= :WKS-ORA-DA
                      AND   ERR_ORAOP <= :WKS-ORA-A
                 ORDER BY   ERR_ORAOP DESC, ERR_PGMNM
           END-EXEC.
       COPY  NPG01RC.
       COPY  NPG03RC.
       COPY  NPX50RC.
EXPAND*    EXEC SQL INCLUDE NP1000EC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP1000EC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER ENTRATA IN ROUTINE AREA APPL. = 1000  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
       LINKAGE SECTION.
       01  WRK-COMMAREA        PIC X(1650).
      *---------------------------------------------------------------*
      *  PROCEDURE DIVISION                                           *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING WRK-COMMAREA.
       INIZIO-PGM.
           MOVE WRK-COMMAREA   TO WRK-NP1000R.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP1000EC
EXPAND*--------------------------------------------------------
           MOVE WRK-NP1000                      TO NPX50RC.
           MOVE WRK-NPG01-1000                  TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING          THRU A0000-END.
           PERFORM A0010-CONTR-INPUT            THRU A0010-END.
           IF X50-RCODE EQUAL SPACES
              PERFORM A0020-ELABORA             THRU A0020-END.
           MOVE NPX50RC                         TO WRK-NP1000.
           MOVE NPG01RC                         TO WRK-NPG01-1000.
EXPAND*    EXEC SQL INCLUDE NP1000FC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP1000FC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER RIENTRO DA ROUTINE AREA APPL. = 1000  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
           MOVE WRK-NP1000R TO WRK-COMMAREA.
           GOBACK.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP1000FC
EXPAND*--------------------------------------------------------
      **************************************************************
      ** OPERAZIONI INIZIALI                                      **
      **************************************************************
       A0000-HOUSE-KEEPING.
           MOVE 'NPX50R'                        TO WRK-PGMNM.
           MOVE SPACES                          TO X50-RCODE.
       A0000-END.
           EXIT.
      **************************************************************
      ** CONTROLLO DATI INPUT                                     **
      **************************************************************
       A0010-CONTR-INPUT.
           IF X50-FUNZI EQUAL SPACES
              MOVE 'RE'                         TO X50-FUNZI.
           IF    X50-FUNZI EQUAL 'RE'
              OR X50-FUNZI EQUAL 'IS'
              OR X50-FUNZI EQUAL 'O1'
              OR X50-FUNZI EQUAL 'F1'
              OR X50-FUNZI EQUAL 'C1'
              NEXT SENTENCE
           ELSE
              MOVE 'KO'                         TO X50-RCODE
              MOVE 'S'                          TO G01-TPERR
              MOVE  WRK-PGMNM                   TO G01-ROUNM
              MOVE 'ERRORE RICHIAMO NPX50R'     TO G01-LABEL
              MOVE 'CODICE FUNZIONE ERRATA'     TO G01-DESC1.
       A0010-END.
           EXIT.
           EJECT
      *****************************************************************
      **   CONTROLLI DI NUMERICITA'                                  **
      *****************************************************************
       A0015-CONTR-NUM.
           MOVE SPACES                         TO   WKS-CAMPO.
           MOVE X50-NPERRT                      TO NPERRT.
           IF X50-FUNZI EQUAL 'RE'
              IF  ERR-DTOPE        NOT NUMERIC
                  MOVE 'DTOPE'       TO WKS-CAMPO
              ELSE
              IF  ERR-ORAOP        NOT NUMERIC
                  MOVE 'ORAOP'       TO WKS-CAMPO.
           IF X50-FUNZI EQUAL 'IS'
              IF  ERR-NOPER        NOT NUMERIC
PERINF            MOVE 999999999999999 TO ERR-NOPER
PERINF*           MOVE 'NOPER'        TO WKS-CAMPO
              ELSE
              IF  ERR-DTOPE        NOT NUMERIC
                  MOVE 'DTOPE'       TO WKS-CAMPO
              ELSE
              IF  ERR-ORAOP        NOT NUMERIC
                  MOVE 'ORAOP'       TO WKS-CAMPO.
           IF X50-FUNZI EQUAL 'O1'
              IF ERR-DTOPE    NOT NUMERIC
                 MOVE 'DTOPE' TO WKS-CAMPO
              ELSE
              IF X50-ORAOP-DA    NOT NUMERIC
                 MOVE 'X50-ORAOP-DA' TO WKS-CAMPO
              ELSE
              IF X50-ORAOP-A     NOT NUMERIC
                 MOVE 'X50-ORAOP-A'   TO WKS-CAMPO.
           IF WKS-CAMPO NOT EQUAL SPACES
              MOVE 'KO'                         TO X50-RCODE
              MOVE 'S'                          TO G01-TPERR
              MOVE  WRK-PGMNM                   TO G01-ROUNM
              MOVE 'ERR. RICHIAMO NPX50R'       TO G01-LABEL
              MOVE WKS-DESC1                    TO G01-DESC1.
       A0015-END.
           EXIT.
           EJECT
      *****************************************************************
      **   ELABORAZIONE                                              **
      *****************************************************************
       A0020-ELABORA.
           IF X50-FUNZI EQUAL 'RE'
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X50-RCODE EQUAL SPACES
                 PERFORM A0100-LEGGI-ERR           THRU A0100-END.
           IF X50-FUNZI EQUAL 'IS'
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X50-RCODE EQUAL SPACES
                 PERFORM A0200-INSERT-ERR          THRU A0200-END.
           IF X50-FUNZI EQUAL 'O1'
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X50-RCODE EQUAL SPACES
                 PERFORM A0400-OPEN-CUR1           THRU A0400-END.
           IF X50-FUNZI EQUAL 'F1'
              PERFORM A0500-FETCH-CUR1          THRU A0500-END.
           IF X50-FUNZI EQUAL 'C1'
              PERFORM A0600-CLOSE-CUR1          THRU A0600-END.
       A0020-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
       A0100-LEGGI-ERR.
           MOVE X50-NPERRT                      TO NPERRT.
           PERFORM Z0010-READ-ERR               THRU Z0010-END.
       A0100-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA                                       **
      *****************************************************************
       A0200-INSERT-ERR.
           MOVE X50-NPERRT                   TO NPERRT.
           PERFORM Z0020-INSERT-ERR          THRU Z0020-END.
       A0200-END.
           EXIT.
      *****************************************************************
      **   OPEN E FETCH CURSORE                                      **
      *****************************************************************
       A0400-OPEN-CUR1.
           MOVE X50-ORAOP-DA                    TO WKS-ORA-DA.
           MOVE X50-ORAOP-A                     TO WKS-ORA-A.
           MOVE X50-NPERRT                      TO NPERRT.
           EXEC SQL OPEN CUR-ERR1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE ERR-ABIUT                    TO WKS-ABIUT-CUR1
              MOVE ERR-CDOPE                    TO WKS-CDOPE-CUR1
              MOVE ERR-CTERM                    TO WKS-CTERM-CUR1
              MOVE ERR-PGMNM                    TO WKS-PGMNM-CUR1
              MOVE ERR-DTOPE                    TO WKS-DTOPE-CUR1
              MOVE ERR-CFILI                    TO WKS-CFILI-CUR1
              MOVE X50-ORAOP-DA                 TO WKS-ORA-DA-CUR1
              MOVE X50-ORAOP-A                  TO WKS-ORA-A-CUR1
              MOVE WKS-CHIAVE-CUR1              TO G01-DESC5
              MOVE 'KO'                         TO X50-RCODE
              MOVE  WRK-PGMNM                   TO G01-PGMNM
              MOVE 'S'                          TO G01-TPERR
              MOVE SQLCODE                      TO G01-PIC3
              MOVE SQLCA                        TO G01-SQLCA
              MOVE 'NPERRT'                     TO G01-ARCHI
              MOVE 'A0400-OPEN-CUR1'            TO G01-LABEL
              MOVE SQLCA                        TO G01-SQLCA
              MOVE 'OPEN CURSORE SU NPERRT '    TO G01-DESC1
           ELSE
              PERFORM A0500-FETCH-CUR1          THRU A0500-END.
       A0400-END.
           EXIT.
      *****************************************************************
      **   FETCH CURSORE                                             **
      *****************************************************************
       A0500-FETCH-CUR1.
           EXEC SQL FETCH CUR-ERR1
                   INTO :ERR-ABIUT,
                        :ERR-SERVI,
                        :ERR-NOPER,
                        :ERR-CDOPE,
                        :ERR-CTERM,
                        :ERR-CTRAN,
                        :ERR-DTOPE,
                        :ERR-ORAOP,
                        :ERR-CFILI,
                        :ERR-MODNM,
                        :ERR-PGMDR,
                        :ERR-PGMNM,
                        :ERR-ROUNM,
                        :ERR-LABEL,
                        :ERR-ARCHI,
                        :ERR-CDERR,
                        :ERR-DESC1,
                        :ERR-DESC2,
                        :ERR-DESC3,
                        :ERR-DESC4,
                        :ERR-DESC5,
                        :ERR-FILLE,
                        :ERR-SQLCA
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                         TO X50-RCODE
              MOVE 'S'                          TO G01-TPERR
              MOVE WRK-PGMNM                    TO G01-PGMNM
              MOVE SQLCODE                      TO G01-PIC3
              MOVE 'NPERRT'                     TO G01-ARCHI
              MOVE 'A0500-FECTH-CUR1'           TO G01-LABEL
              MOVE SQLCA                        TO G01-SQLCA
              MOVE 'FETCH CUR-ERR1'             TO G01-DESC1.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                         TO X50-RCODE.
           IF SQLCODE EQUAL ZERO
              MOVE NPERRT                       TO X50-NPERRT.
       A0500-END.
           EXIT.
      *****************************************************************
      **   CLOSE CURSORE                                             **
      *****************************************************************
       A0600-CLOSE-CUR1.
           EXEC SQL CLOSE CUR-ERR1
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'KO'                         TO X50-RCODE
              MOVE  WRK-PGMNM                   TO G01-PGMNM
              MOVE 'S'                          TO G01-TPERR
              MOVE SQLCODE                      TO G01-PIC3
              MOVE 'NPERRT'                     TO G01-ARCHI
              MOVE 'A0600-CLOSE-CUR1'           TO G01-LABEL
              MOVE SQLCA                        TO G01-SQLCA
              MOVE 'CLOSE CURSORE SU NPERRT'    TO G01-DESC1.
       A0600-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA ERRORI                                    **
      *****************************************************************
       Z0010-READ-ERR.
           EXEC SQL
                SELECT  ERR_ABIUT,
                        ERR_SERVI,
                        ERR_NOPER,
                        ERR_CDOPE,
                        ERR_CTERM,
                        ERR_CTRAN,
                        ERR_DTOPE,
                        ERR_ORAOP,
                        ERR_CFILI,
                        ERR_MODNM,
                        ERR_PGMDR,
                        ERR_PGMNM,
                        ERR_ROUNM,
                        ERR_LABEL,
                        ERR_ARCHI,
                        ERR_CDERR,
                        ERR_DESC1,
                        ERR_DESC2,
                        ERR_DESC3,
                        ERR_DESC4,
                        ERR_DESC5,
                        ERR_FILLE,
                        ERR_SQLCA
                   INTO :ERR-ABIUT,
                        :ERR-SERVI,
                        :ERR-NOPER,
                        :ERR-CDOPE,
                        :ERR-CTERM,
                        :ERR-CTRAN,
                        :ERR-DTOPE,
                        :ERR-ORAOP,
                        :ERR-CFILI,
                        :ERR-MODNM,
                        :ERR-PGMDR,
                        :ERR-PGMNM,
                        :ERR-ROUNM,
                        :ERR-LABEL,
                        :ERR-ARCHI,
                        :ERR-CDERR,
                        :ERR-DESC1,
                        :ERR-DESC2,
                        :ERR-DESC3,
                        :ERR-DESC4,
                        :ERR-DESC5,
                        :ERR-FILLE,
                        :ERR-SQLCA
                   FROM NPERRT
                WHERE   ERR_ABIUT =  :ERR-ABIUT
                  AND   ERR_CDOPE =  :ERR-CDOPE
                  AND   ERR_CTERM =  :ERR-CTERM
                  AND   ERR_DTOPE =  :ERR-DTOPE
                  AND   ERR_ORAOP =  :ERR-ORAOP
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE ERR-ABIUT                    TO WKS-ABIUT-XX
              MOVE ERR-CDOPE                    TO WKS-CDOPE-XX
              MOVE ERR-CTERM                    TO WKS-CTERM-XX
              MOVE ERR-DTOPE                    TO WKS-DTOPE-XX
              MOVE ERR-ORAOP                    TO WKS-ORAOP-XX
              MOVE WKS-CHIAVE-XX                TO G01-DESC5
              MOVE 'KO'                         TO X50-RCODE
              MOVE 'S'                          TO G01-TPERR
              MOVE  SQLCODE                     TO G01-PIC3
              MOVE 'NPERRT'                     TO G01-ARCHI
              MOVE  WRK-PGMNM                   TO G01-ROUNM
              MOVE 'Z0010-READ-ERR'             TO G01-LABEL
              MOVE 'LETTURA TABELLA ERRORI'     TO G01-DESC1
              MOVE SQLCA                        TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              MOVE ERR-ABIUT                    TO WKS-ABIUT-XX
              MOVE ERR-CDOPE                    TO WKS-CDOPE-XX
              MOVE ERR-CTERM                    TO WKS-CTERM-XX
              MOVE ERR-DTOPE                    TO WKS-DTOPE-XX
              MOVE ERR-ORAOP                    TO WKS-ORAOP-XX
              MOVE WKS-CHIAVE-XX                TO G01-DESC5
              MOVE 'GE'                       TO X50-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPERRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0010-READ-ERR'           TO G01-LABEL
              MOVE 'LETTURA TABELLA ERRORI'   TO G01-DESC1
              MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPERRT                     TO X50-NPERRT.
       Z0010-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO RECORD                                        **
      *****************************************************************
       Z0020-INSERT-ERR.
           EXEC SQL INSERT
                    INTO     NPERRT
                    VALUES (:NPERRT)
           END-EXEC.
       Z0020-END.
           EXIT.
      *----------------------------------------------------------------
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPX50R
EXPAND*--------------------------------------------------------
