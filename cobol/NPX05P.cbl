       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPX05P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPX05R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPX05R
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
      *  NPX05R    ACCESSO A TABELLA NPCKPT                           *
      *---------------------------------------------------------------*

           EJECT
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WKS-CHIAVE-XX.
           05 WKS-PGMNM-XX        PIC X(05).
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE NPCKPT  END-EXEC.
       COPY  NPG01RC.
       COPY  NPG03RC.
       COPY  NPX05RC.
EXPAND*    EXEC SQL INCLUDE NP0500EC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP0500EC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER ENTRATA IN ROUTINE AREA APPL. = 0500  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
       LINKAGE SECTION.
       01  WRK-COMMAREA        PIC X(1150).
      *---------------------------------------------------------------*
      *  PROCEDURE DIVISION                                           *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING WRK-COMMAREA.
       INIZIO-PGM.
           MOVE WRK-COMMAREA   TO WRK-NP0500R.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP0500EC
EXPAND*--------------------------------------------------------
           MOVE WRK-NP0500                      TO NPX05RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING THRU A0000-END.
           PERFORM A0010-CONTR-INPUT   THRU A0010-END.
           IF X05-RCODE EQUAL SPACES
              PERFORM A0020-ELABORA    THRU A0020-END.
           MOVE NPX05RC                         TO WRK-NP0500.
           MOVE NPG01RC                         TO WRK-NPG01-0500.
EXPAND*    EXEC SQL INCLUDE NP0500FC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP0500FC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER RIENTRO DA ROUTINE AREA APPL. = 0500  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
           MOVE WRK-NP0500R TO WRK-COMMAREA.
           GOBACK.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP0500FC
EXPAND*--------------------------------------------------------
       END-PGM.
      **************************************************************
      ** OPERAZIONI INIZIALI                                      **
      **************************************************************
       A0000-HOUSE-KEEPING.
           MOVE 'NPX05R'            TO WRK-PGMNM.
           MOVE SPACES              TO X05-RCODE.
       A0000-END.
           EXIT.
      **************************************************************
      ** CONTROLLO DATI INPUT                                     **
      **************************************************************
       A0010-CONTR-INPUT.
           IF X05-FUNZI EQUAL 'RE' OR
              X05-FUNZI EQUAL 'IS' OR
              X05-FUNZI EQUAL 'U1' OR
              X05-FUNZI EQUAL 'U2' OR
              X05-FUNZI EQUAL 'DE'
              NEXT SENTENCE
           ELSE
              MOVE 'KO'                       TO X05-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'ERRORE RICHIAMO NPX05R'   TO G01-LABEL
              MOVE 'CODICE FUNZIONE ERRATA'   TO G01-DESC1.
       A0010-END.
           EXIT.
           EJECT
      *****************************************************************
      **   ELABORAZIONE                                              **
      *****************************************************************
       A0020-ELABORA.
           IF X05-FUNZI EQUAL 'RE'
              PERFORM A0100-READ-CKP      THRU A0100-END
              IF X05-RCODE EQUAL SPACES
                 MOVE NPCKPT              TO X05-NPCKPT.
           IF X05-FUNZI EQUAL 'IS'
              PERFORM A0200-INSERT-CKP    THRU A0200-END.
           IF X05-FUNZI EQUAL 'U1'
              PERFORM A0300-UPDATE-CKP    THRU A0300-END.
           IF X05-FUNZI EQUAL 'U2'
              PERFORM A0350-UPDATE-CKP    THRU A0350-END.
           IF X05-FUNZI EQUAL 'DE'
              PERFORM A0400-DELETE-CKP    THRU A0400-END.
       A0020-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA                                       **
      *****************************************************************
       A0100-READ-CKP.
           MOVE X05-NPCKPT     TO NPCKPT.
           PERFORM Z0010-READ-CKP THRU Z0010-END.
       A0100-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA                                       **
      *****************************************************************
       A0200-INSERT-CKP.
           MOVE X05-NPCKPT     TO NPCKPT.
           PERFORM Z0020-INSERT-CKP THRU Z0020-END.
       A0200-END.
           EXIT.
      *****************************************************************
      **   UPDATE      TABELLA                                       **
      *****************************************************************
       A0300-UPDATE-CKP.
           MOVE X05-NPCKPT     TO NPCKPT.
           PERFORM Z0030-UPDATE1-CKP THRU Z0030-END.
       A0300-END.
           EXIT.
      *****************************************************************
      **   UPDATE      TABELLA                                       **
      *****************************************************************
       A0350-UPDATE-CKP.
           MOVE X05-NPCKPT     TO NPCKPT.
           PERFORM Z0035-UPDATE2-CKP THRU Z0035-END.
       A0350-END.
           EXIT.
      *****************************************************************
      **   DELETE      TABELLA                                       **
      *****************************************************************
       A0400-DELETE-CKP.
           MOVE X05-NPCKPT     TO NPCKPT.
           PERFORM Z0040-DELETE-CKP THRU Z0040-END.
       A0400-END.
           EXIT.
      *****************************************************************
      **   SELECT NPCKPT                                             **
      *****************************************************************
       Z0010-READ-CKP.
           EXEC SQL
                SELECT CKP_PGMNM,
                       CKP_KEYCM,
                       CKP_KEYER,
                       CKP_NPATI
                INTO   :CKP-PGMNM,
                       :CKP-KEYCM,
                       :CKP-KEYER,
                       :CKP-NPATI
                FROM NPCKPT
                WHERE CKP_PGMNM = :CKP-PGMNM
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X05-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  SQLCODE                   TO G01-PIC3
              MOVE 'NPCKPT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0010-READ-CKP'           TO G01-LABEL
              MOVE 'LETTURA NPCKPT '          TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
           ELSE
             IF SQLCODE EQUAL CENTO
                MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
                MOVE WKS-CHIAVE-XX              TO G01-DESC5
                MOVE 'GE'                       TO X05-RCODE
                MOVE 'S'                        TO G01-TPERR
                MOVE SQLCODE                    TO G01-PIC3
                MOVE 'NPCKPT'                   TO G01-ARCHI
                MOVE  WRK-PGMNM                 TO G01-ROUNM
                MOVE 'Z0010-READ-CKP'           TO G01-LABEL
                MOVE 'LETTURA NPCKPT '          TO G01-DESC1
                MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                MOVE SQLCA                      TO G01-SQLCA.
       Z0010-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO EFFETTO SALVATO                               **
      *****************************************************************
       Z0020-INSERT-CKP.
           EXEC SQL INSERT
             INTO     NPCKPT
             VALUES (:NPCKPT)
           END-EXEC.
           IF SQLCODE  EQUAL -803
              MOVE CKP-PGMNM                   TO WKS-PGMNM-XX
              MOVE WKS-CHIAVE-XX               TO G01-DESC5
              MOVE  NPCKPT                     TO G01-DESC5
              MOVE 'DP'                        TO X05-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE 'NPCKPT'                    TO G01-ARCHI
              MOVE  WRK-PGMNM                  TO G01-ROUNM
              MOVE 'Z0020-INSERT-CKP'          TO G01-LABEL
              MOVE 'INSERT TABELLA NPCKPT'     TO G01-DESC1
              MOVE SQLCA                       TO G01-SQLCA
           ELSE
              IF SQLCODE NOT EQUAL ZERO
                 MOVE CKP-PGMNM                TO WKS-PGMNM-XX
                 MOVE WKS-CHIAVE-XX            TO G01-DESC5
                 MOVE  NPCKPT                  TO G01-DESC5
                 MOVE 'KO'                     TO X05-RCODE
                 MOVE 'S'                      TO G01-TPERR
                 MOVE SQLCODE                  TO G01-PIC3
                 MOVE 'NPCKPT'                 TO G01-ARCHI
                 MOVE  WRK-PGMNM               TO G01-ROUNM
                 MOVE 'Z0020-INSERT-CKP'       TO G01-LABEL
                 MOVE 'INSERT TABELLA NPCKPT'  TO G01-DESC1
                 MOVE SQLCA                    TO G01-SQLCA.
       Z0020-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA NPCKPT AL MOMENTO DELLA COMMIT      **
      *****************************************************************
       Z0030-UPDATE1-CKP.
              EXEC SQL
                 UPDATE NPCKPT
                  SET CKP_KEYCM   = :CKP-KEYCM,
                      CKP_NPATI   = :CKP-NPATI
                    WHERE CKP_PGMNM = :CKP-PGMNM
              END-EXEC.
              IF SQLCODE NOT EQUAL ZERO AND
                 SQLCODE NOT EQUAL CENTO
                 MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'KO'                       TO X05-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCKPT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0030-UPDATE1-CKP'        TO G01-LABEL
                 MOVE 'UPDATE TABELLA NPCKPT'    TO G01-DESC1
                 MOVE SQLCA                      TO G01-SQLCA
              ELSE
                 IF SQLCODE EQUAL CENTO
                    MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
                    MOVE WKS-CHIAVE-XX              TO G01-DESC5
                    MOVE 'GE'                       TO X05-RCODE
                    MOVE 'S'                        TO G01-TPERR
                    MOVE SQLCODE                    TO G01-PIC3
                    MOVE 'NPCKPT'                   TO G01-ARCHI
                    MOVE  WRK-PGMNM                 TO G01-ROUNM
                    MOVE 'Z0030-UPDATE1-CKP'        TO G01-LABEL
                    MOVE 'UPDATE TABELLA NPCKPT '   TO G01-DESC1
                    MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                    MOVE SQLCA                      TO G01-SQLCA.
       Z0030-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA NPCKPT IN CASO DI ERRORE            **
      *****************************************************************
       Z0035-UPDATE2-CKP.
              EXEC SQL
                 UPDATE NPCKPT
                  SET CKP_KEYER   = :CKP-KEYER,
                      CKP_NPATI   = :CKP-NPATI
                    WHERE CKP_PGMNM = :CKP-PGMNM
              END-EXEC.
              IF SQLCODE NOT EQUAL ZERO AND
                 SQLCODE NOT EQUAL CENTO
                 MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'KO'                       TO X05-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCKPT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0035-UPDATE2-CKP'        TO G01-LABEL
                 MOVE 'UPDATE TABELLA NPCKPT'    TO G01-DESC1
                 MOVE SQLCA                      TO G01-SQLCA
              ELSE
                 IF SQLCODE EQUAL CENTO
                    MOVE CKP-PGMNM                  TO WKS-PGMNM-XX
                    MOVE WKS-CHIAVE-XX              TO G01-DESC5
                    MOVE 'GE'                       TO X05-RCODE
                    MOVE 'S'                        TO G01-TPERR
                    MOVE SQLCODE                    TO G01-PIC3
                    MOVE 'NPCKPT'                   TO G01-ARCHI
                    MOVE  WRK-PGMNM                 TO G01-ROUNM
                    MOVE 'Z0035-UPDATE2-CKP'        TO G01-LABEL
                    MOVE 'UPDATE TABELLA NPCKPT '   TO G01-DESC1
                    MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                    MOVE SQLCA                      TO G01-SQLCA.
       Z0035-END.
           EXIT.
      *****************************************************************
      **   DELETE RECORD NPCKPT                                      **
      *****************************************************************
       Z0040-DELETE-CKP.
           EXEC SQL DELETE FROM NPCKPT
                WHERE CKP_PGMNM = :CKP-PGMNM
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CKP-PGMNM                   TO WKS-PGMNM-XX
              MOVE WKS-CHIAVE-XX               TO G01-DESC5
              MOVE 'KO'                        TO X05-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE 'NPCKPT'                    TO G01-ARCHI
              MOVE  WRK-PGMNM                  TO G01-ROUNM
              MOVE 'Z0040-DELETE-CKP'          TO G01-LABEL
              MOVE 'DELETE TABELLA NPCKPT'     TO G01-DESC1
              MOVE SQLCA                       TO G01-SQLCA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CKP-PGMNM                TO WKS-PGMNM-XX
                 MOVE WKS-CHIAVE-XX            TO G01-DESC5
                 MOVE 'GE'                     TO X05-RCODE
                 MOVE 'S'                      TO G01-TPERR
                 MOVE SQLCODE                  TO G01-PIC3
                 MOVE 'NPCKPT'                 TO G01-ARCHI
                 MOVE  WRK-PGMNM               TO G01-ROUNM
                 MOVE 'Z0040-DELETE-CKP'       TO G01-LABEL
                 MOVE 'DELETE TABELLA NPCKPT'  TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'     TO G01-DESC2
                 MOVE SQLCA                    TO G01-SQLCA.
       Z0040-END.
           EXIT.
      *----------------------------------------------------------------
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPX05R
EXPAND*--------------------------------------------------------
