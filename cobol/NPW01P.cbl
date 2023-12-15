       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW01P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW01R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW01R
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
      *  NPW01R    ROUTINE ACCESSO NPDATT                             *
      *---------------------------------------------------------------*
RV0600* 210600 FORZATA LA LETTURA DELLA FILIALE 226 IN CASO DI -811   *
      *        IN LETTURA DEL CAB 03220                               *
PM0811* 081101 INSERITA WITH IN TUTTE LE LETTURE PER EVITARE CONTESE  *
      *---------------------------------------------------------------*
XBDN01* 19/12/2001 BDN SPERSONALIZZAZIONI BANCO DI NAPOLI             *
      *---------------------------------------------------------------*
MGVCPC* 01/08/2014 FUNZI=R3 RECUPERO CFILI DAL PRFIL                  *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WKS-ABIUT           PIC X(005).
       01  WKS-SERVI           PIC X(004).
       01  WKS-CABFI           PIC X(005).
       01  WKS-DTINI           PIC X(008).
       01  WKS-FTPRE           PIC X(002).
       01  DESCR-TIPO.
           05 FILLER   PIC X(21) VALUE 'ERRORE NELLA TABELLA '.
           05 TIPO-TAB PIC X(003).
PANZ   01  NPDATT-RED.
PANZ       05 FILLER    PIC X(003).
PANZ       05 DA1-ABIUT PIC X(005).
PANZ       05 DA1-RESTO PIC X(027).
XBDN01 01  WKS-DAT-KEYGE.
XBDN01     05 WKS-DAT-ABIUT  PIC X(005) VALUE SPACE.
XBDN01     05 WKS-DAT-MDUTI  PIC X(005) VALUE '00226'.
XBDN01     05 WKS-DAT-TRANS  PIC X(004) VALUE 'NP00'.
MGVCPC 01  WKS-PRFIL           PIC X(005).
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE NPDATT  END-EXEC.
           EXEC SQL DECLARE CUR-ADDT CURSOR FOR
                SELECT DAT_CODIT,                                       00580000
                       DAT_KEYGE,                                       00590000
                       DAT_RESTO                                        00600000
                FROM   NPDATT
                WHERE  DAT_CODIT = :DAT-CODIT
                ORDER BY  DAT_KEYGE
PM0811          WITH UR
           END-EXEC.
           COPY  NPA02TC.
           COPY  NPA51TC.
           COPY  NPG01RC.
           COPY  NPG03RC.
           COPY  NPW01RC.
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
           MOVE WRK-NP0500                      TO NPW01RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           PERFORM A0010-ELABORA        THRU A0010-END.
           MOVE NPW01RC                         TO WRK-NP0500.
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
      *****************************************************************
      **   ELABORAZIONE                                              **
      *****************************************************************
       A0010-ELABORA.
           MOVE SPACES                  TO   W01-RCODE.
           PERFORM A0020-CONTR          THRU A0020-END.
           IF W01-FUNZI EQUAL 'RE'
              PERFORM A0030-LEGGI       THRU A0030-END.
           IF W01-FUNZI EQUAL 'R1'
              PERFORM A0031-LEGGI       THRU A0031-END.
           IF W01-FUNZI EQUAL 'R2'
              PERFORM A0035-LEGGI       THRU A0035-END.
MGVCPC     IF W01-FUNZI EQUAL 'R3'
MGVCPC        PERFORM A0036-LEGGI       THRU A0036-END.
           IF W01-FUNZI EQUAL 'IS'
              PERFORM A0040-INSERISCI   THRU A0040-END.
           IF W01-FUNZI EQUAL 'DE'
              PERFORM A0050-DELETA       THRU A0050-END.
           IF W01-FUNZI EQUAL 'D1'
              PERFORM A0055-DELETA-CODIT THRU A0055-END.
           IF W01-FUNZI EQUAL 'UP'
              PERFORM A0060-AGGIORNA     THRU A0060-END.
CALUS1     IF W01-FUNZI EQUAL 'U1'
CALUS1        PERFORM A0062-AGGIORNA     THRU A0062-END.
CALUS1*
CALUS1     IF W01-FUNZI EQUAL 'U2'
CALUS1        PERFORM A0064-AGGIORNA     THRU A0064-END.
           IF W01-FUNZI EQUAL 'O1'
              PERFORM A0070-OPEN-CUR1    THRU A0070-END.
           IF W01-FUNZI EQUAL 'F1'
              PERFORM A0080-FETCH-CUR1   THRU A0080-END.
           IF W01-FUNZI EQUAL 'C1'
              PERFORM A0090-CLOSE-CUR1   THRU A0090-END.
       A0010-END.
           EXIT.
      *****************************************************************
      **   CONTROLLO INPUT                                           **
      *****************************************************************
       A0020-CONTR.
           IF W01-FUNZI EQUAL SPACES
              MOVE 'RE'       TO W01-FUNZI.
           IF W01-FUNZI EQUAL 'RE' OR
              W01-FUNZI EQUAL 'R1' OR
              W01-FUNZI EQUAL 'R2' OR
MGVCPC        W01-FUNZI EQUAL 'R3' OR
              W01-FUNZI EQUAL 'UP' OR
CALUS1        W01-FUNZI EQUAL 'U1' OR
CALUS1        W01-FUNZI EQUAL 'U2' OR
              W01-FUNZI EQUAL 'IS' OR
              W01-FUNZI EQUAL 'DE' OR
              W01-FUNZI EQUAL 'O1' OR
              W01-FUNZI EQUAL 'F1' OR
              W01-FUNZI EQUAL 'C1' OR
PANZ  *       W01-FUNZI EQUAL 'DE'
PANZ          W01-FUNZI EQUAL 'D1'
              NEXT SENTENCE
           ELSE
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'ERRORE RICHIAMO NPW01R'   TO G01-LABEL
              MOVE 'CODICE FUNZIONE ERRATA'   TO G01-DESC1.
       A0020-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
       A0030-LEGGI.
           MOVE W01-NPDATT     TO NPDATT.
           EXEC SQL
                SELECT DAT_RESTO
                INTO   :DAT-RESTO
                FROM   NPDATT
                WHERE  DAT_CODIT = :DAT-CODIT
                  AND  DAT_KEYGE = :DAT-KEYGE
PM0811          WITH UR
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0030-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC3
E00409        MOVE DAT-KEYGE                  TO G01-DESC4
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              PERFORM A0032-LEGGI-99999 THRU A0032-END.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                     TO W01-NPDATT.
       A0030-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA A02 PER VERIFICARE CORRETTEZZA CAB        **
      *****************************************************************
       A0031-LEGGI.
           MOVE W01-NPDATT     TO NPDATT.
           MOVE W01-NPDATT     TO NPA02TC.
           MOVE A02-ABIUT      TO WKS-ABIUT.
           MOVE A02-SERVI      TO WKS-SERVI.
           MOVE A02-CABFI      TO WKS-CABFI.
           EXEC SQL
                SELECT DAT_CODIT,                                       02360000
                       DAT_KEYGE,                                       02370000
                       DAT_RESTO                                        02380000
                INTO  :DAT-CODIT,                                       02390000
                      :DAT-KEYGE,                                       02400000
                      :DAT-RESTO                                        02410000
                FROM   NPDATT
                WHERE  DAT_CODIT              = :DAT-CODIT
                  AND  SUBSTR(DAT_KEYGE,1,5)  = :WKS-ABIUT
                  AND  SUBSTR(DAT_KEYGE,11,4) = :WKS-SERVI
                  AND  SUBSTR(DAT_RESTO,6,5)  = :WKS-CABFI
PM0811          WITH UR
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
210600     AND SQLCODE NOT EQUAL -811
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0031-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
210600     IF  SQLCODE  EQUAL -811
210600         MOVE ZEROES                     TO SQLCODE
210600         IF WKS-CABFI EQUAL '03220'
210600            PERFORM A0033-FORZA-FILIALE   THRU  A0033-END
210600            GO TO A0031-END.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0031-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                     TO W01-NPDATT.
       A0031-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
       A0032-LEGGI-99999.
           MOVE W01-NPDATT     TO NPDATT-RED.
           MOVE '99999'        TO DA1-ABIUT.
           MOVE NPDATT-RED     TO NPDATT.
           EXEC SQL
              SELECT DAT_RESTO
              INTO   :DAT-RESTO
              FROM NPDATT
                WHERE DAT_CODIT = :DAT-CODIT
                  AND DAT_KEYGE = :DAT-KEYGE
PM0811          WITH UR
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0032-LEGGI-99999'        TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC3
E00409        MOVE DAT-KEYGE                  TO G01-DESC4
              MOVE SQLCA                      TO G01-SQLCA
              GO TO A0032-END.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0032-LEGGI-99999'        TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC3
E00409        MOVE DAT-KEYGE                  TO G01-DESC4
              MOVE SQLCA                      TO G01-SQLCA
              GO TO A0032-END.
           MOVE NPDATT      TO W01-NPDATT.
       A0032-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
210600 A0033-FORZA-FILIALE.
           MOVE 'A02'             TO DAT-CODIT.
XBDN01*    MOVE '0102500226NP00'  TO DAT-KEYGE.
XBDN01     MOVE WKS-ABIUT         TO WKS-DAT-ABIUT.
XBDN01     MOVE WKS-DAT-KEYGE     TO DAT-KEYGE.
           EXEC SQL
                SELECT DAT_RESTO
                INTO   :DAT-RESTO
                FROM   NPDATT
                WHERE  DAT_CODIT = :DAT-CODIT
                  AND  DAT_KEYGE = :DAT-KEYGE
PM0811          WITH UR
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0033-FORZA-FILIALE'      TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC3
              MOVE DAT-KEYGE                  TO G01-DESC4
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0033-FORZA-FILIALE'      TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC3
              MOVE DAT-KEYGE                  TO G01-DESC4
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                     TO W01-NPDATT.
       A0033-END.
210600     EXIT.
      *****************************************************************
      **   LETTURA TABELLA A51 PER VERIFICA BOLLO STORICIZZATO       **
      *****************************************************************
       A0035-LEGGI.
           MOVE W01-NPDATT     TO NPDATT.
           MOVE W01-NPDATT     TO NPA51TC.
           MOVE A51-DTINI      TO WKS-DTINI.
           MOVE A51-ABIUT      TO WKS-ABIUT.
           MOVE A51-FTPRE      TO WKS-FTPRE.
           EXEC SQL
                SELECT DAT_CODIT,                                       04010000
                       DAT_KEYGE,                                       04020000
                       DAT_RESTO                                        04030000
                INTO  :DAT-CODIT,                                       04040000
                      :DAT-KEYGE,                                       04050000
                      :DAT-RESTO                                        04060000
                FROM   NPDATT
                WHERE  DAT_CODIT                = :DAT-CODIT
                  AND  SUBSTR(DAT_KEYGE,1,5)    = :WKS-ABIUT
                  AND  SUBSTR(DAT_KEYGE,6,2)    = :WKS-FTPRE
                  AND  SUBSTR(DAT_KEYGE,8,8)   <= :WKS-DTINI
                  AND  SUBSTR(DAT_KEYGE,16,8)  >= :WKS-DTINI
PM0811          WITH UR
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0035-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA NPA51TC ' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0035-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA NPA51TC ' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                     TO W01-NPDATT.
       A0035-END.
           EXIT.
MGVCPC*****************************************************************
MGVCPC**   LETTURA TABELLA A02 PER PRFIL E RECUPERO CFILI            **
MGVCPC*****************************************************************
MGVCPC A0036-LEGGI.
           MOVE W01-NPDATT     TO NPDATT.
           MOVE W01-NPDATT     TO NPA02TC.
           MOVE A02-ABIUT      TO WKS-ABIUT.
           MOVE A02-SERVI      TO WKS-SERVI.
           MOVE A02-PRFIL      TO WKS-PRFIL.
           EXEC SQL
                SELECT DAT_CODIT,                                       02360000
                       DAT_KEYGE,                                       02370000
                       DAT_RESTO                                        02380000
                INTO  :DAT-CODIT,                                       02390000
                      :DAT-KEYGE,                                       02400000
                      :DAT-RESTO                                        02410000
                FROM   NPDATT
                WHERE  DAT_CODIT              = :DAT-CODIT
                  AND  SUBSTR(DAT_KEYGE,1,5)  = :WKS-ABIUT
                  AND  SUBSTR(DAT_KEYGE,11,4) = :WKS-SERVI
                  AND  SUBSTR(DAT_RESTO,1,5)  = :WKS-PRFIL
                WITH UR
           END-EXEC.
           IF  SQLCODE NOT EQUAL ZERO  AND
               SQLCODE NOT EQUAL CENTO AND
               SQLCODE NOT EQUAL -811
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0031-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA
           END-IF.
           IF  SQLCODE  EQUAL -811
              MOVE 'DP'                       TO W01-RCODE
              MOVE 'PRFIL NON UNIVOCO'        TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
           END-IF.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0031-LEGGI'              TO G01-LABEL
              MOVE 'LETTURA TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA
           END-IF.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                     TO W01-NPDATT
           END-IF.
MGVCPC A0036-END.
MGVCPC     EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA                                       **
      *****************************************************************
       A0040-INSERISCI.
           MOVE W01-NPDATT     TO NPDATT.
           EXEC SQL
                INSERT
                INTO NPDATT
                     ( DAT_CODIT,  DAT_KEYGE,  DAT_RESTO)
                VALUES
                     (:DAT-CODIT, :DAT-KEYGE, :DAT-RESTO)
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL -803
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0040-INSERISCI'          TO G01-LABEL
              MOVE 'INSERT  TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC2
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL -803
              MOVE 'DP'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0040-INSERISCI'          TO G01-LABEL
              MOVE 'INSERT  TABELLA GENERICA' TO G01-DESC1
              MOVE 'CHIAVE DUPLICATA        ' TO G01-DESC2
              MOVE DAT-CODIT                  TO TIPO-TAB
PERINF*       MOVE DESCR-TIPO                 TO G01-DESC3
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
       A0040-END.
           EXIT.
      *****************************************************************
      **   CANCELLAZIONE TABELLA                                     **
      *****************************************************************
       A0050-DELETA.
           MOVE W01-NPDATT     TO NPDATT.
           EXEC SQL
                DELETE
                FROM   NPDATT
                WHERE  DAT_CODIT = :DAT-CODIT
                  AND  DAT_KEYGE = :DAT-KEYGE
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0050-DELETA'             TO G01-LABEL
              MOVE 'DELETE  TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC2
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0050-DELETA'             TO G01-LABEL
              MOVE 'DELETE  TABELLA GENERICA' TO G01-DESC1
              MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
              MOVE DAT-CODIT                  TO TIPO-TAB
PERINF*       MOVE DESCR-TIPO                 TO G01-DESC3
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
       A0050-END.
           EXIT.
      *****************************************************************
      **   CANCELLAZIONE DI UN CODICE TABELLA,ABI E SERVIZIO         **
      *****************************************************************
       A0055-DELETA-CODIT.
           MOVE W01-NPDATT     TO NPDATT.
           EXEC SQL DELETE FROM NPDATT
                WHERE   DAT_CODIT = :DAT-CODIT
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                     TO W01-RCODE
              MOVE 'S'                      TO G01-TPERR
              MOVE SQLCODE                  TO G01-PIC3
              MOVE 'NPDATT'                 TO G01-ARCHI
              MOVE 'A0055-DELETA-CODIT'     TO G01-LABEL
              MOVE SQLCA                    TO G01-SQLCA
              MOVE 'CANCELLAZIONE TABELLA'  TO G01-DESC1.
       A0055-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA                                     **
      *****************************************************************
       A0060-AGGIORNA.
           MOVE W01-NPDATT     TO NPDATT.
           EXEC SQL
                UPDATE  NPDATT
                SET     DAT_RESTO = :DAT-RESTO
                WHERE   DAT_CODIT = :DAT-CODIT
                  AND   DAT_KEYGE = :DAT-KEYGE
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0060-AGGIORNA'           TO G01-LABEL
              MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
              MOVE DAT-CODIT                  TO TIPO-TAB
              MOVE DESCR-TIPO                 TO G01-DESC2
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              MOVE 'GE'                       TO W01-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPDATT'                   TO G01-ARCHI
              MOVE 'NPW01R'                   TO G01-ROUNM
              MOVE 'A0060-AGGIORNA'           TO G01-LABEL
              MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
              MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
              MOVE DAT-CODIT                  TO TIPO-TAB
PERINF*       MOVE DESCR-TIPO                 TO G01-DESC3
PERINF        MOVE DAT-KEYGE                  TO G01-DESC3
              MOVE SQLCA                      TO G01-SQLCA.
       A0060-END.
           EXIT.
CALUS1*****************************************************************
CALUS1**   AGGIORNAMENTO TABELLA                                     **
CALUS1*****************************************************************
CALUS1 A0062-AGGIORNA.
CALUS1*
CALUS1     MOVE W01-NPDATT     TO NPDATT.
CALUS1*
CALUS1     EXEC SQL
CALUS1          UPDATE  NPDATT
CALUS1          SET     DAT_RESTO =
CALUS1          SUBSTR(DAT_RESTO,1,2)||'NO'||
CALUS1          SUBSTR(DAT_RESTO,5,196)
CALUS1          WHERE   DAT_CODIT = :DAT-CODIT
CALUS1            AND   DAT_KEYGE = :DAT-KEYGE
CALUS1     END-EXEC.
CALUS1*
CALUS1     IF SQLCODE NOT EQUAL ZERO AND
CALUS1        SQLCODE NOT EQUAL CENTO
CALUS1        MOVE 'KO'                       TO W01-RCODE
CALUS1        MOVE 'S'                        TO G01-TPERR
CALUS1        MOVE SQLCODE                    TO G01-PIC3
CALUS1        MOVE 'NPDATT'                   TO G01-ARCHI
CALUS1        MOVE 'NPW01R'                   TO G01-ROUNM
CALUS1        MOVE 'A0062-AGGIORNA'           TO G01-LABEL
CALUS1        MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
CALUS1        MOVE DAT-CODIT                  TO TIPO-TAB
CALUS1        MOVE DESCR-TIPO                 TO G01-DESC2
CALUS1        MOVE DAT-KEYGE                  TO G01-DESC3
CALUS1        MOVE SQLCA                      TO G01-SQLCA.
CALUS1*
CALUS1     IF SQLCODE EQUAL CENTO
CALUS1        MOVE 'GE'                       TO W01-RCODE
CALUS1        MOVE 'S'                        TO G01-TPERR
CALUS1        MOVE SQLCODE                    TO G01-PIC3
CALUS1        MOVE 'NPDATT'                   TO G01-ARCHI
CALUS1        MOVE 'NPW01R'                   TO G01-ROUNM
CALUS1        MOVE 'A0062-AGGIORNA'           TO G01-LABEL
CALUS1        MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
CALUS1        MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
CALUS1        MOVE DAT-CODIT                  TO TIPO-TAB
CALUS1        MOVE DAT-KEYGE                  TO G01-DESC3
CALUS1        MOVE SQLCA                      TO G01-SQLCA.
CALUS1*
CALUS1 A0062-END.
CALUS1     EXIT.
CALUS1*****************************************************************
CALUS1**   AGGIORNAMENTO TABELLA                                     **
CALUS1*****************************************************************
CALUS1 A0064-AGGIORNA.
CALUS1*
CALUS1     MOVE W01-NPDATT     TO NPDATT.
CALUS1*
CALUS1     EXEC SQL
CALUS1          UPDATE  NPDATT
CALUS1          SET     DAT_RESTO =
CALUS1          SUBSTR(DAT_RESTO,1,12)||'NONO'||
CALUS1          SUBSTR(DAT_RESTO,17,184)
CALUS1          WHERE   DAT_CODIT = :DAT-CODIT
CALUS1            AND   DAT_KEYGE = :DAT-KEYGE
CALUS1     END-EXEC.
CALUS1*
CALUS1     IF SQLCODE NOT EQUAL ZERO AND
CALUS1        SQLCODE NOT EQUAL CENTO
CALUS1        MOVE 'KO'                       TO W01-RCODE
CALUS1        MOVE 'S'                        TO G01-TPERR
CALUS1        MOVE SQLCODE                    TO G01-PIC3
CALUS1        MOVE 'NPDATT'                   TO G01-ARCHI
CALUS1        MOVE 'NPW01R'                   TO G01-ROUNM
CALUS1        MOVE 'A0064-AGGIORNA'           TO G01-LABEL
CALUS1        MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
CALUS1        MOVE DAT-CODIT                  TO TIPO-TAB
CALUS1        MOVE DESCR-TIPO                 TO G01-DESC2
CALUS1        MOVE DAT-KEYGE                  TO G01-DESC3
CALUS1        MOVE SQLCA                      TO G01-SQLCA.
CALUS1*
CALUS1     IF SQLCODE EQUAL CENTO
CALUS1        MOVE 'GE'                       TO W01-RCODE
CALUS1        MOVE 'S'                        TO G01-TPERR
CALUS1        MOVE SQLCODE                    TO G01-PIC3
CALUS1        MOVE 'NPDATT'                   TO G01-ARCHI
CALUS1        MOVE 'NPW01R'                   TO G01-ROUNM
CALUS1        MOVE 'A0064-AGGIORNA'           TO G01-LABEL
CALUS1        MOVE 'UPDATE  TABELLA GENERICA' TO G01-DESC1
CALUS1        MOVE 'CHIAVE NON TROVATA      ' TO G01-DESC2
CALUS1        MOVE DAT-CODIT                  TO TIPO-TAB
CALUS1        MOVE DAT-KEYGE                  TO G01-DESC3
CALUS1        MOVE SQLCA                      TO G01-SQLCA.
CALUS1*
CALUS1 A0064-END.
CALUS1     EXIT.
      *****************************************************************
      **   OPEN DEL CURSORE PER CODICE TABELLA                       **
      *****************************************************************
       A0070-OPEN-CUR1.
           MOVE W01-NPDATT                  TO NPDATT.
           EXEC SQL OPEN CUR-ADDT END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'KO'                     TO W01-RCODE
              MOVE WRK-PGMNM                TO G01-PGMNM
              MOVE 'S'                      TO G01-TPERR
              MOVE SQLCODE                  TO G01-PIC3
              MOVE SQLCA                    TO G01-SQLCA
              MOVE 'ENDATT'                 TO G01-ARCHI
              MOVE 'A0070-OPEN'             TO G01-LABEL
              MOVE SQLCA                    TO G01-SQLCA
              MOVE 'OPEN CURS SU NPDATT'    TO G01-DESC1
PERINF        MOVE DAT-KEYGE                TO G01-DESC3
           ELSE
              PERFORM A0080-FETCH-CUR1      THRU A0080-END.
       A0070-END.
           EXIT.
      *****************************************************************
      **   FETCH DEL CURSORE PER CODICE TABELLA                      **
      *****************************************************************
       A0080-FETCH-CUR1.
           EXEC SQL FETCH CUR-ADDT
                    INTO :DAT-CODIT,                                    06310000
                         :DAT-KEYGE,                                    06320000
                         :DAT-RESTO                                     06330000
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO   AND
              SQLCODE NOT EQUAL CENTO
              MOVE 'KO'                     TO W01-RCODE
              MOVE 'S'                      TO G01-TPERR
              MOVE WRK-PGMNM                TO G01-PGMNM
              MOVE SQLCODE                  TO G01-PIC3
              MOVE 'NPDATT'                 TO G01-ARCHI
              MOVE 'A0080-FETCH-CUR1'       TO G01-LABEL
              MOVE SQLCA                    TO G01-SQLCA
PERINF        MOVE DAT-KEYGE                TO G01-DESC3
              MOVE 'FETCH CUR-ADDT'         TO G01-DESC1.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                   TO   W01-NPDATT.
           IF SQLCODE EQUAL CENTO
              PERFORM A0090-CLOSE-CUR1      THRU A0090-END
              MOVE 'GE'                     TO   W01-RCODE.
       A0080-END.
           EXIT.
      *****************************************************************
      **   CLOSE DEL CURSORE PER CODICE TABELLA                      **
      *****************************************************************
       A0090-CLOSE-CUR1.
           EXEC SQL CLOSE CUR-ADDT END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'KO'                     TO W01-RCODE
              MOVE 'S'                      TO G01-TPERR
              MOVE SQLCODE                  TO G01-PIC3
              MOVE 'NPDATT'                 TO G01-ARCHI
              MOVE 'A0090-CLOSE-CUR1'       TO G01-LABEL
              MOVE SQLCA                    TO G01-SQLCA
PERINF        MOVE DAT-KEYGE                TO G01-DESC3
              MOVE 'CLOSE CURS SU NPDATT'   TO G01-DESC1.
       A0090-END.
           EXIT.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW01R
EXPAND*--------------------------------------------------------
