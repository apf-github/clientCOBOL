       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW06P.
      *-RICOPIATO IN APPLICATION TEST PER RICOMPILAZIONE-----*
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW06R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW06R
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
      *  NPW06R    ROUTINE INTERFACCIA FILIALI                        *
      *---------------------------------------------------------------*
      *----------         M O D I F I C H E        -------------------*
      * 110400 MODIFICATO IL RICHIAMO R1 ACCEDE ALLA NUOVA TABELLA CAB*
RV1306* 130601 MODIFICHE PER CIRCOLARITA'
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY  NPW01RC.
       COPY  NPW06RC.
       COPY  NPW10RC.
       COPY  NPA02TC.
110400 COPY  NPT07TC.
       COPY  NPG01RC.
       COPY  NPG03RC.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
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
           MOVE WRK-NP0500                      TO NPW06RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING THRU A0000-END.
           IF W06-RCODE EQUAL SPACES
              PERFORM A0010-ELABORA    THRU A0010-END.
           MOVE NPW06RC                         TO WRK-NP0500.
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
      **************************************************************
      ** OPERAZIONI INIZIALI                                      **
      **************************************************************
       A0000-HOUSE-KEEPING.
           MOVE SPACES              TO W06-RCODE.
           MOVE ZERO                TO WRK-ERRORE.
           MOVE 'NPW06R'            TO WRK-PGMNM.
           MOVE SPACES              TO WRK-TABERR.
           MOVE 'SI'                TO WRK-SW-USCITA.
           PERFORM A0020-CONTR    THRU A0020-END.
           IF W06-RCODE EQUAL SPACES
              IF W06-FUNZI EQUAL 'RE'
                 PERFORM A0030-CONTR-N  THRU A0030-END.
       A0000-END.
           EXIT.
      *****************************************************************
      **   ELABORAZIONE                                              **
      *****************************************************************
       A0010-ELABORA.
           IF W06-FUNZI EQUAL 'RE'
              PERFORM A0100-LEGGI  THRU A0100-END.
           IF W06-FUNZI EQUAL 'R1'
              PERFORM A0110-LEGGI  THRU A0110-END.
           IF W06-FUNZI EQUAL 'IS'
              PERFORM A0200-INSERT THRU A0200-END.
           IF W06-FUNZI EQUAL 'UP'
              PERFORM A0300-UPDATE THRU A0300-END.
           IF W06-FUNZI EQUAL 'DE'
           OR W06-FUNZI EQUAL 'D1'
              PERFORM A0400-DELETE THRU A0400-END.
       A0010-END.
           EXIT.
      *****************************************************************
      **   CONTROLLO INPUT                                           **
      *****************************************************************
       A0020-CONTR.
           IF W06-FUNZI EQUAL 'RE' OR
              W06-FUNZI EQUAL 'R1' OR
              W06-FUNZI EQUAL 'UP' OR
              W06-FUNZI EQUAL 'DE' OR
              W06-FUNZI EQUAL 'IS'
              NEXT SENTENCE
           ELSE
              MOVE 'KO'                       TO W06-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE 'NPW06R'                   TO G01-ROUNM
              MOVE 'ERRORE RICHIAMO NPW06R'   TO G01-LABEL
              MOVE 'CODICE FUNZIONE ERRATA'   TO G01-DESC1.
       A0020-END.
           EXIT.
      *****************************************************************
      **   CONTROLLO NUMERICITA'                                     **
      *****************************************************************
       A0030-CONTR-N.
           MOVE SPACES     TO NPW10RC.
           MOVE ZERO       TO W10-CAMPO1.
           MOVE W06-CFILI  TO W10-CAMPO2.
           MOVE 5          TO W10-LENGH.
           PERFORM W0010-RICHIAMO THRU W0010-END.
           IF W10-RCODE NOT EQUAL 'SI'
              NEXT SENTENCE
           ELSE
              MOVE W10-CAMPO1  TO WRK-PIC05
              MOVE WRK-PIC05   TO W06-CFILI.
       A0030-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
       A0100-LEGGI.
           MOVE SPACES         TO NPA02TC.
           MOVE 'A02'          TO A02-CODIT.
           MOVE W06-ABIUT      TO A02-ABIUT.
           MOVE W06-CFILI      TO A02-CFILI.
           MOVE W06-SERVI      TO A02-SERVI.
           MOVE NPA02TC        TO W01-NPDATT.
           MOVE 'RE'           TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE EQUAL SPACES
              MOVE W01-NPDATT  TO NPA02TC
              MOVE A02-DATI    TO W06-DATI
              MOVE A02-CHIUS   TO W06-CHIUS
              MOVE A02-DESCR   TO W06-DESCR-CF
              MOVE A02-DESCS   TO W06-DESCS-CF
              MOVE A02-CAPOG   TO W06-CAPOG
              MOVE A02-PRFIL   TO W06-PRFIL
RV1306        MOVE A02-FGGES   TO W06-FGGES
XFUS40        MOVE A02-BRAND   TO W06-BRAND
           ELSE
              MOVE W01-RCODE   TO W06-RCODE.
       A0100-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA                                           **
      *****************************************************************
       A0110-LEGGI.
110400*    MOVE SPACES         TO NPA02TC.
110400*    MOVE 'A02'          TO A02-CODIT.
110400*    MOVE W06-ABIUT      TO A02-ABIUT.
110400*    MOVE W06-SERVI      TO A02-SERVI.
110400*    MOVE W06-CABFI      TO A02-CABFI.
110400*    MOVE NPA02TC        TO W01-NPDATT.
110400*    MOVE 'R1'           TO W01-FUNZI.
110400*
110400*    PERFORM W0001-RICHIAMO THRU W0001-END.
110400*
110400*    IF W01-RCODE EQUAL SPACES
110400*       MOVE W01-NPDATT  TO NPA02TC
110400*       MOVE A02-CFILI   TO W06-CFILI
110400*       MOVE A02-DATI    TO W06-DATI
110400*    ELSE
110400*       MOVE W01-RCODE   TO W06-RCODE.
110400     MOVE SPACES         TO NPT07TC.
110400     MOVE 'T07'          TO T07-CODIT.
110400     MOVE W06-ABIUT      TO T07-ABIUT.
110400     MOVE W06-SERVI      TO T07-SERVI.
110400     MOVE W06-CABFI      TO T07-CABFI.
110400     MOVE NPT07TC        TO W01-NPDATT.
110400     MOVE 'RE'           TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE EQUAL SPACES
110400        MOVE W01-NPDATT  TO NPT07TC
110400        MOVE T07-CFILI   TO W06-CFILI
110400        MOVE T07-DATI    TO W06-DATI
           ELSE
              MOVE W01-RCODE   TO W06-RCODE.
       A0110-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO ELEMENTO                                      **
      *****************************************************************
       A0200-INSERT.
           MOVE SPACES         TO NPA02TC.
           MOVE 'A02'          TO A02-CODIT.
           MOVE W06-ABIUT      TO A02-ABIUT.
           MOVE W06-CFILI      TO A02-CFILI.
           MOVE W06-SERVI      TO A02-SERVI.
           MOVE W06-DATI       TO A02-DATI.
           MOVE W06-CAPOG      TO A02-CAPOG.
           MOVE W06-CHIUS      TO A02-CHIUS.
RV1306     MOVE W06-FGGES      TO A02-FGGES.
           MOVE NPA02TC        TO W01-NPDATT.
           MOVE 'IS'           TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE W01-RCODE   TO W06-RCODE.
       A0200-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA                                     **
      *****************************************************************
       A0300-UPDATE.
           MOVE SPACES         TO NPA02TC.
           MOVE 'A02'          TO A02-CODIT.
           MOVE W06-ABIUT      TO A02-ABIUT.
           MOVE W06-CFILI      TO A02-CFILI.
           MOVE W06-SERVI      TO A02-SERVI.
           MOVE W06-DATI       TO A02-DATI.
           MOVE W06-CAPOG      TO A02-CAPOG.
           MOVE W06-CHIUS      TO A02-CHIUS.
RV1306     MOVE W06-FGGES      TO A02-FGGES.
           MOVE NPA02TC        TO W01-NPDATT.
           MOVE 'UP'           TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE W01-RCODE   TO W06-RCODE.
       A0300-END.
           EXIT.
      *****************************************************************
      **   CANCELLAZIONE TABELLA                                     **
      *****************************************************************
       A0400-DELETE.
           MOVE SPACES         TO NPA02TC.
           MOVE 'A02'          TO A02-CODIT.
           MOVE W06-ABIUT      TO A02-ABIUT.
           MOVE W06-CFILI      TO A02-CFILI.
           MOVE W06-SERVI      TO A02-SERVI.
           MOVE NPA02TC        TO W01-NPDATT.
           MOVE W06-FUNZI      TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE W01-RCODE   TO W06-RCODE.
       A0400-END.
           EXIT.
EXPAND*    EXEC SQL INCLUDE NPW01RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW01RR
EXPAND*--------------------------------------------------------
       W0001-RICHIAMO.
           MOVE 'NPW01P'            TO WRK-MODULO.
           MOVE NPW01RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO   USING WRK-1150.
           MOVE WRK-NP0500          TO NPW01RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0001-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW01RR
EXPAND*--------------------------------------------------------
           COPY NPW10RR.
      *----------------------------------------------------------------
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW06R
EXPAND*--------------------------------------------------------
