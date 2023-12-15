       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPX09P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPX09R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPX09R
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
      *  NPX09R    ROUTINE DI ACCESSO TABELLA NPCTRT                  *
      *                                                               *
      *   NELLA FUNZIONE DI LETTURA SE NON TROVATA LA                 *
      *   TABELLA VIENE INSERITA INIZIALIZZATA                        *
      *   PER PROVVEDERE AUTOMATICAMENTE ALL'INSERIMENTO              *
      *   DI UNA NUOVA FILIALE.                                       *
      *---------------------------------------------------------------*
XUSU02* 30-08-05 IMPOSTATO IL CAMPO NRVAL A 10001                     *
SR0602*          MODIFICHE PER TRAMITAZIONE                           *
      *                                                               *
PEF001* 30/10/07 OTTIMIZZAZIONE PERFORMANCE                           *
SEQUEN* 061109 AGGIUNTA SEQUENCE PER GESTIONE NOPER.                  *
SEQD01* 031209 AGGIUNTA SEQUENCE PER GESTIONE NRD01.                  *
211209* 211209 CORREZIONE DI UN "IF" EFFETTUATO SUL CAMPO SBAGLIATO.  *
MO1006* 14/06/10 NUOVA FUNZIONE PER AGGIORNARE I CONTATORI DI PIU'    *
MO1006*          UNITA' (FUNZIONE INTRODOTTA PER I MAV ON LINE)       *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WKS-DESC1.
           05 WKS-CAMPO        PIC X(09).
           05 FILLER           PIC X(14) VALUE ' NON NUMERICO.'.
       01  WKS-CDOPE            PIC X(008).
       01  WKS-ABIUT-A16        PIC X(005).
CTR    01  WKS-ABIUT-A56        PIC X(005).
       01  WKS-CHIAVE-XX.
           05  WKS-ABIUT-XX     PIC X(05).
           05  WKS-PRFIL-XX     PIC X(05).
       01  WKS-CHIAVE-X1.
           05  WKS-ABIUT-X1     PIC X(05).
           05  WKS-PRFIL-X1     PIC X(05).
           05  WKS-CAMPONR-X1   PIC 9(15).
       01  WKS-CHIAVE-X2.
           05  WKS-ABIUT-X2     PIC X(05).
           05  WKS-PRFIL-X2     PIC X(05).
           05  WKS-UNOPE-X2     PIC 9(11).
       01  WKS-CHIAVE-X3.
           05  WKS-ABIUT-X3     PIC X(05).
           05  WKS-PRFIL-X3     PIC X(05).
           05  WKS-CDRAP-X3     PIC 9(11).
       01  WKS-CHIAVE-X4.
           05  WKS-ABIUT-X4     PIC X(05).
           05  WKS-PRFIL-X4     PIC X(05).
           05  WKS-CDDEB-X4     PIC 9(11).
       01  WKS-CHIAVE-X5.
           05  WKS-ABIUT-X5     PIC X(05).
           05  WKS-PRFIL-X5     PIC X(05).
           05  WKS-CDCNV-X5     PIC 9(15).
       01  WKS-CHIAVE-X6.
           05  WKS-ABIUT-X6     PIC X(05).
           05  WKS-PRFIL-X6     PIC X(05).
           05  WKS-CAMPONR-X6   PIC 9(11).
       01  WKS-CHIAVE-X7.
           05  WKS-ABIUT-X7     PIC X(05).
           05  WKS-PRFIL-X7     PIC X(05).
           05  WKS-CAMPONR-X7   PIC 9(09).
PEF001 05  WKS-IND              PIC 9(01).
PEF001 05  WKS-IND-MAX          PIC 9(01) VALUE 6.
PEF001 05  WKS-TUTTO-OK         PIC X(02).
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA  END-EXEC.
           EXEC SQL INCLUDE NPCTRT END-EXEC.
           EXEC SQL INCLUDE NPCT1T END-EXEC.
           EXEC SQL INCLUDE NPCT2T END-EXEC.
           EXEC SQL
              DECLARE  CUR-CTR1   CURSOR
              FOR SELECT    CT2_UNOPE
                 FROM  NPCT2T
                WHERE  CT2_ABIUT = :CT2-ABIUT
                  AND  CT2_PRFIL = :CT2-PRFIL
                  FOR UPDATE OF CT2_UNOPE
           END-EXEC.
       COPY  NPG01RC.
       COPY  NPG03RC.
       COPY  NPG20RC.
       COPY  NPA16TC.
CTR    COPY  NPA56TC.
SR0602 COPY  NPT62TC.
       COPY  NPX09RC.
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
           MOVE WRK-NP0500                      TO NPX09RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING THRU A0000-END.
           PERFORM A0010-CONTR-INPUT   THRU A0010-END.
           IF CONTINUA
              PERFORM A0020-ELABORA    THRU A0020-END.
           MOVE NPX09RC                         TO WRK-NP0500.
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
           MOVE 'NPX09R'            TO WRK-PGMNM.
           MOVE SPACES              TO X09-RCODE.
           MOVE 'SI'                TO WRK-SW-USCITA.
       A0000-END.
           EXIT.
      **************************************************************
      ** CONTROLLO DATI INPUT                                     **
      **************************************************************
       A0010-CONTR-INPUT.
           IF X09-FUNZI EQUAL SPACES
              MOVE 'RE'  TO X09-FUNZI.
           IF X09-FUNZI EQUAL 'RE' OR
              X09-FUNZI EQUAL 'IS' OR
              X09-FUNZI EQUAL 'DE' OR
              X09-FUNZI EQUAL 'U3' OR
              X09-FUNZI EQUAL 'U4' OR
              X09-FUNZI EQUAL 'U5' OR
              X09-FUNZI EQUAL 'U6' OR
              X09-FUNZI EQUAL 'U7' OR
              X09-FUNZI EQUAL 'U8' OR
              X09-FUNZI EQUAL 'U9' OR
              X09-FUNZI EQUAL 'UA' OR
              X09-FUNZI EQUAL 'UB' OR
              X09-FUNZI EQUAL 'UC' OR
              X09-FUNZI EQUAL 'UD' OR
              X09-FUNZI EQUAL 'UE' OR
              X09-FUNZI EQUAL 'UF' OR
              X09-FUNZI EQUAL 'UG' OR
              X09-FUNZI EQUAL 'UI' OR
              X09-FUNZI EQUAL 'UL' OR
L00054        X09-FUNZI EQUAL 'UM' OR
PEF001        X09-FUNZI EQUAL 'UQ' OR
PEF001        X09-FUNZI EQUAL 'UR' OR
SEQUEN        X09-FUNZI EQUAL 'NV' OR
SEQD01        X09-FUNZI EQUAL 'N1' OR
MO1006        X09-FUNZI EQUAL 'US' OR
              X09-FUNZI EQUAL 'UP'
              NEXT SENTENCE
           ELSE
              MOVE SPACES                     TO WRK-SW-USCITA
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'A0010-CONTR-INPUT'        TO G01-LABEL
              MOVE 'CODICE FUNZIONE ERRATA'   TO G01-DESC1.
       A0010-END.
           EXIT.
      *****************************************************************
      **   CONTROLLI DI NUMERICITA'                                  **
      *****************************************************************
       A0015-CONTR-NUM.
           MOVE SPACES                         TO   WKS-CAMPO.
           IF X09-FUNZI EQUAL 'IS'      AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-UNOPE        NOT NUMERIC
                 MOVE 'CTR-UNOPE' TO WKS-CAMPO
              ELSE
              IF CTR-CDDEB        NOT NUMERIC
                 MOVE 'CTR-CDDEB' TO WKS-CAMPO
              ELSE
              IF CTR-NRPRE        NOT NUMERIC
                 MOVE 'CTR-NRPRE' TO WKS-CAMPO
              ELSE
              IF CTR-NRD01        NOT NUMERIC
                 MOVE 'CTR-NRD01' TO WKS-CAMPO
              ELSE
              IF CTR-NRD02        NOT NUMERIC
                 MOVE 'CTR-NRD02' TO WKS-CAMPO
              ELSE
              IF CTR-NRD04        NOT NUMERIC
                 MOVE 'CTR-NRD04' TO WKS-CAMPO
              ELSE
              IF CTR-NRD05        NOT NUMERIC
                 MOVE 'CTR-NRD05' TO WKS-CAMPO
              ELSE
              IF CTR-NRD07        NOT NUMERIC
                 MOVE 'CTR-NRD07' TO WKS-CAMPO
              ELSE
              IF CTR-NRD10        NOT NUMERIC
                 MOVE 'CTR-NRD10' TO WKS-CAMPO
              ELSE
              IF CTR-NRMAV        NOT NUMERIC
                 MOVE 'CTR-NRMAV' TO WKS-CAMPO
              ELSE
              IF CTR-NRDAS        NOT NUMERIC
                 MOVE 'CTR-NRDAS' TO WKS-CAMPO
              ELSE
              IF CTR-NRRI1        NOT NUMERIC
                 MOVE 'CTR-NRRI1' TO WKS-CAMPO
              ELSE
              IF CTR-NRRES        NOT NUMERIC
                 MOVE 'CTR-NRRES' TO WKS-CAMPO
              ELSE
              IF CTR-NRSBF        NOT NUMERIC
                 MOVE 'CTR-NRSBF' TO WKS-CAMPO
              ELSE
              IF CTR-DTSTA        NOT NUMERIC
                 MOVE 'CTR-DTSTA' TO WKS-CAMPO
              ELSE
              IF CTR-DTDST        NOT NUMERIC
                 MOVE 'CTR-DTDST' TO WKS-CAMPO
              ELSE
              IF CTR-ID020        NOT NUMERIC
                 MOVE 'CTR-ID020' TO WKS-CAMPO
L00054        ELSE
L00054        IF CTR-PRLDI        NOT NUMERIC
L00054           MOVE 'CTR-PRLDI' TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'IS'      AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-CDRAP      NOT NUMERIC
                 MOVE 'CT1-CDRAP'  TO WKS-CAMPO
              ELSE
              IF CT1-CDDEB      NOT NUMERIC
                 MOVE 'CT1-CDDEB'  TO WKS-CAMPO
              ELSE
              IF CT1-CDCNV      NOT NUMERIC
                 MOVE 'CT1-CDCNV'  TO WKS-CAMPO
              ELSE
              IF CT1-NRPRE      NOT NUMERIC
                 MOVE 'CT1-NRPRE'  TO WKS-CAMPO
              ELSE
              IF CT1-NRVAL      NOT NUMERIC
                 MOVE 'CT1-NRVAL'  TO WKS-CAMPO
              ELSE
              IF CT1-NRRIM      NOT NUMERIC
                 MOVE 'CT1-NRRIM'  TO WKS-CAMPO
              ELSE
              IF CT1-NRRES      NOT NUMERIC
                 MOVE 'CT1-NRRES'  TO WKS-CAMPO
              ELSE
              IF CT1-NMARD      NOT NUMERIC
                 MOVE 'CT1-NMARD'  TO WKS-CAMPO
              ELSE
              IF CT1-NMAMN      NOT NUMERIC
                 MOVE 'CT1-NMAMN'  TO WKS-CAMPO
              ELSE
              IF CT1-ID020      NOT NUMERIC
                 MOVE 'CT1-ID020'  TO WKS-CAMPO
L00054        ELSE
L00054        IF CT1-PRLDI      NOT NUMERIC
L00054           MOVE 'CT1-PRLDI'  TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'IS'      AND X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT           TO NPCT2T
              IF CT2-UNOPE      NOT NUMERIC
                 MOVE 'CT2-UNOPE'  TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UP'      AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-UNOPE        NOT NUMERIC
                 MOVE 'CTR-UNOPE'    TO WKS-CAMPO
              ELSE
              IF CTR-CDDEB        NOT NUMERIC
                 MOVE 'CTR-CDDEB'    TO WKS-CAMPO
              ELSE
              IF CTR-NRPRE        NOT NUMERIC
                 MOVE 'CTR-NRPRE'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD01        NOT NUMERIC
                 MOVE 'CTR-NRD01'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD02        NOT NUMERIC
                 MOVE 'CTR-NRD02'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD04        NOT NUMERIC
                 MOVE 'CTR-NRD04'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD05        NOT NUMERIC
                 MOVE 'CTR-NRD05'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD07        NOT NUMERIC
                 MOVE 'CTR-NRD07'    TO WKS-CAMPO
              ELSE
              IF CTR-NRD10        NOT NUMERIC
                 MOVE 'CTR-NRD10'    TO WKS-CAMPO
              ELSE
              IF CTR-NRMAV        NOT NUMERIC
                 MOVE 'CTR-NRMAV'    TO WKS-CAMPO
              ELSE
              IF CTR-NRDAS        NOT NUMERIC
                 MOVE 'CTR-NRDAS'    TO WKS-CAMPO
              ELSE
              IF CTR-NRRI1        NOT NUMERIC
                 MOVE 'CTR-NRRI1'    TO WKS-CAMPO
              ELSE
              IF CTR-NRRES        NOT NUMERIC
                 MOVE 'CTR-NRRES'    TO WKS-CAMPO
              ELSE
              IF CTR-NRSBF        NOT NUMERIC
                 MOVE 'CTR-NRSBF'    TO WKS-CAMPO
              ELSE
              IF CTR-DTSTA        NOT NUMERIC
                 MOVE 'CTR-DTSTA'    TO WKS-CAMPO
              ELSE
              IF CTR-DTDST        NOT NUMERIC
                 MOVE 'CTR-DTDST'    TO WKS-CAMPO
              ELSE
              IF CTR-ID020        NOT NUMERIC
                 MOVE 'CTR-ID020'    TO WKS-CAMPO
L00054        ELSE
L00054        IF CTR-PRLDI      NOT NUMERIC
L00054           MOVE 'CTR-PRLDI'  TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UP'      AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-CDRAP             NOT NUMERIC
                 MOVE 'CT1-CDRAP'      TO WKS-CAMPO
              ELSE
              IF CT1-CDDEB             NOT NUMERIC
                 MOVE 'CT1-CDDEB'      TO WKS-CAMPO
              ELSE
              IF CT1-CDCNV             NOT NUMERIC
                 MOVE 'CT1-CDCNV'      TO WKS-CAMPO
              ELSE
              IF CT1-NRPRE             NOT NUMERIC
                 MOVE 'CT1-NRPRE'      TO WKS-CAMPO
              ELSE
              IF CT1-NRVAL             NOT NUMERIC
                 MOVE 'CT1-NRVAL'      TO WKS-CAMPO
              ELSE
              IF CT1-NRRIM             NOT NUMERIC
                 MOVE 'CT1-NRRIM'      TO WKS-CAMPO
              ELSE
              IF CT1-NRRES             NOT NUMERIC
                 MOVE 'CT1-NRRES'      TO WKS-CAMPO
              ELSE
              IF CT1-NMARD             NOT NUMERIC
                 MOVE 'CT1-NMARD'      TO WKS-CAMPO
              ELSE
              IF CT1-NMAMN             NOT NUMERIC
                 MOVE 'CT1-NMAMN'      TO WKS-CAMPO
              ELSE
              IF CT1-ID020             NOT NUMERIC
                 MOVE 'CT1-ID020'      TO WKS-CAMPO
L00054        ELSE
L00054        IF CT1-PRLDI      NOT NUMERIC
L00054           MOVE 'CT1-PRLDI'  TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UP'      AND X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT           TO NPCT2T
              IF CT2-UNOPE             NOT NUMERIC
                 MOVE 'CT2-UNOPE'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U3'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-NRRIM             NOT NUMERIC
                 MOVE 'CT1-NRRIM'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U4'
MO1006     OR X09-FUNZI EQUAL 'US'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-NRD01   NOT NUMERIC
                 MOVE 'CTR-NRD01'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U5'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-NRVAL NOT NUMERIC
                 MOVE 'CT1-NRVAL'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U6'      AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-UNOPE        NOT NUMERIC
                 MOVE 'CTR-UNOPE'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U6'      AND X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT           TO NPCT2T
              IF CT2-UNOPE           NOT NUMERIC
                 MOVE 'CT2-UNOPE'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U7'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-CDRAP  NOT NUMERIC
                 MOVE 'CT1-CDRAP'         TO WKS-CAMPO.
           IF  X09-FUNZI EQUAL 'U8'
               MOVE X09-NPCTRT          TO NPCTRT
               IF CTR-NRRI1    NOT NUMERIC
                  MOVE 'CTR-NRRI1'        TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U9'    AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-CDDEB     NOT NUMERIC
                 MOVE 'CTR-CDDEB'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'U9'    AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-CDDEB     NOT NUMERIC
                 MOVE 'CT1-CDDEB'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UA'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-CDCNV    NOT NUMERIC
                 MOVE 'CT1-CDCNV'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UB'     AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-NRRES    NOT NUMERIC
                 MOVE 'CTR-NRRES'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UB'     AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-NRRES    NOT NUMERIC
                 MOVE 'CT1-NRRES'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UC'     AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-NRPRE      NOT NUMERIC
                 MOVE 'CTR-NRPRE'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UC'     AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-NRPRE      NOT NUMERIC
                 MOVE 'CT1-NRPRE'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UD'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-NRSBF     NOT NUMERIC
                 MOVE 'CTR-NRSBF'       TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UF'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-DTSTA    NOT NUMERIC
                 MOVE 'CTR-DTSTA'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UG'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-DTDST    NOT NUMERIC
                 MOVE 'CTR-DTDTS'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UI'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-NMAMN    NOT NUMERIC
                 MOVE 'CT1-NMAMN'     TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UL' AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-ID020      NOT NUMERIC
                 MOVE 'CTR-ID020'      TO WKS-CAMPO.
           IF X09-FUNZI EQUAL 'UL' AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-ID020      NOT NUMERIC
                 MOVE 'CT1-ID020'      TO WKS-CAMPO.
L00054     IF X09-FUNZI EQUAL 'UM' AND X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT           TO NPCTRT
              IF CTR-PRLDI      NOT NUMERIC
                 MOVE 'CTR-PRLDI'      TO WKS-CAMPO.
PAOLO *
L00054     IF X09-FUNZI EQUAL 'UM' AND X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT           TO NPCT1T
              IF CT1-PRLDI      NOT NUMERIC
                 MOVE 'CT1-PRLDI'      TO WKS-CAMPO.
           IF WKS-CAMPO NOT EQUAL SPACES
              MOVE 'KO'                         TO X09-RCODE
              MOVE 'S'                          TO G01-TPERR
              MOVE  WRK-PGMNM                   TO G01-ROUNM
              MOVE 'ERR. RICHIAMO NPX09R'       TO G01-LABEL
              MOVE WKS-DESC1                    TO G01-DESC1.
       A0015-END.
           EXIT.
           EJECT
      *****************************************************************
      **   ELABORAZIONE                                              **
      *****************************************************************
       A0020-ELABORA.
           IF X09-FUNZI EQUAL 'RE'
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0100-LEGGI-CTR        THRU A0100-END.
           IF X09-FUNZI EQUAL 'IS'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0200-INSERT-CTR       THRU A0200-END.
           IF X09-FUNZI EQUAL 'DE'
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
              PERFORM A0300-DELETE-CTR          THRU A0300-END.
           IF X09-FUNZI EQUAL 'UP'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0400-UPDATE-CTR       THRU A0400-END.
           IF X09-FUNZI EQUAL 'U3'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0430-UPDATE-CT1       THRU A0430-END.
           IF X09-FUNZI EQUAL 'U4'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0440-UPDATE-CTR       THRU A0440-END.
MO1006     IF X09-FUNZI EQUAL 'US'
MO1006        PERFORM A0050-CONTR-OBBL          THRU A0050-END
MO1006        PERFORM A0015-CONTR-NUM           THRU A0015-END
MO1006        IF X09-RCODE EQUAL SPACES
MO1006           PERFORM A0630-UPDATE-CTR       THRU A0630-END
MO1006        END-IF
MO1006     END-IF
           IF X09-FUNZI EQUAL 'U5'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0450-UPDATE-CT1       THRU A0450-END.
           IF X09-FUNZI EQUAL 'U6'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0460-UPDATE-CTR       THRU A0460-END.
           IF X09-FUNZI EQUAL 'U7'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0470-UPDATE-CT1       THRU A0470-END.
           IF  X09-FUNZI EQUAL 'U8'
               PERFORM A0050-CONTR-OBBL         THRU A0050-END
               PERFORM A0015-CONTR-NUM          THRU A0015-END
               IF X09-RCODE EQUAL SPACES
                  PERFORM A0480-UPDATE-CTR      THRU A0480-END.
           IF X09-FUNZI EQUAL 'U9'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0490-UPDATE-CTR       THRU A0490-END.
           IF X09-FUNZI EQUAL 'UA'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0500-UPDATE-CT1       THRU A0500-END.
           IF X09-FUNZI EQUAL 'UB'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0510-UPDATE-CTR       THRU A0510-END.
           IF X09-FUNZI EQUAL 'UC'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0520-UPDATE-CTR       THRU A0520-END.
           IF X09-FUNZI EQUAL 'UD'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0530-UPDATE-CTR       THRU A0530-END.
           IF X09-FUNZI EQUAL 'UE'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0540-UPDATE-CTR       THRU A0540-END.
           IF X09-FUNZI EQUAL 'UF'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0550-UPDATE-DTSTA     THRU A0550-END.
           IF X09-FUNZI EQUAL 'UG'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0560-UPDATE-DTDST     THRU A0560-END.
           IF X09-FUNZI EQUAL 'UI'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0580-UPDATE-NMAMN     THRU A0580-END.
           IF X09-FUNZI EQUAL 'UL'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0590-UPDATE-ID020     THRU A0590-END.
PAOLO *
L00054     IF X09-FUNZI EQUAL 'UM'
              PERFORM A0050-CONTR-OBBL          THRU A0050-END
              PERFORM A0015-CONTR-NUM           THRU A0015-END
              IF X09-RCODE EQUAL SPACES
                 PERFORM A0600-UPDATE-PRLDI     THRU A0600-END.
SEQUEN     IF X09-FUNZI EQUAL 'NV'
SEQUEN        PERFORM A0610-NEXT-VALUE          THRU A0610-END.
SEQD01     IF X09-FUNZI EQUAL 'N1'
SEQD01        PERFORM A0620-NEXT-NRD01          THRU A0620-END.
       A0020-END.
           EXIT.
      *****************************************************************
      **   CONTROLLO OBBLIGATORIETA' CAMPO CDOPE                     **
      *****************************************************************
       A0050-CONTR-OBBL.
PEF001*    IF X09-FOPTB EQUAL 'B'
PEF001     IF X09-FOPTB EQUAL 'B' OR 'P'
              MOVE X09-NPCTRT                TO NPCTRT
              MOVE CTR-CDOPE                 TO WKS-CDOPE.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT                TO NPCT1T
              MOVE CT1-CDOPE                 TO WKS-CDOPE.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT                TO NPCT2T
              MOVE CT2-CDOPE                 TO WKS-CDOPE.
           IF WKS-CDOPE EQUAL SPACES
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'ERRORE RICHIAMO NPX09R'   TO G01-LABEL
              MOVE 'CDOPE NON VALORIZZATO'    TO G01-DESC1.
       A0050-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI                                 **
      *****************************************************************
       A0100-LEGGI-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT                TO NPCTRT
              PERFORM Z0010-READ-CTR         THRU Z0010-END.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT                TO NPCT1T
              PERFORM Z0011-READ-CT1         THRU Z0011-END.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT                TO NPCT2T
              PERFORM Z0012-READ-CT2         THRU Z0012-END.
PEF001     IF X09-FOPTB EQUAL 'P'
PEF001        MOVE X09-NPCTRT                TO NPCTRT
PEF001        PERFORM Z0013-GEST-CTR         THRU Z0013-END.
       A0100-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI PER UNOPE                       **
      *****************************************************************
       A0110-LEGGI-CTR.
           MOVE X09-NPCTRT                     TO NPCT2T.
           EXEC SQL
               OPEN  CUR-CTR1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'KO'                        TO X09-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE SQLCA                       TO G01-SQLCA
              MOVE 'NPCT2T'                    TO G01-ARCHI
              MOVE 'A0110-LEGGI-CTR'           TO G01-LABEL
              MOVE 'OPEN CURSORE SU NPCT2T '   TO G01-DESC1
              MOVE WRK-PGMNM                   TO G01-PGMNM
           ELSE
              PERFORM A0111-FETCH-CTR1         THRU A0111-END.
       A0110-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI                                 **
      *****************************************************************
       A0111-FETCH-CTR1.
            EXEC SQL FETCH CUR-CTR1
                     INTO :CTR-UNOPE
            END-EXEC.
            IF SQLCODE  NOT EQUAL ZERO
               MOVE 'KO'                        TO X09-RCODE
               MOVE 'S'                         TO G01-TPERR
               MOVE SQLCODE                     TO G01-PIC3
               MOVE 'NPCTRT'                    TO G01-ARCHI
               MOVE 'A0111-FETCH-CTR1'          TO G01-LABEL
               MOVE 'FETCH CUR-PRO1'            TO G01-DESC1
               MOVE WRK-PGMNM                   TO G01-PGMNM
               MOVE SQLCA                       TO G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPCT2T                        TO X09-NPCTRT
              PERFORM A0112-CLOSE-CTR1         THRU A0112-END.
       A0111-END.
           EXIT.
      *****************************************************************
      **   CLOSE DEL CURSORE                                         **
      *****************************************************************
       A0112-CLOSE-CTR1.
           EXEC SQL CLOSE CUR-CTR1
           END-EXEC
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'KO'                         TO X09-RCODE
              MOVE  WRK-PGMNM                   TO G01-PGMNM
              MOVE 'S'                          TO G01-TPERR
              MOVE SQLCODE                      TO G01-PIC3
              MOVE 'NPCT2T'                     TO G01-ARCHI
              MOVE 'A0112-CLOSE-CTR1'           TO G01-LABEL
              MOVE SQLCA                        TO G01-SQLCA
              MOVE 'CLOSE CURSORE SU NPCT2T'    TO G01-DESC1.
       A0112-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA                                       **
      *****************************************************************
       A0200-INSERT-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT             TO NPCTRT
              PERFORM Z0020-INSERT-CTR    THRU Z0020-END.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT             TO NPCT1T
              PERFORM Z0021-INSERT-CT1    THRU Z0021-END.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT             TO NPCT2T
              PERFORM Z0022-INSERT-CT2    THRU Z0022-END.
       A0200-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA CONTATORI BATCH CON I VALORI INIZIALI **
      *****************************************************************
       A0210-INSERT-CTR.
           MOVE CTR-ABIUT          TO   WKS-ABIUT-A16.
           PERFORM A0220-LEGGI-A16 THRU A0220-END.
CTR        MOVE CTR-ABIUT          TO   WKS-ABIUT-A56.
CTR   *
CTR        PERFORM A0221-LEGGI-A56 THRU A0221-END.
SR0602     PERFORM A0222-LEGGI-T62 THRU A0222-END.
           IF CONTINUA
              MOVE X09-NPCTRT            TO NPCTRT
              MOVE G20-UNOPE-MNB         TO CTR-UNOPE
              MOVE G20-CDDEB-MNB         TO CTR-CDDEB
              MOVE G20-NRPRE-MNB         TO CTR-NRPRE
              MOVE A16-MINMA             TO CTR-NRMAV
CTR           MOVE A56-MIN01             TO CTR-NRD01
CTR           MOVE A56-MIN02             TO CTR-NRD02
CTR           MOVE A56-MIN04             TO CTR-NRD04
CTR           MOVE A56-MIN05             TO CTR-NRD05
CTR           MOVE A56-MIN07             TO CTR-NRD07
CTR           MOVE A56-MIN10             TO CTR-NRD10
              MOVE 1                     TO CTR-NRDAS
E00415*       MOVE 1                     TO CTR-NRRI1
SR0603        IF CTR-PRFIL NOT = T62-FILTR
E00415           MOVE G20-NRRI1-MNB      TO CTR-NRRI1
SR0603        ELSE
SR0603           MOVE T62-MINRS          TO CTR-NRRI1
SR0603        END-IF
              MOVE G20-NRRES-MNB         TO CTR-NRRES
              MOVE 1                     TO CTR-NRSBF
              MOVE ZERO                  TO CTR-DTSTA
              MOVE ZERO                  TO CTR-DTDST
PAOLO         MOVE A16-MINCB             TO CTR-ID020
L00054        MOVE G20-PRLDI-MNB         TO CTR-PRLDI
              MOVE 'NPX09R'              TO CTR-CDOPE
              PERFORM Z0020-INSERT-CTR THRU Z0020-END
              IF CONTINUA
                 MOVE NPCTRT             TO X09-NPCTRT.
       A0210-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO TABELLA CONTATORI TP CON I VALORI INIZIALI    **
      *****************************************************************
       A0211-INSERT-CT1.
           MOVE    CT1-ABIUT           TO   WKS-ABIUT-A16.
           PERFORM A0220-LEGGI-A16     THRU A0220-END.
           IF CONTINUA
              MOVE X09-NPCTRT            TO NPCT1T
              MOVE A16-MINRP             TO CT1-CDRAP
              MOVE G20-CDDEB-MNT         TO CT1-CDDEB
              MOVE 2                     TO CT1-CDCNV
              MOVE G20-NRPRE-MNT         TO CT1-NRPRE
XUSU02*       MOVE 1                     TO CT1-NRVAL
XUSU02        MOVE 10001                 TO CT1-NRVAL
E00415*       MOVE 1                     TO CT1-NRRIM
E00415        MOVE G20-NRRIM-MNT         TO CT1-NRRIM
              MOVE G20-NRRES-MNT         TO CT1-NRRES
PAOLO         MOVE A16-MINCT             TO CT1-ID020
L00054        MOVE G20-PRLDI-MNT         TO CT1-PRLDI
              MOVE ZERO                  TO CT1-NMARD
              MOVE A16-MINMC             TO CT1-NMAMN
              MOVE 'NPX09R'              TO CTR-CDOPE
              PERFORM Z0021-INSERT-CT1 THRU Z0021-END
              IF CONTINUA
                 MOVE NPCT1T             TO X09-NPCTRT.
       A0211-END.
           EXIT.
      *****************************************************************
      **  INSERIMENTO TABELLA CONTATORI DRIVER CON I VALORI INIZIALI **
      *****************************************************************
       A0212-INSERT-CT2.
           IF CONTINUA
              MOVE X09-NPCTRT            TO NPCT2T
              MOVE G20-UNOPE-MNT         TO CT2-UNOPE
              MOVE 'NPX09R'              TO CT2-CDOPE
              PERFORM Z0022-INSERT-CT2 THRU Z0022-END
              IF CONTINUA
                 MOVE NPCT2T             TO X09-NPCTRT.
       A0212-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA RANGE A16                                 **
      *****************************************************************
       A0220-LEGGI-A16.
           MOVE SPACES          TO NPW01RC.
           MOVE SPACES          TO NPA16TC.
           MOVE 'A16'           TO A16-CODIT.
           MOVE WKS-ABIUT-A16   TO A16-ABIUT.
           MOVE 'RE'            TO W01-FUNZI.
           MOVE NPA16TC         TO W01-NPDATT.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              MOVE W01-NPDATT                 TO NPA16TC.
       A0220-END.
           EXIT.
CTR   *****************************************************************
CTR   **   LETTURA TABELLA RANGE A56                                 **
CTR   *****************************************************************
CTR    A0221-LEGGI-A56.
CTR   *
CTR        MOVE SPACES          TO NPW01RC.
CTR        MOVE SPACES          TO NPA56TC.
CTR        MOVE 'A56'           TO A56-CODIT.
CTR        MOVE WKS-ABIUT-A56   TO A56-ABIUT.
CTR        MOVE 'RE'            TO W01-FUNZI.
CTR        MOVE NPA56TC         TO W01-NPDATT.
CTR   *
CTR        PERFORM W0001-RICHIAMO THRU W0001-END.
CTR   *
CTR        IF W01-RCODE NOT EQUAL SPACES
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SPACES                     TO WRK-SW-USCITA
CTR        ELSE
CTR           MOVE W01-NPDATT                 TO NPA56TC.
CTR   *
CTR    A0221-END.
CTR        EXIT.
      *****************************************************************
SR0602**  LETTURA TABELLA RANGE T62                                 **
SR0602*****************************************************************
SR0602 A0222-LEGGI-T62.
SR0602*
SR0602     MOVE SPACES          TO NPW01RC.
SR0602     MOVE SPACES          TO NPT62TC.
SR0602     MOVE 'T62'           TO T62-CODIT.
SR0602     MOVE CTR-ABIUT       TO T62-ABIUT.
SR0602     MOVE 'RE'            TO W01-FUNZI.
SR0602     MOVE NPT62TC         TO W01-NPDATT.
SR0602*
SR0602     PERFORM W0001-RICHIAMO THRU W0001-END.
SR0602*
SR0602     IF W01-RCODE NOT EQUAL SPACES
SR0602        MOVE 'KO'                       TO X09-RCODE
SR0602        MOVE 'S'                        TO G01-TPERR
SR0602        MOVE SPACES                     TO WRK-SW-USCITA
SR0602     ELSE
SR0602        MOVE W01-NPDATT                 TO NPT62TC.
SR0602*
SR0602 A0222-END.
SR0602     EXIT.
      *****************************************************************
      **   DELETE TABELLA CONTATORI                                  **
      *****************************************************************
       A0300-DELETE-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT         TO NPCTRT
              PERFORM Z0030-DELETE-CTR THRU Z0030-END.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT         TO NPCT1T
              PERFORM Z0031-DELETE-CT1 THRU Z0031-END.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT         TO NPCT2T
              PERFORM Z0032-DELETE-CT2 THRU Z0032-END.
       A0300-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CONTATORI                           **
      *****************************************************************
       A0400-UPDATE-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT          TO NPCTRT
              PERFORM Z0040-UPDATE-CTR THRU Z0040-END.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT          TO NPCT1T
              PERFORM Z0041-UPDATE-CT1 THRU Z0041-END.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT          TO NPCT2T
              PERFORM Z0042-UPDATE-CT2 THRU Z0042-END.
       A0400-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-NRRIM   -TP-                    **
      *****************************************************************
       A0430-UPDATE-CT1.
           MOVE X09-NPCTRT            TO NPCT1T.
           PERFORM Z0043-UPDATE-CT1 THRU Z0043-END.
           MOVE NPCT1T               TO X09-NPCTRT.
       A0430-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRDXX  -BATCH-                  **
      *****************************************************************
       A0440-UPDATE-CTR.
           MOVE X09-NPCTRT            TO NPCTRT.
           MOVE CTR-ABIUT             TO WKS-ABIUT-A16.
           PERFORM A0220-LEGGI-A16  THRU A0220-END.
CTR        MOVE CTR-ABIUT          TO   WKS-ABIUT-A56.
CTR   *
           PERFORM A0221-LEGGI-A56 THRU A0221-END.
           IF CONTINUA
CTR           IF X09-FTPRE = CARTACEO
CTR              PERFORM Z0101-UPDATE-CTR01 THRU Z0101-END
CTR           ELSE
CTR           IF X09-FTPRE = RIBA
CTR              PERFORM Z0102-UPDATE-CTR02 THRU Z0102-END
CTR           ELSE
CTR           IF X09-FTPRE = MAV
CTR              PERFORM Z0104-UPDATE-CTR04 THRU Z0104-END
CTR           ELSE
CTR           IF X09-FTPRE = RID
CTR              PERFORM Z0105-UPDATE-CTR05 THRU Z0105-END
CTR           ELSE
CTR           IF X09-FTPRE = RIA
CTR              PERFORM Z0107-UPDATE-CTR07 THRU Z0107-END
CTR           ELSE
CTR           IF X09-FTPRE = DOCUMENTI
CTR              PERFORM Z0110-UPDATE-CTR10 THRU Z0110-END.
           IF CONTINUA
              MOVE NPCTRT                TO X09-NPCTRT.
       A0440-END.
           EXIT.

MO1006*****************************************************************
MO1006**   UPDATE TABELLA CTR PER PIU' DI UN'UNITA' (FUNZIONE        **
MO1006*    INTRODOTTA PER I MAV ON LINE)                              *
MO1006*****************************************************************
MO1006 A0630-UPDATE-CTR.
MO1006*
MO1006     MOVE X09-NPCTRT            TO NPCTRT
MO1006*
MO1006     MOVE CTR-ABIUT             TO WKS-ABIUT-A16
MO1006     PERFORM A0220-LEGGI-A16  THRU A0220-END
MO1006*
MO1006     MOVE CTR-ABIUT          TO   WKS-ABIUT-A56
MO1006*
MO1006     PERFORM A0221-LEGGI-A56 THRU A0221-END
MO1006*
MO1006     IF CONTINUA
MO1006        PERFORM ZB104-UPDATE-CTR04 THRU ZB104-END
MO1006     END-IF
MO1006*
MO1006     IF CONTINUA
MO1006        MOVE NPCTRT                TO X09-NPCTRT
MO1006     END-IF.
MO1006*
MO1006*
MO1006 A0630-END.
           EXIT.

      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-NRVAL -TP-                      **
      *****************************************************************
       A0450-UPDATE-CT1.
           MOVE X09-NPCTRT            TO NPCT1T.
           PERFORM Z0045-UPDATE-CT1 THRU Z0045-END.
           MOVE NPCT1T                TO X09-NPCTRT.
       A0450-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-UNOPE BATCH-TP                  **
      *****************************************************************
       A0460-UPDATE-CTR.
PEF001*    IF X09-FOPTB EQUAL 'B'
PEF001     IF X09-FOPTB EQUAL 'B' OR 'P'
              MOVE X09-NPCTRT            TO NPCTRT
              PERFORM Z0046-UPDATE-CTR THRU Z0046-END
              MOVE NPCTRT                TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'D'
              MOVE X09-NPCTRT            TO NPCT2T
              PERFORM Z0046-D-UPDATE-CT2 THRU Z0046-D-END
              MOVE NPCT2T                TO X09-NPCTRT.
       A0460-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-CDRAP  -TP-                     **
      *****************************************************************
       A0470-UPDATE-CT1.
           MOVE X09-NPCTRT             TO NPCT1T.
           MOVE CT1-ABIUT              TO WKS-ABIUT-A16.
           PERFORM A0220-LEGGI-A16     THRU A0220-END.
           IF CONTINUA
              PERFORM Z0047-UPDATE-CT1 THRU Z0047-END
              MOVE NPCT1T              TO X09-NPCTRT.
       A0470-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRI1 -BATCH-                   **
      *****************************************************************
       A0480-UPDATE-CTR.
           MOVE X09-NPCTRT            TO NPCTRT.
SR0602     PERFORM A0222-LEGGI-T62 THRU A0222-END.
           PERFORM Z0048-UPDATE-CTR THRU Z0048-END.
           MOVE NPCTRT               TO X09-NPCTRT.
       A0480-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-CDDEB  -BATCH-TP-               **
      *****************************************************************
       A0490-UPDATE-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT          TO NPCTRT
              PERFORM Z0049-UPDATE-CTR THRU Z0049-END
              MOVE NPCTRT              TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT            TO NPCT1T
              PERFORM Z0049-T-UPDATE-CT1 THRU Z0049-T-END
              MOVE NPCT1T                TO X09-NPCTRT.
       A0490-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-CDCNV  -TP-                     **
      *****************************************************************
       A0500-UPDATE-CT1.
           MOVE X09-NPCTRT            TO NPCT1T.
           PERFORM Z0050-UPDATE-CT1 THRU Z0050-END.
           MOVE NPCT1T               TO X09-NPCTRT.
       A0500-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRES  -BATCH-TP-               **
      *****************************************************************
       A0510-UPDATE-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT            TO NPCTRT
              PERFORM Z0051-UPDATE-CTR THRU Z0051-END
              MOVE NPCTRT                TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT              TO NPCT1T
              PERFORM Z0051-T-UPDATE-CT1 THRU Z0051-T-END
              MOVE NPCT1T                  TO X09-NPCTRT.
       A0510-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRPRE  -BATCH-TP-               **
      *****************************************************************
       A0520-UPDATE-CTR.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT            TO NPCTRT
              PERFORM Z0052-UPDATE-CTR THRU Z0052-END
              MOVE NPCTRT                TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT              TO NPCT1T
              PERFORM Z0052-T-UPDATE-CT1 THRU Z0052-T-END
              MOVE NPCT1T                  TO X09-NPCTRT.
       A0520-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRSBF  -BATCH-                  **
      *****************************************************************
       A0530-UPDATE-CTR.
           MOVE X09-NPCTRT            TO NPCTRT.
           PERFORM Z0053-UPDATE-CTR THRU Z0053-END.
           MOVE NPCTRT               TO X09-NPCTRT.
       A0530-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRMAV -BATCH-                   **
      *****************************************************************
       A0540-UPDATE-CTR.
           MOVE X09-NPCTRT          TO   NPCTRT.
           MOVE CTR-ABIUT           TO   WKS-ABIUT-A16.
           PERFORM A0220-LEGGI-A16  THRU A0220-END.
           IF CONTINUA
              PERFORM Z0054-UPDATE-CTR THRU Z0054-END
              MOVE NPCTRT                TO X09-NPCTRT.
       A0540-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-DTSTA -BATCH-                   **
      *****************************************************************
       A0550-UPDATE-DTSTA.
           MOVE X09-NPCTRT            TO NPCTRT.
           PERFORM Z0055-UPDATE-DTSTA THRU Z0055-END.
           MOVE NPCTRT                TO X09-NPCTRT.
       A0550-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-DTDST -BATCH-                   **
      *****************************************************************
       A0560-UPDATE-DTDST.
           MOVE X09-NPCTRT            TO NPCTRT.
           PERFORM Z0056-UPDATE-DTDST THRU Z0056-END.
           MOVE NPCTRT                TO X09-NPCTRT.
       A0560-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-NMAMN -TP-                      **
      *****************************************************************
       A0580-UPDATE-NMAMN.
           MOVE X09-NPCTRT            TO NPCT1T.
           PERFORM Z0058-UPDATE-NMAMN THRU Z0058-END.
           MOVE NPCT1T                TO X09-NPCTRT.
       A0580-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-ID020 -BATCH- / TP              **
      *****************************************************************
       A0590-UPDATE-ID020.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT         TO NPCTRT
              PERFORM Z0059-UPDATE-ID020 THRU Z0059-END
              MOVE NPCTRT             TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT         TO NPCT1T
              PERFORM Z0060-UPDATE-ID020 THRU Z0060-END
              MOVE NPCT1T             TO X09-NPCTRT.
       A0590-END.
           EXIT.
      *****************************************************************
PAOLO **   AGGIORNAMENTO TABELLA CTR-PRLDI -BATCH-TP-                **
      *****************************************************************
L00054 A0600-UPDATE-PRLDI.
           IF X09-FOPTB EQUAL 'B'
              MOVE X09-NPCTRT         TO NPCTRT
              PERFORM Z0061-UPDATE-PRLDI THRU Z0061-END
              MOVE NPCTRT             TO X09-NPCTRT.
           IF X09-FOPTB EQUAL 'T'
              MOVE X09-NPCTRT         TO NPCT1T
              PERFORM Z0062-UPDATE-PRLDI THRU Z0062-END
              MOVE NPCT1T             TO X09-NPCTRT.
L00054 A0600-END.
           EXIT.
      *****************************************************************
      **   GENERAZIONE NUOVO NOPER                                   **
      *****************************************************************
SEQUEN A0610-NEXT-VALUE.
SEQUEN*
SEQUEN     MOVE X09-NPCTRT            TO NPCTRT.
SEQUEN     PERFORM Z0120-NEXT-VALUE THRU Z0120-END.
SEQUEN*
SEQUEN A0610-END.
           EXIT.
      *****************************************************************
      **   GENERAZIONE NUOVO NRD01                                   **
      *****************************************************************
SEQD01 A0620-NEXT-NRD01.
SEQD01*
SEQD01     MOVE X09-NPCTRT            TO NPCTRT.
SEQD01     PERFORM Z0130-NEXT-NRD01 THRU Z0130-END.
SEQD01*
SEQD01 A0620-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI BATCH                           **
      *****************************************************************
       Z0010-READ-CTR.
           EXEC SQL
              SELECT  CTR_ABIUT,
                      CTR_PRFIL,
                      CTR_UNOPE,
                      CTR_CDDEB,
                      CTR_NRPRE,
                      CTR_NRD01,
                      CTR_NRD02,
                      CTR_NRD04,
                      CTR_NRD05,
                      CTR_NRD07,
                      CTR_NRD10,
                      CTR_NRMAV,
                      CTR_NRDAS,
                      CTR_NRRI1,
                      CTR_NRRES,
                      CTR_NRSBF,
                      CTR_DTSTA,
                      CTR_DTDST,
                      CTR_ID020,
                      CTR_CDOPE,
L00054                CTR_PRLDI
                INTO  :CTR-ABIUT,
                      :CTR-PRFIL,
                      :CTR-UNOPE,
                      :CTR-CDDEB,
                      :CTR-NRPRE,
                      :CTR-NRD01,
                      :CTR-NRD02,
                      :CTR-NRD04,
                      :CTR-NRD05,
                      :CTR-NRD07,
                      :CTR-NRD10,
                      :CTR-NRMAV,
                      :CTR-NRDAS,
                      :CTR-NRRI1,
                      :CTR-NRRES,
                      :CTR-NRSBF,
                      :CTR-DTSTA,
                      :CTR-DTDST,
                      :CTR-ID020,
                      :CTR-CDOPE,
L00054                :CTR-PRLDI
              FROM NPCTRT
                WHERE CTR_ABIUT = :CTR-ABIUT
                  AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  SQLCODE                   TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0010-READ-CTR'           TO G01-LABEL
              MOVE 'LETTURA CONTATORI'        TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
             IF SQLCODE EQUAL CENTO
                PERFORM A0210-INSERT-CTR     THRU A0210-END
             ELSE
                MOVE NPCTRT                     TO X09-NPCTRT.
       Z0010-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI PER TP                          **
      *****************************************************************
       Z0011-READ-CT1.
           EXEC SQL
              SELECT  CT1_ABIUT,
                      CT1_PRFIL,
                      CT1_CDRAP,
                      CT1_CDDEB,
                      CT1_CDCNV,
                      CT1_NRPRE,
                      CT1_NRVAL,
                      CT1_NRRIM,
                      CT1_NRRES,
                      CT1_NMARD,
                      CT1_NMAMN,
                      CT1_ID020,
                      CT1_CDOPE,
L00054                CT1_PRLDI
                INTO  :CT1-ABIUT,
                      :CT1-PRFIL,
                      :CT1-CDRAP,
                      :CT1-CDDEB,
                      :CT1-CDCNV,
                      :CT1-NRPRE,
                      :CT1-NRVAL,
                      :CT1-NRRIM,
                      :CT1-NRRES,
                      :CT1-NMARD,
                      :CT1-NMAMN,
                      :CT1-ID020,
                      :CT1-CDOPE,
L00054                :CT1-PRLDI
              FROM NPCT1T
                WHERE CT1_ABIUT = :CT1-ABIUT
                  AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  SQLCODE                   TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0011-READ-CT1'           TO G01-LABEL
              MOVE 'LETTURA CONTATORI TP'     TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
             IF SQLCODE EQUAL CENTO
                PERFORM A0211-INSERT-CT1     THRU A0211-END
             ELSE
                MOVE NPCT1T                    TO X09-NPCTRT.
       Z0011-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA CONTATORI PER DRIVER                      **
      *****************************************************************
       Z0012-READ-CT2.
           EXEC SQL
              SELECT  CT2_ABIUT,
                      CT2_PRFIL,
                      CT2_UNOPE,
                      CT2_CDOPE
                INTO  :CT2-ABIUT,
                      :CT2-PRFIL,
                      :CT2-UNOPE,
                      :CT2-CDOPE
              FROM NPCT2T
                WHERE CT2_ABIUT = :CT2-ABIUT
                  AND CT2_PRFIL = :CT2-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT2-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT2-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE  SQLCODE                   TO G01-PIC3
              MOVE 'NPCT2T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0012-READ-CT2'           TO G01-LABEL
              MOVE 'LETTURA CONTATORI DRIVER' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
             IF SQLCODE EQUAL CENTO
                PERFORM A0212-INSERT-CT2     THRU A0212-END
             ELSE
                MOVE NPCT2T                    TO X09-NPCTRT.
       Z0012-END.
           EXIT.
PEF001*****************************************************************
PEF001**   LETTURA TABELLA CONTATORI PERFORMANCE (GESTIONE CONTESE)  **
PEF001*****************************************************************
PEF001 Z0013-GEST-CTR.
PEF001*
PEF001     PERFORM VARYING WKS-IND FROM 1 BY 1
PEF001       UNTIL WKS-IND GREATER WKS-IND-MAX
PEF001          OR WKS-TUTTO-OK EQUAL 'SI'
PEF001         PERFORM Z0014-READ-CTR  THRU Z0014-END
PEF001     END-PERFORM.
PEF001*
PEF001 Z0013-END.
PEF001     EXIT.
PEF001*****************************************************************
PEF001**   LETTURA TABELLA CONTATORI PERFORMANCE (GESTIONE CONTESE)  **
PEF001*****************************************************************
PEF001 Z0014-READ-CTR.
PEF001*
PEF001     EXEC SQL
PEF001        SELECT  CTR_ABIUT,
PEF001                CTR_PRFIL,
PEF001                CTR_UNOPE,
PEF001                CTR_CDDEB,
PEF001                CTR_NRPRE,
PEF001                CTR_NRD01,
PEF001                CTR_NRD02,
PEF001                CTR_NRD04,
PEF001                CTR_NRD05,
PEF001                CTR_NRD07,
PEF001                CTR_NRD10,
PEF001                CTR_NRMAV,
PEF001                CTR_NRDAS,
PEF001                CTR_NRRI1,
PEF001                CTR_NRRES,
PEF001                CTR_NRSBF,
PEF001                CTR_DTSTA,
PEF001                CTR_DTDST,
PEF001                CTR_ID020,
PEF001                CTR_CDOPE,
PEF001                CTR_PRLDI
PEF001*
PEF001          INTO  :CTR-ABIUT,
PEF001                :CTR-PRFIL,
PEF001                :CTR-UNOPE,
PEF001                :CTR-CDDEB,
PEF001                :CTR-NRPRE,
PEF001                :CTR-NRD01,
PEF001                :CTR-NRD02,
PEF001                :CTR-NRD04,
PEF001                :CTR-NRD05,
PEF001                :CTR-NRD07,
PEF001                :CTR-NRD10,
PEF001                :CTR-NRMAV,
PEF001                :CTR-NRDAS,
PEF001                :CTR-NRRI1,
PEF001                :CTR-NRRES,
PEF001                :CTR-NRSBF,
PEF001                :CTR-DTSTA,
PEF001                :CTR-DTDST,
PEF001                :CTR-ID020,
PEF001                :CTR-CDOPE,
PEF001                :CTR-PRLDI
PEF001        FROM NPCTRT
PEF001          WHERE CTR_ABIUT = :CTR-ABIUT
PEF001            AND CTR_PRFIL = :CTR-PRFIL
PEF001           WITH UR
PEF001     END-EXEC.
PEF001*
PEF001     IF SQLCODE NOT EQUAL ZERO AND
PEF001        SQLCODE NOT EQUAL CENTO AND
PEF001        SQLCODE NOT EQUAL -911
PEF001        MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
PEF001        MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
PEF001        MOVE WKS-CHIAVE-XX              TO G01-DESC5
PEF001        MOVE 'KO'                       TO X09-RCODE
PEF001        MOVE 'S'                        TO G01-TPERR
PEF001        MOVE  SQLCODE                   TO G01-PIC3
PEF001        MOVE 'NPCTRT'                   TO G01-ARCHI
PEF001        MOVE  WRK-PGMNM                 TO G01-ROUNM
PEF001        MOVE 'Z0014-READ-CTR'           TO G01-LABEL
PEF001        MOVE 'LETTURA CONTATORI'        TO G01-DESC1
PEF001        MOVE SQLCA                      TO G01-SQLCA
PEF001        MOVE SPACES                     TO WRK-SW-USCITA
PEF001     ELSE
PEF001       IF SQLCODE EQUAL -911
PEF001          IF WKS-IND EQUAL 6
PEF001             MOVE CTR-ABIUT             TO WKS-ABIUT-XX
PEF001             MOVE CTR-PRFIL             TO WKS-PRFIL-XX
PEF001             MOVE WKS-CHIAVE-XX         TO G01-DESC5
PEF001             MOVE 'KO'                  TO X09-RCODE
PEF001             MOVE 'S'                   TO G01-TPERR
PEF001             MOVE SQLCODE               TO G01-PIC3
PEF001             MOVE 'NPCTRT'              TO G01-ARCHI
PEF001             MOVE WRK-PGMNM             TO G01-ROUNM
PEF001             MOVE 'Z0014-READ-CTR'      TO G01-LABEL
PEF001             MOVE 'LETTURA CONTATORI'   TO G01-DESC1
PEF001             MOVE SQLCA                 TO G01-SQLCA
PEF001             MOVE SPACES                TO WRK-SW-USCITA
PEF001          END-IF
PEF001       ELSE
PEF001          IF SQLCODE EQUAL CENTO
PEF001             PERFORM A0210-INSERT-CTR THRU A0210-END
PEF001             IF CONTINUA
PEF001                MOVE 'SI'               TO WKS-TUTTO-OK
PEF001             END-IF
PEF001          ELSE
PEF001             MOVE 'SI'                  TO WKS-TUTTO-OK
PEF001             MOVE NPCTRT                TO X09-NPCTRT.
PEF001*
PEF001 Z0014-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO CONTATORI                                     **
      *****************************************************************
       Z0020-INSERT-CTR.
           EXEC SQL INSERT
             INTO     NPCTRT
             VALUES (:NPCTRT)
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL -803
              MOVE CTR-ABIUT                   TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                   TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX               TO G01-DESC5
              MOVE 'KO'                        TO X09-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE 'NPCTRT'                    TO G01-ARCHI
              MOVE  WRK-PGMNM                  TO G01-ROUNM
              MOVE 'Z0020-INSERT-CTR'          TO G01-LABEL
              MOVE 'INSERT TAB.CONTAT. BATCH'  TO G01-DESC1
              MOVE SQLCA                       TO G01-SQLCA
              MOVE SPACES                      TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL -803
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'DP'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0020-INSERT-CTR'         TO G01-LABEL
                 MOVE 'INSERT TAB.CONTAT. BATCH' TO G01-DESC1
                 MOVE 'CHIAVE DUPLICATA'         TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0020-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO CONTATORI TP                                  **
      *****************************************************************
       Z0021-INSERT-CT1.
           EXEC SQL INSERT
             INTO     NPCT1T
             VALUES (:NPCT1T)
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL -803
              MOVE CT1-ABIUT                   TO WKS-ABIUT-XX
              MOVE CT1-PRFIL                   TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX               TO G01-DESC5
              MOVE 'KO'                        TO X09-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE 'NPCT1T'                    TO G01-ARCHI
              MOVE  WRK-PGMNM                  TO G01-ROUNM
              MOVE 'Z0021-INSERT-CT1'          TO G01-LABEL
              MOVE 'INSERT TABELLA CONTAT.TP'  TO G01-DESC1
              MOVE SQLCA                       TO G01-SQLCA
              MOVE SPACES                      TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL -803
                 MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'DP'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT1T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0021-INSERT-CT1'         TO G01-LABEL
                 MOVE 'INSERT TABELLA CONTAT.TP' TO G01-DESC1
                 MOVE 'CHIAVE DUPLICATA'         TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0021-END.
           EXIT.
      *****************************************************************
      **   INSERIMENTO CONTATORI DRIVER                              **
      *****************************************************************
       Z0022-INSERT-CT2.
           EXEC SQL INSERT
             INTO     NPCT2T
             VALUES (:NPCT2T)
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL -803
              MOVE CT2-ABIUT                   TO WKS-ABIUT-XX
              MOVE CT2-PRFIL                   TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX               TO G01-DESC5
              MOVE 'KO'                        TO X09-RCODE
              MOVE 'S'                         TO G01-TPERR
              MOVE SQLCODE                     TO G01-PIC3
              MOVE 'NPCT2T'                    TO G01-ARCHI
              MOVE  WRK-PGMNM                  TO G01-ROUNM
              MOVE 'Z0022-INSERT-CT2'          TO G01-LABEL
              MOVE 'INSERT TAB.CONTAT. DRIVER' TO G01-DESC1
              MOVE SQLCA                       TO G01-SQLCA
              MOVE SPACES                      TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL -803
                 MOVE CT2-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT2-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'DP'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT2T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0022-INSERT-CT2'         TO G01-LABEL
                 MOVE 'INSERT TAB.CONTAT.DRIVER' TO G01-DESC1
                 MOVE 'CHIAVE DUPLICATA'         TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0022-END.
           EXIT.
      *****************************************************************
      **   DELETE TABELLA CONTATORI BATCH                            **
      *****************************************************************
       Z0030-DELETE-CTR.
           EXEC SQL DELETE FROM NPCTRT
                WHERE CTR_ABIUT = :CTR-ABIUT
                  AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0030-DELETE-CTR'         TO G01-LABEL
              MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0030-DELETE-CTR'         TO G01-LABEL
                 MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0030-END.
           EXIT.
      *****************************************************************
      **   DELETE TABELLA CONTATORI TP                               **
      *****************************************************************
       Z0031-DELETE-CT1.
           EXEC SQL DELETE FROM NPCT1T
                WHERE CT1_ABIUT = :CT1-ABIUT
                  AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0031-DELETE-CT1'         TO G01-LABEL
              MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT1T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0031-DELETE-CT1'         TO G01-LABEL
                 MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0031-END.
           EXIT.
      *****************************************************************
      **   DELETE TABELLA CONTATORI DRIVER                           **
      *****************************************************************
       Z0032-DELETE-CT2.
           EXEC SQL DELETE FROM NPCT2T
                WHERE CT2_ABIUT = :CT2-ABIUT
                  AND CT2_PRFIL = :CT2-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT2-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT2-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT2T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0032-DELETE-CT2'         TO G01-LABEL
              MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CT2-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT2-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT2T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0032-DELETE-CT2'         TO G01-LABEL
                 MOVE 'DELETE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0032-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CONTATORI BATCH                     **
      *****************************************************************
       Z0040-UPDATE-CTR.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_UNOPE = :CTR-UNOPE,
                    CTR_CDDEB = :CTR-CDDEB,
                    CTR_NRPRE = :CTR-NRPRE,
                    CTR_NRD01 = :CTR-NRD01,
                    CTR_NRD02 = :CTR-NRD02,
                    CTR_NRD04 = :CTR-NRD04,
                    CTR_NRD05 = :CTR-NRD05,
                    CTR_NRD07 = :CTR-NRD07,
                    CTR_NRD10 = :CTR-NRD10,
                    CTR_NRMAV = :CTR-NRMAV,
                    CTR_NRDAS = :CTR-NRDAS,
                    CTR_NRRI1 = :CTR-NRRI1,
                    CTR_NRRES = :CTR-NRRES,
                    CTR_NRSBF = :CTR-NRSBF,
                    CTR_DTSTA = :CTR-DTSTA,
                    CTR_DTDST = :CTR-DTDST,
                    CTR_ID020 = :CTR-ID020,
                    CTR_CDOPE = :CTR-CDOPE,
L00054              CTR_PRLDI = :CTR-PRLDI
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0040-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0040-UPDATE-CTR'         TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0040-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CONTATORI TP                        **
      *****************************************************************
       Z0041-UPDATE-CT1.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_CDRAP = :CT1-CDRAP,
                    CT1_CDDEB = :CT1-CDDEB,
                    CT1_CDCNV = :CT1-CDCNV,
                    CT1_NRPRE = :CT1-NRPRE,
                    CT1_NRVAL = :CT1-NRVAL,
                    CT1_NRRIM = :CT1-NRRIM,
                    CT1_NRRES = :CT1-NRRES,
                    CT1_NMARD = :CT1-NMARD,
                    CT1_NMAMN = :CT1-NMAMN,
                    CT1_ID020 = :CT1-ID020,
                    CT1_CDOPE = :CT1-CDOPE,
L00054              CT1_PRLDI = :CT1-PRLDI
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0041-UPDATE-CT1'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT1T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0041-UPDATE-CT1'         TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0041-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CONTATORI DRIVER                    **
      *****************************************************************
       Z0042-UPDATE-CT2.
           EXEC SQL
             UPDATE NPCT2T
                SET CT2_UNOPE = :CT2-UNOPE,
                    CT2_CDOPE = :CT2-CDOPE
              WHERE CT2_ABIUT = :CT2-ABIUT
                AND CT2_PRFIL = :CT2-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT2-ABIUT                  TO WKS-ABIUT-XX
              MOVE CT2-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT2T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0042-UPDATE-CT2'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CT1-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CT1-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCT2T'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0042-UPDATE-CT2'         TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0042-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRIM                           **
      *****************************************************************
       Z0043-UPDATE-CT1.
       Z0043-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_NRRIM = :CT1-NRRIM
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_NRRIM = :CT1-NRRIM - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X6
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CT1-NRRIM GIVING   WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0043-UPDATE-CT1'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-NRRIM EQUAL 9999999999
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-NRRIM = CT1-NRRIM + 1
                    GO TO Z0043-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-NRRIM          EQUAL (G20-NRRIM-MNT + 1)
E00419              PERFORM Z0043U-D-UPD THRU Z0043U-D-END
E00419           ELSE
                    COMPUTE CT1-NRRIM = CT1-NRRIM + 1
                    GO TO Z0043-INIZIO.
       Z0043-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0043U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_NRRIM = :CT1-NRRIM
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X1
              MOVE CT1-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRRIM GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0046-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0043U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRVAL                           **
      *****************************************************************
       Z0045-UPDATE-CT1.
       Z0045-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_NRVAL = :CT1-NRVAL
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_NRVAL = :CT1-NRVAL - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X1
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRVAL GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0045-UPDATE-CT1'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-NRVAL EQUAL 999999999999999
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-NRVAL = CT1-NRVAL + 1
                    GO TO Z0045-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-NRVAL          EQUAL  2
E00419              PERFORM Z0045U-D-UPD THRU Z0045U-D-UPD
E00419           ELSE
                    COMPUTE CT1-NRVAL = CT1-NRVAL + 1
                    GO TO Z0045-INIZIO.
       Z0045-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0045U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_NRVAL = :CT1-NRVAL
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X1
              MOVE CT1-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRVAL GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0045-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0045U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-UNOPE -BATCH-                   **
      *****************************************************************
       Z0046-UPDATE-CTR.
       Z0046-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_UNOPE = :CTR-UNOPE
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_UNOPE = :CTR-UNOPE - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X2
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X2
              SUBTRACT 1 FROM CTR-UNOPE GIVING   WKS-UNOPE-X2
              MOVE WKS-CHIAVE-X2              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0046-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-UNOPE EQUAL G20-UNOPE-MXB
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-UNOPE = CTR-UNOPE + 1
                    GO TO Z0046-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
211209           IF CTR-UNOPE          EQUAL (G20-UNOPE-MNB + 1)
E00419              PERFORM Z0046U-D-UPD THRU Z0046U-D-UPD
E00419           ELSE
                 COMPUTE CTR-UNOPE = CTR-UNOPE + 1
                 GO TO Z0046-INIZIO.
       Z0046-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0046U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_UNOPE = :CTR-UNOPE
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X2
              MOVE CTR-PRFIL    TO WKS-PRFIL-X2
              SUBTRACT 1 FROM CTR-UNOPE GIVING
                            WKS-UNOPE-X2
              MOVE WKS-CHIAVE-X2 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0046-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0046U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-UNOPE -DRIVER-                  **
      *****************************************************************
       Z0046-D-UPDATE-CT2.
       Z0046-D-INIZIO.
           EXEC SQL
             UPDATE NPCT2T
                SET CT2_UNOPE = :CT2-UNOPE
              WHERE CT2_ABIUT = :CT2-ABIUT
                AND CT2_PRFIL = :CT2-PRFIL
                AND CT2_UNOPE = :CT2-UNOPE - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT2-ABIUT                  TO WKS-ABIUT-X2
              MOVE CT2-PRFIL                  TO WKS-PRFIL-X2
              SUBTRACT 1 FROM CT2-UNOPE GIVING   WKS-UNOPE-X2
              MOVE WKS-CHIAVE-X2              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT2T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0046-D-UPDATE-CT2'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT2-UNOPE EQUAL G20-UNOPE-MXT
                 PERFORM Z0012-READ-CT2  THRU Z0012-END
                 IF CONTINUA
                    COMPUTE CT2-UNOPE = CT2-UNOPE + 1
                    GO TO Z0046-D-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
                 COMPUTE CT2-UNOPE = CT2-UNOPE + 1
                 GO TO Z0046-D-INIZIO.
       Z0046-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-CDRAP                           **
      *****************************************************************
       Z0047-UPDATE-CT1.
       Z0047-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_CDRAP = :CT1-CDRAP
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_CDRAP = :CT1-CDRAP - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X3
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X3
              SUBTRACT 1 FROM CT1-CDRAP GIVING   WKS-CDRAP-X3
              MOVE WKS-CHIAVE-X3              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0047-UPDATE-CT1'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-CDRAP EQUAL A16-MAXRP
                 PERFORM Z0011-READ-CT1  THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-CDRAP = CT1-CDRAP + 1
                    GO                  TO Z0047-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-CDRAP          EQUAL (A16-MINRP + 1)
E00419              PERFORM Z0047U-D-UPD THRU Z0047U-D-UPD
E00419           ELSE
                    COMPUTE CT1-CDRAP = CT1-CDRAP + 1
                    GO                     TO Z0047-INIZIO.
       Z0047-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0047U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_CDRAP = :CT1-CDRAP
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X3
              MOVE CT1-PRFIL    TO WKS-PRFIL-X3
              SUBTRACT 1 FROM CT1-CDRAP GIVING
                            WKS-CDRAP-X3
              MOVE WKS-CHIAVE-X3 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0047-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0047U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRI1                           **
      *****************************************************************
       Z0048-UPDATE-CTR.
       Z0048-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_NRRI1 = :CTR-NRRI1
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_NRRI1 = :CTR-NRRI1 - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRRI1 GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0048-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
SR0603      AND CTR-PRFIL NOT = T62-FILTR
              IF CTR-NRRI1 EQUAL 999999999999999
                  PERFORM Z0010-READ-CTR THRU Z0010-END
                  IF CONTINUA
                     COMPUTE CTR-NRRI1 = CTR-NRRI1 + 1
                     GO TO Z0048-INIZIO
                  ELSE
                     NEXT SENTENCE
              ELSE
E00419            IF CTR-NRRI1          EQUAL (G20-NRRI1-MNB + 1)
E00419               PERFORM Z0048U-D-UPD THRU Z0048U-D-UPD
E00419            ELSE
                     COMPUTE CTR-NRRI1 = CTR-NRRI1 + 1
                     GO TO Z0048-INIZIO.
SR0603     IF SQLCODE EQUAL CENTO
SR0603      AND CTR-PRFIL = T62-FILTR
SR0603        IF CTR-NRRI1 EQUAL T62-MAXRS
SR0603            PERFORM Z0010-READ-CTR THRU Z0010-END
SR0603            IF CONTINUA
SR0603               COMPUTE CTR-NRRI1 = CTR-NRRI1 + 1
SR0603               GO TO Z0048-INIZIO
SR0603            ELSE
SR0603               NEXT SENTENCE
SR0603        ELSE
SR0603            IF CTR-NRRI1          EQUAL (T62-MINRS + 1)
SR0603               PERFORM Z0048U-D-UPD THRU Z0048U-D-UPD
SR0603            ELSE
SR0603               COMPUTE CTR-NRRI1 = CTR-NRRI1 + 1
SR0603               GO TO Z0048-INIZIO.
       Z0048-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0048U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRRI1 = :CTR-NRRI1
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRRI1 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0048-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0048U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-CDDEB  -BATCH-                  **
      *****************************************************************
       Z0049-UPDATE-CTR.
       Z0049-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_CDDEB = :CTR-CDDEB
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_CDDEB = :CTR-CDDEB - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X4
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X4
              SUBTRACT 1 FROM CTR-CDDEB GIVING   WKS-CDDEB-X4
              MOVE WKS-CHIAVE-X4              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0049-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-CDDEB EQUAL G20-CDDEB-MXB
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-CDDEB = CTR-CDDEB + 1
                    GO TO Z0049-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-CDDEB          EQUAL (G20-CDDEB-MNT + 1)
E00419              PERFORM Z0049U-D-UPD THRU Z0049U-D-END
E00419           ELSE
                    COMPUTE CTR-CDDEB = CTR-CDDEB + 1
                    GO TO Z0049-INIZIO.
       Z0049-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0049U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_CDDEB = :CTR-CDDEB
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X4
              MOVE CTR-PRFIL    TO WKS-PRFIL-X4
              SUBTRACT 1 FROM CTR-CDDEB GIVING
                            WKS-CDDEB-X4
              MOVE WKS-CHIAVE-X4 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0049-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0049U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-CDDEB -TP-                      **
      *****************************************************************
       Z0049-T-UPDATE-CT1.
       Z0049-T-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_CDDEB = :CT1-CDDEB
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_CDDEB = :CT1-CDDEB - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X4
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X4
              SUBTRACT 1 FROM CT1-CDDEB GIVING   WKS-CDDEB-X4
              MOVE WKS-CHIAVE-X4              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0049-T-UPDATE-CT1'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-CDDEB EQUAL G20-CDDEB-MXT
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-CDDEB = CT1-CDDEB + 1
                    GO TO Z0049-T-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-CDDEB          EQUAL (G20-CDDEB-MNT + 1)
E00419              PERFORM Z0049T-D-UPD THRU Z0049T-D-UPD
E00419           ELSE
                    COMPUTE CT1-CDDEB = CT1-CDDEB + 1
                    GO TO Z0049-T-INIZIO.
       Z0049-T-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0049T-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_CDDEB = :CT1-CDDEB
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X4
              MOVE CT1-PRFIL    TO WKS-PRFIL-X4
              SUBTRACT 1 FROM CT1-CDDEB GIVING
                            WKS-CDDEB-X4
              MOVE WKS-CHIAVE-X4 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0049T-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0049T-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-CDCNV                           **
      *****************************************************************
       Z0050-UPDATE-CT1.
       Z0050-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_CDCNV = :CT1-CDCNV
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_CDCNV = :CT1-CDCNV - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X5
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X5
              SUBTRACT 1 FROM CT1-CDCNV GIVING   WKS-CDCNV-X5
              MOVE WKS-CHIAVE-X5              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0050-UPDATE-CT1'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-CDCNV EQUAL 999999999999999
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-CDCNV = CT1-CDCNV + 1
                    GO TO Z0050-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-CDCNV          EQUAL 3
E00419              PERFORM Z0050U-D-UPD THRU Z0050U-D-UPD
E00419           ELSE
                    COMPUTE CT1-CDCNV = CT1-CDCNV + 1
                    GO TO Z0050-INIZIO.
       Z0050-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0050U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_CDCNV = :CT1-CDCNV
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X5
              MOVE CT1-PRFIL    TO WKS-PRFIL-X5
              SUBTRACT 1 FROM CT1-CDCNV GIVING
                            WKS-CDCNV-X5
              MOVE WKS-CHIAVE-X5 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0050-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0050U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRES -BATCH-                   **
      *****************************************************************
       Z0051-UPDATE-CTR.
       Z0051-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_NRRES = :CTR-NRRES
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_NRRES = :CTR-NRRES - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRRES GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0051-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-NRRES EQUAL G20-NRRES-MXB
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-NRRES = CTR-NRRES + 1
                    GO TO Z0051-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-NRRES          EQUAL (G20-NRRES-MNT + 1)
E00419              PERFORM Z0051U-D-UPD THRU Z0051U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRRES = CTR-NRRES + 1
                    GO TO Z0051-INIZIO.
       Z0051-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0051U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRRES = :CTR-NRRES
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRRES GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0051-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0051U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRRES -TP-                      **
      *****************************************************************
       Z0051-T-UPDATE-CT1.
       Z0051-T-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_NRRES = :CT1-NRRES
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_NRRES = :CT1-NRRES - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X1
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRRES GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0051-T-UPDATE-CT1'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-NRRES EQUAL G20-NRRES-MXT
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-NRRES = CT1-NRRES + 1
                    GO TO Z0051-T-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-NRRES          EQUAL (G20-NRRES-MNT + 1)
E00419              PERFORM Z0051T-D-UPD THRU Z0051T-D-UPD
E00419           ELSE
                    COMPUTE CT1-NRRES = CT1-NRRES + 1
                    GO TO Z0051-T-INIZIO.
       Z0051-T-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0051T-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_NRRES = :CT1-NRRES
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X1
              MOVE CT1-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRRES GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0051T-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0051T-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRPRE -BATCH-                   **
      *****************************************************************
       Z0052-UPDATE-CTR.
       Z0052-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_NRPRE = :CTR-NRPRE
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_NRPRE = :CTR-NRPRE - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X6
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CTR-NRPRE GIVING   WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0052-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-NRPRE  EQUAL G20-NRPRE-MXB
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-NRPRE = CTR-NRPRE + 1
                    GO TO Z0052-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-NRPRE          EQUAL (G20-NRPRE-MNT + 1)
E00419              PERFORM Z0052U-D-UPD THRU Z0052U-D-UPD
E00419           ELSE
                 COMPUTE CTR-NRPRE = CTR-NRPRE + 1
                 GO TO Z0052-INIZIO.
       Z0052-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0052U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRPRE = :CTR-NRPRE
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRPRE GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0052-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0052U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-NRPRE -TP-                      **
      *****************************************************************
       Z0052-T-UPDATE-CT1.
       Z0052-T-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_NRPRE = :CT1-NRPRE
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_NRPRE = :CT1-NRPRE - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X6
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CT1-NRPRE GIVING   WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0052-T-UPDATE-CT1'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-NRPRE EQUAL G20-NRPRE-MXT
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-NRPRE = CT1-NRPRE + 1
                    GO TO Z0052-T-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-NRPRE          EQUAL (G20-NRPRE-MNT + 1)
E00419              PERFORM Z0052T-D-UPD THRU Z0052T-D-UPD
E00419           ELSE
                    COMPUTE CT1-NRPRE = CT1-NRPRE + 1
                    GO TO Z0052-T-INIZIO.
       Z0052-T-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0052T-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_NRPRE = :CT1-NRPRE
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X1
              MOVE CT1-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NRPRE GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0052T-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0052T-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRSBF                           **
      *****************************************************************
       Z0053-UPDATE-CTR.
       Z0053-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_NRSBF = :CTR-NRSBF
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_NRSBF = :CTR-NRSBF - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRSBF GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0053-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-NRSBF EQUAL 9999999999
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-NRSBF = CTR-NRSBF + 1
                    GO TO Z0053-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-NRSBF          EQUAL 2
E00419              PERFORM Z0053U-D-UPD THRU Z0053U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRSBF = CTR-NRSBF + 1
                    GO TO Z0053-INIZIO.

       Z0053-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0053U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRSBF = :CTR-NRSBF
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRSBF GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0053-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0053U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-NRMAV                           **
      *****************************************************************
       Z0054-UPDATE-CTR.
       Z0054-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_NRMAV = :CTR-NRMAV
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_NRMAV = :CTR-NRMAV - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X7
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X7
              SUBTRACT 1 FROM CTR-NRMAV GIVING   WKS-CAMPONR-X7
              MOVE WKS-CHIAVE-X7              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0054-UPDATE-CTR'         TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-NRMAV EQUAL A16-MAXMA
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-NRMAV = CTR-NRMAV + 1
                    GO TO Z0054-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-NRMAV          EQUAL (A16-MINMA + 1)
E00419              PERFORM Z0054U-D-UPD THRU Z0054U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRMAV = CTR-NRMAV + 1
                    GO TO Z0054-INIZIO.
       Z0054-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0054U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRMAV = :CTR-NRMAV
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X7
              MOVE CTR-PRFIL    TO WKS-PRFIL-X7
              SUBTRACT 1 FROM CTR-NRMAV GIVING
                            WKS-CAMPONR-X7
              MOVE WKS-CHIAVE-X7 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0054-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0054U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-DTSTA                           **
      *****************************************************************
       Z0055-UPDATE-DTSTA.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_DTSTA = :CTR-DTSTA
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0055-UPDATE-DTSTA'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0055-UPDATE-DTSTA'       TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0055-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-DTDST                           **
      *****************************************************************
       Z0056-UPDATE-DTDST.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_DTDST = :CTR-DTDST
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0056-UPDATE-DTDST'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0056-UPDATE-DTDST'       TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0056-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-NMAMN                           **
      *****************************************************************
       Z0058-UPDATE-NMAMN.
       Z0058-INIZIO-01.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_NMAMN = :CT1-NMAMN
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_NMAMN = :CT1-NMAMN - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X1
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NMAMN GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0058-UPDATE-MNAMN'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-NMAMN EQUAL A16-MAXSE
                 PERFORM Z0011-READ-CT1       THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-NMAMN = CT1-NMAMN + 1
                    GO              TO Z0058-INIZIO-01
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-NMAMN          EQUAL (A16-MINMC + 1)
E00419              PERFORM Z0058U-D-UPD THRU Z0058U-D-UPD
E00419           ELSE
                    COMPUTE CT1-NMAMN = CT1-NMAMN + 1
                    GO                 TO Z0058-INIZIO-01.
       Z0058-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0058U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_NMAMN = :CT1-NMAMN
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X1
              MOVE CT1-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-NMAMN GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0058-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0058U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CTR-ID020                           **
      *****************************************************************
       Z0059-UPDATE-ID020.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_ID020 = :CTR-ID020
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
              MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
              MOVE WKS-CHIAVE-XX              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0059-UPDATE-ID020'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA
           ELSE
              IF SQLCODE EQUAL CENTO
                 MOVE CTR-ABIUT                  TO WKS-ABIUT-XX
                 MOVE CTR-PRFIL                  TO WKS-PRFIL-XX
                 MOVE WKS-CHIAVE-XX              TO G01-DESC5
                 MOVE 'GE'                       TO X09-RCODE
                 MOVE 'S'                        TO G01-TPERR
                 MOVE SQLCODE                    TO G01-PIC3
                 MOVE 'NPCTRT'                   TO G01-ARCHI
                 MOVE  WRK-PGMNM                 TO G01-ROUNM
                 MOVE 'Z0059-UPDATE-ID020'       TO G01-LABEL
                 MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
                 MOVE 'CHIAVE NON TROVATA'       TO G01-DESC2
                 MOVE SPACES                     TO WRK-SW-USCITA
                 MOVE SQLCA                      TO G01-SQLCA.
       Z0059-END.
           EXIT.
CTR   *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-ID020                           **
      *****************************************************************
       Z0060-UPDATE-ID020.
       Z0060-INIZIO-01.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_ID020 = :CT1-ID020
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_ID020 = :CT1-ID020 - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X1
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CT1-ID020 GIVING   WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0060-UPDATE-ID020'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-ID020 EQUAL A16-MAXCT
                 PERFORM Z0011-READ-CT1       THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-ID020 = CT1-ID020 + 1
                    GO              TO Z0060-INIZIO-01
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-ID020          EQUAL (A16-MINCT + 1)
E00419              PERFORM Z0060U-D-UPD THRU Z0060U-D-UPD
E00419           ELSE
                    COMPUTE CT1-ID020 = CT1-ID020 + 1
                    GO                 TO Z0060-INIZIO-01.
       Z0060-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0060U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_ID020 = :CT1-ID020
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X6
              MOVE CT1-PRFIL    TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CT1-ID020 GIVING
                            WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0060-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0060U-D-END.
           EXIT.
      *****************************************************************
PAOLO **   AGGIORNAMENTO TABELLA CTR-PRLDI                           **
      *****************************************************************
L00054 Z0061-UPDATE-PRLDI.
       Z0061-INIZIO.
           EXEC SQL
             UPDATE NPCTRT
                SET CTR_PRLDI = :CTR-PRLDI
              WHERE CTR_ABIUT = :CTR-ABIUT
                AND CTR_PRFIL = :CTR-PRFIL
                AND CTR_PRLDI = :CTR-PRLDI - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CTR-ABIUT                  TO WKS-ABIUT-X6
              MOVE CTR-PRFIL                  TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CTR-PRLDI GIVING   WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCTRT'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0061-UPDATE-PRLDI'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CTR-PRLDI  EQUAL G20-PRLDI-MXB
                 PERFORM Z0010-READ-CTR THRU Z0010-END
                 IF CONTINUA
                    COMPUTE CTR-PRLDI = CTR-PRLDI + 1
                    GO TO Z0061-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CTR-PRLDI          EQUAL (G20-PRLDI-MNT + 1)
E00419              PERFORM Z0061U-D-UPD THRU Z0061U-D-UPD
E00419           ELSE
                    COMPUTE CTR-PRLDI = CTR-PRLDI + 1
                    GO TO Z0061-INIZIO.
L00054 Z0061-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0061U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_PRLDI = :CTR-PRLDI
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL  = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X6
              MOVE CTR-PRFIL    TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CTR-PRLDI GIVING
                            WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0061-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.
E0041  Z0061U-D-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO TABELLA CT1-PRLDI                           **
      *****************************************************************
L00054 Z0062-UPDATE-PRLDI.
       Z0062-INIZIO.
           EXEC SQL
             UPDATE NPCT1T
                SET CT1_PRLDI = :CT1-PRLDI
              WHERE CT1_ABIUT = :CT1-ABIUT
                AND CT1_PRFIL = :CT1-PRFIL
                AND CT1_PRLDI = :CT1-PRLDI - 1
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO AND
              SQLCODE NOT EQUAL CENTO
              MOVE CT1-ABIUT                  TO WKS-ABIUT-X6
              MOVE CT1-PRFIL                  TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CT1-PRLDI GIVING   WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6              TO G01-DESC5
              MOVE 'KO'                       TO X09-RCODE
              MOVE 'S'                        TO G01-TPERR
              MOVE SQLCODE                    TO G01-PIC3
              MOVE 'NPCT1T'                   TO G01-ARCHI
              MOVE  WRK-PGMNM                 TO G01-ROUNM
              MOVE 'Z0062-UPDATE-PRLDI'       TO G01-LABEL
              MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
              MOVE SQLCA                      TO G01-SQLCA
              MOVE SPACES                     TO WRK-SW-USCITA.
           IF SQLCODE EQUAL CENTO
              IF CT1-PRLDI EQUAL G20-PRLDI-MXT
                 PERFORM Z0011-READ-CT1 THRU Z0011-END
                 IF CONTINUA
                    COMPUTE CT1-PRLDI = CT1-PRLDI + 1
                    GO TO Z0062-INIZIO
                 ELSE
                    NEXT SENTENCE
              ELSE
E00419           IF CT1-PRLDI          EQUAL (G20-PRLDI-MNT + 1)
E00419              PERFORM Z0062U-D-UPD THRU Z0062U-D-UPD
E00419           ELSE
                    COMPUTE CT1-PRLDI = CT1-PRLDI + 1
                    GO TO Z0062-INIZIO.
L00054 Z0062-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0062U-D-UPD.
           EXEC SQL
            UPDATE NPCT1T
             SET CT1_PRLDI = :CT1-PRLDI
            WHERE CT1_ABIUT = :CT1-ABIUT
             AND CT1_PRFIL = :CT1-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CT1-ABIUT    TO WKS-ABIUT-X6
              MOVE CT1-PRFIL    TO WKS-PRFIL-X6
              SUBTRACT 1 FROM CT1-PRLDI GIVING
                            WKS-CAMPONR-X6
              MOVE WKS-CHIAVE-X6 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCT1T'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0062-D-UPDATE-CT1' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0062U-D-END.
           EXIT.
      *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD01                           **
CTR   *****************************************************************
CTR    Z0101-UPDATE-CTR01.
CTR    Z0101-INIZIO-01.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD01 = :CTR-NRD01
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD01 = :CTR-NRD01 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD01 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0101-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD01 EQUAL A56-MAX01
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD01 = CTR-NRD01 + 1
CTR                 GO              TO Z0101-INIZIO-01
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD01          EQUAL (A56-MIN01  + 1)
E00419              PERFORM Z0101U-D-UPD THRU Z0101U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD01 = CTR-NRD01 + 1
                    GO                 TO Z0101-INIZIO-01.
CTR   *
CTR    Z0101-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0101U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD01 = :CTR-NRD01
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD01 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0101-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0101U-D-END.
CTR        EXIT.
CTR   *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD02                           **
CTR   *****************************************************************
CTR    Z0102-UPDATE-CTR02.
CTR    Z0102-INIZIO-02.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD02 = :CTR-NRD02
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD02 = :CTR-NRD02 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD02 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0102-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD02 EQUAL A56-MAX02
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD02 = CTR-NRD02 + 1
CTR                 GO              TO Z0102-INIZIO-02
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD02          EQUAL (A56-MIN02 + 1)
E00419              PERFORM Z0102U-D-UPD THRU Z0102U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD02 = CTR-NRD02 + 1
                    GO                 TO Z0102-INIZIO-02.
CTR   *
CTR    Z0102-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0102U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD02 = :CTR-NRD02
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD02 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0102-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0102U-D-END.
CTR        EXIT.
CTR   *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD04                           **
CTR   *****************************************************************
CTR    Z0104-UPDATE-CTR04.
CTR    Z0104-INIZIO-04.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD04 = :CTR-NRD04
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD04 = :CTR-NRD04 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD04 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0104-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD04 EQUAL A56-MAX04
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD04 = CTR-NRD04 + 1
CTR                 GO              TO Z0104-INIZIO-04
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD04          EQUAL (A56-MIN04 + 1)
E00419              PERFORM Z0104U-D-UPD THRU Z0104U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD04 = CTR-NRD04 + 1
                    GO                 TO Z0104-INIZIO-04.
CTR   *
CTR    Z0104-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0104U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD04 = :CTR-NRD04
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD04 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0104-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0104U-D-END.
CTR        EXIT.
CTR   *****************************************************************
MO1006**   UPDATE TABELLA CTR PER PIU' DI UN'UNITA (MAV ON LINE)     **
MO1006*****************************************************************
MO1006 ZB104-UPDATE-CTR04.
MO1006     EXEC SQL
MO1006       UPDATE NPCTRT
MO1006          SET CTR_NRD04 = :CTR-NRD04
MO1006        WHERE CTR_ABIUT = :CTR-ABIUT
MO1006          AND CTR_PRFIL = :CTR-PRFIL
MO1006          AND CTR_NRD04 = :CTR-NRD04 - :X09-QUANT
MO1006     END-EXEC.
MO1006*
DEBUG *    DISPLAY '<NPX09P> UPDATE CTR04 SQLCODE:'SQLCODE
DEBUG *    DISPLAY '<NPX09P> SQLCA:'SQLCA
DEBUG *    DISPLAY '<NPX09P> CTR-ABIUT:'CTR-ABIUT
DEBUG *    DISPLAY '<NPX09P> CTR-PRFIL:'CTR-PRFIL
DEBUG *    DISPLAY '<NPX09P> CTR-NRD04:'CTR-NRD04
DEBUG *    DISPLAY '<NPX09P> X09-QUANT:'X09-QUANT

MO1006     IF SQLCODE NOT EQUAL ZERO AND
MO1006        SQLCODE NOT EQUAL CENTO
MO1006        DISPLAY '<NPX09P> **************************************'
MO1006        DISPLAY '<NPX09P>  ERRORE UPDATE TABELLA NPCTRT '
MO1006        DISPLAY '<NPX09P>  PARAGRAFO  ZB104-UPDATE-CTR04'
MO1006        DISPLAY '<NPX09P>  SQLCODE:'SQLCODE
MO1006        DISPLAY '<NPX09P>  SQLCA  :'SQLCA
MO1006        DISPLAY '<NPX09P>  CTR-ABIUT:'CTR-ABIUT
MO1006        DISPLAY '<NPX09P>  CTR-PRFIL:'CTR-PRFIL
MO1006        DISPLAY '<NPX09P>  CTR-NRD04:'CTR-NRD04
MO1006        DISPLAY '<NPX09P>  X09-QUANT:'X09-QUANT
MO1006        DISPLAY '<NPX09P> **************************************'
MO1006        MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
MO1006        MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
MO1006        SUBTRACT X09-QUANT FROM CTR-NRD04 GIVING   WKS-CAMPONR-X1
MO1006        MOVE WKS-CHIAVE-X1              TO G01-DESC5
MO1006        MOVE 'KO'                       TO X09-RCODE
MO1006        MOVE 'S'                        TO G01-TPERR
MO1006        MOVE SQLCODE                    TO G01-PIC3
MO1006        MOVE 'NPCTRT'                   TO G01-ARCHI
MO1006        MOVE  WRK-PGMNM                 TO G01-ROUNM
MO1006        MOVE 'ZB104-UPDATE-CTR'         TO G01-LABEL
MO1006        MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
MO1006        MOVE SQLCA                      TO G01-SQLCA
MO1006        MOVE SPACES                     TO WRK-SW-USCITA
MO1006     END-IF
MO1006*
MO1006     IF SQLCODE EQUAL CENTO
DEBUG *       DISPLAY 'ZB104-UPDATE-CTR04 SQLCODE = +100'
MO1006        IF CTR-NRD04 EQUAL A56-MAX04
DEBUG *          DISPLAY 'CTR-NRD04 = A56-MAX04:'A56-MAX04
DEBUG *          DISPLAY '  **  LEGGO CTR ***'
MO1006           PERFORM Z0010-READ-CTR       THRU Z0010-END
MO1006           IF CONTINUA
MO1006              COMPUTE CTR-NRD04 = CTR-NRD04 + X09-QUANT
DEBUG *             DISPLAY 'CTR-NRD04:'CTR-NRD04
MO1006              GO              TO ZB104-UPDATE-CTR04
MO1006           ELSE
MO1006              NEXT SENTENCE
MO1006        ELSE
MO1006           IF CTR-NRD04   EQUAL (A56-MIN04 + X09-QUANT)
DEBUG *             DISPLAY 'CTR-NRD04 = A56-MIN04 + X09-QUANT:'
DEBUG *                      CTR-NRD04
DEBUG *             DISPLAY 'ESEGUO  Z0104U-D-UPD'
MO1006              PERFORM Z0104U-D-UPD THRU Z0104U-D-UPD
MO1006           ELSE
MO1006              COMPUTE CTR-NRD04 = CTR-NRD04 + X09-QUANT
DEBUG *             DISPLAY 'CTR-NRD04:'CTR-NRD04
MO1006              GO                 TO ZB104-UPDATE-CTR04.
MO1006*
MO1006 ZB104-END.
MO1006     EXIT.
CTR   *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD05                           **
CTR   *****************************************************************
CTR    Z0105-UPDATE-CTR05.
CTR    Z0105-INIZIO-05.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD05 = :CTR-NRD05
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD05 = :CTR-NRD05 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD05 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0105-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD05 EQUAL A56-MAX05
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD05 = CTR-NRD05 + 1
CTR                 GO              TO Z0105-INIZIO-05
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD05          EQUAL (A56-MIN05 + 1)
E00419              PERFORM Z0105U-D-UPD THRU Z0105U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD05 = CTR-NRD05 + 1
                    GO                 TO Z0105-INIZIO-05.
CTR   *
CTR    Z0105-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0105U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD05 = :CTR-NRD05
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD05 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0105-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0105U-D-END.
CTR        EXIT.
CTR   *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD07                           **
CTR   *****************************************************************
CTR    Z0107-UPDATE-CTR07.
CTR    Z0107-INIZIO-07.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD07 = :CTR-NRD07
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD07 = :CTR-NRD07 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD07 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0107-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD07 EQUAL A56-MAX07
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD07 = CTR-NRD07 + 1
CTR                 GO              TO Z0107-INIZIO-07
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD07          EQUAL (A56-MIN07 + 1)
E00419              PERFORM Z0107U-D-UPD THRU Z0107U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD07 = CTR-NRD07 + 1
                    GO                 TO Z0107-INIZIO-07.
CTR   *
CTR    Z0107-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0107U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD07 = :CTR-NRD07
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD07 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0107-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0107U-D-END.
CTR        EXIT.
CTR   *****************************************************************
CTR   **   AGGIORNAMENTO TABELLA CTR-NRD10                           **
CTR   *****************************************************************
CTR    Z0110-UPDATE-CTR10.
CTR    Z0110-INIZIO-10.
CTR   *
CTR        EXEC SQL
CTR          UPDATE NPCTRT
CTR             SET CTR_NRD10 = :CTR-NRD10
CTR           WHERE CTR_ABIUT = :CTR-ABIUT
CTR             AND CTR_PRFIL = :CTR-PRFIL
CTR             AND CTR_NRD10 = :CTR-NRD10 - 1
CTR        END-EXEC.
CTR   *
CTR        IF SQLCODE NOT EQUAL ZERO AND
CTR           SQLCODE NOT EQUAL CENTO
CTR           MOVE CTR-ABIUT                  TO WKS-ABIUT-X1
CTR           MOVE CTR-PRFIL                  TO WKS-PRFIL-X1
CTR           SUBTRACT 1 FROM CTR-NRD10 GIVING   WKS-CAMPONR-X1
CTR           MOVE WKS-CHIAVE-X1              TO G01-DESC5
CTR           MOVE 'KO'                       TO X09-RCODE
CTR           MOVE 'S'                        TO G01-TPERR
CTR           MOVE SQLCODE                    TO G01-PIC3
CTR           MOVE 'NPCTRT'                   TO G01-ARCHI
CTR           MOVE  WRK-PGMNM                 TO G01-ROUNM
CTR           MOVE 'Z0110-UPDATE-CTR'         TO G01-LABEL
CTR           MOVE 'UPDATE TABELLA CONTATORI' TO G01-DESC1
CTR           MOVE SQLCA                      TO G01-SQLCA
CTR           MOVE SPACES                     TO WRK-SW-USCITA.
CTR   *
CTR        IF SQLCODE EQUAL CENTO
CTR           IF CTR-NRD10 EQUAL A56-MAX10
CTR              PERFORM Z0010-READ-CTR       THRU Z0010-END
CTR              IF CONTINUA
CTR                 COMPUTE CTR-NRD10 = CTR-NRD10 + 1
CTR                 GO              TO Z0110-INIZIO-10
CTR              ELSE
CTR                 NEXT SENTENCE
CTR           ELSE
E00419           IF CTR-NRD10          EQUAL (A56-MIN10 + 1)
E00419              PERFORM Z0110U-D-UPD THRU Z0110U-D-UPD
E00419           ELSE
                    COMPUTE CTR-NRD10 = CTR-NRD10 + 1
                    GO                 TO Z0110-INIZIO-10.
CTR   *
CTR    Z0110-END.
           EXIT.
      *****************************************************************
      **   AGGIORNAMENTO NEL CASO SI SIA RAGGIUNTO IL MASSIMO VALORE **
      *****************************************************************
E00419 Z0110U-D-UPD.
           EXEC SQL
            UPDATE NPCTRT
             SET CTR_NRD10 = :CTR-NRD10
            WHERE CTR_ABIUT = :CTR-ABIUT
             AND CTR_PRFIL = :CTR-PRFIL
           END-EXEC.
           IF SQLCODE NOT EQUAL ZERO
              MOVE CTR-ABIUT    TO WKS-ABIUT-X1
              MOVE CTR-PRFIL    TO WKS-PRFIL-X1
              SUBTRACT 1 FROM CTR-NRD10 GIVING
                            WKS-CAMPONR-X1
              MOVE WKS-CHIAVE-X1 TO G01-DESC5
              MOVE 'KO'          TO X09-RCODE
              MOVE 'S'           TO G01-TPERR
              MOVE SQLCODE       TO G01-PIC3
              MOVE 'NPCTRT'      TO G01-ARCHI
              MOVE WRK-PGMNM     TO G01-ROUNM
              MOVE 'Z0110-D-UPDATE-CTR' TO G01-DESC1
              MOVE SQLCA         TO G01-SQLCA
              MOVE SPACE         TO WRK-SW-USCITA.

E00419 Z0110U-D-END.
CTR        EXIT.
      *****************************************************************
      **   GENERAZIONE NUOVO NOPER                                   **
      *****************************************************************
SEQUEN Z0120-NEXT-VALUE.
SEQUEN*
SEQUEN     EXEC SQL
SEQUEN          SET :CTR-UNOPE = NEXT VALUE
SEQUEN          FOR AGNP.CTR_UNOPE
SEQUEN     END-EXEC.
SEQUEN*
SEQUEN     IF SQLCODE NOT EQUAL ZERO
SEQUEN        MOVE SPACES                    TO WRK-SW-USCITA
SEQUEN        MOVE 'KO'                      TO X09-RCODE OF NPX09RC
SEQUEN        MOVE 'S'                       TO G01-TPERR
SEQUEN        MOVE WRK-PGMNM                 TO G01-ROUNM
SEQUEN        MOVE 'Z0120-NEXT-VALUE'        TO G01-LABEL
SEQUEN        MOVE 'CTR_UNOPE'               TO G01-ARCHI
SEQUEN        MOVE SQLCODE                   TO G01-PIC3
SEQUEN        MOVE 'ERRORE LETTURA SEQUENCE' TO G01-DESC1
SEQUEN        MOVE SQLCA                     TO G01-SQLCA.
SEQUEN*
SEQUEN     IF CONTINUA
SEQUEN        MOVE NPCTRT                    TO X09-NPCTRT.
SEQUEN*
SEQUEN Z0120-END.
SEQUEN     EXIT.
      *****************************************************************
      **   GENERAZIONE NUOVO NRD01                                   **
      *****************************************************************
SEQD01 Z0130-NEXT-NRD01.
SEQD01*
SEQD01     EXEC SQL
SEQD01          SET :CTR-NRD01 = NEXT VALUE
SEQD01          FOR AGNP.CTR_NRD01
SEQD01     END-EXEC.
SEQD01*
SEQD01     IF SQLCODE NOT EQUAL ZERO
SEQD01        MOVE SPACES                    TO WRK-SW-USCITA
SEQD01        MOVE 'KO'                      TO X09-RCODE OF NPX09RC
SEQD01        MOVE 'S'                       TO G01-TPERR
SEQD01        MOVE WRK-PGMNM                 TO G01-ROUNM
SEQD01        MOVE 'Z0130-NEXT-NRD01'        TO G01-LABEL
SEQD01        MOVE 'CTR_NRD01'               TO G01-ARCHI
SEQD01        MOVE SQLCODE                   TO G01-PIC3
SEQD01        MOVE 'ERRORE LETTURA SEQUENCE' TO G01-DESC1
SEQD01        MOVE SQLCA                     TO G01-SQLCA.
SEQD01*
SEQD01     IF CONTINUA
SEQD01        MOVE NPCTRT                    TO X09-NPCTRT.
SEQD01*
SEQD01 Z0130-END.
SEQD01     EXIT.
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
      *----------------------------------------------------------------
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPX09R
EXPAND*--------------------------------------------------------
