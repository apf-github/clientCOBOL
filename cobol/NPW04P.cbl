       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW04P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW04R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW04R
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
      *  NPW04R    RECUPERO DATA SISTEMA                              *
      *---------------------------------------------------------------*
XMTA01*  22/08/01 - MODIFICA PER GESTIRE LA SOMMA DEI GIORNI DELLA    *
      *             TABELLA A10 COME LAVORATIVI                       *
      *---------------------------------------------------------------*
           EJECT
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSK-HHMM.
           05 WSK-HH                   PIC XX.
           05 WSK-MM                   PIC XX.
       01  HHMMSS9-WK                  PIC 999999.
       01  WSK-DATA                    PIC S9(15) COMP-3.
       01  WSK-DATX.
           05 WSK-GGX                  PIC X(002).
           05 FILLER                   PIC X(001).
           05 WSK-MMX                  PIC X(002).
           05 FILLER                   PIC X(001).
           05 WSK-AAX                  PIC X(002).
       01  WK-DATA.
           05 WK-DATA-AA               PIC 9(002).
           05 WK-DATA-MM               PIC 9(002).
           05 WK-DATA-GG               PIC 9(002).
       01  WK-TIME.
           05 WK-ORA                   PIC 9(002).
           05 WK-MIN                   PIC 9(002).
           05 WK-SEC                   PIC 9(002).
       01  WK-PIC2                     PIC 9(002).
       01  WK-PIC4                     PIC 9(004).
       01  WK-PIC6                     PIC 9(006).
       01  WK-PIC8                     PIC 9(008).
       COPY  NPW01RC.
       COPY  NPW04RC.
       COPY  NPA07TC.
       COPY  NPA10TC.
       COPY  NPG01RC.
       COPY  NPG03RC.
       COPY  NPW08RC.
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
           MOVE WRK-NP0500                      TO NPW04RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           MOVE SPACES                TO W04-RCODE.
           PERFORM A0010-ACQUISISCI THRU A0010-END.
           PERFORM A0020-LEGGI-A07  THRU A0020-END.
           IF W01-RCODE = SPACES
              PERFORM A0030-LEGGI-A10  THRU A0030-END.
           MOVE NPW04RC                         TO WRK-NP0500.
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
      **   ACQUISIZIONE DATA SISTEMA                                 **
      *****************************************************************
       A0010-ACQUISISCI.
           MOVE '/'       TO W04-SEP1.
           MOVE '/'       TO W04-SEP2.
           MOVE ':'       TO W04-SEP3.
EXPAND*    EXEC SQL INCLUDE NPJ01RC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPJ01RC
EXPAND*--------------------------------------------------------
           ACCEPT WK-DATA FROM DATE.
           ACCEPT WK-TIME FROM TIME.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPJ01RC
EXPAND*--------------------------------------------------------
           IF WK-DATA-AA > 60
              MOVE 19                  TO W04-DATA-SEC
              MOVE WK-DATA-AA          TO W04-DATA-AA
              MOVE WK-DATA-MM          TO W04-DATA-MM
              MOVE WK-DATA-GG          TO W04-DATA-GG
              MOVE W04-DATA            TO W04-DTLAV
           ELSE
              MOVE 20                  TO W04-DATA-SEC
              MOVE WK-DATA-AA          TO W04-DATA-AA
              MOVE WK-DATA-MM          TO W04-DATA-MM
              MOVE WK-DATA-GG          TO W04-DATA-GG
              MOVE W04-DATA            TO W04-DTLAV.
           MOVE WK-TIME                TO W04-ORA.
           MOVE W04-DATA-AAAA          TO W04-AAAA-ST.
           MOVE W04-DATA-MM            TO W04-MM-ST.
           MOVE W04-DATA-GG            TO W04-GG-ST.
           MOVE W04-ORA-HH             TO W04-HH-ST.
           MOVE W04-ORA-MM             TO W04-MI-ST.
       A0010-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA DATA SISTEMA                              **
      *****************************************************************
       A0020-LEGGI-A07.
           MOVE SPACES      TO NPW01RC.
           MOVE SPACES      TO A07-KEY.
           MOVE 'A07'       TO A07-CODIT.
           MOVE W04-ABIUT   TO A07-ABIUT.
           MOVE WRK-SERVI   TO A07-SERVI.
           MOVE A07-KEY     TO W01-NPDATT.
           MOVE 'RE'        TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE = SPACES
              MOVE W01-NPDATT     TO NPA07TC
              MOVE A07-DATAS      TO W04-DATA
              MOVE A07-DATAS      TO W04-DTLAV
              MOVE W04-DATA-AAAA  TO W04-AAAA-ST
              MOVE W04-DATA-MM    TO W04-MM-ST
              MOVE W04-DATA-GG    TO W04-GG-ST
           ELSE
PERINF*       IF W01-RCODE = 'GE'
PERINF*          MOVE SPACES      TO NPG01RC
PERINF*          MOVE SPACES      TO W01-RCODE
PERINF*       ELSE
                 IF W01-RCODE NOT EQUAL SPACES
                    MOVE 'KO'     TO W04-RCODE.
       A0020-END.
           EXIT.
      *****************************************************************
      **   LETTURA TABELLA A10                                       **
      *****************************************************************
       A0030-LEGGI-A10.
           MOVE SPACES      TO NPW01RC.
           MOVE SPACES      TO A10-KEY.
           MOVE 'A10'       TO A10-CODIT.
           MOVE W04-ABIUT   TO A10-ABIUT.
           MOVE W04-PGMNM   TO A10-PGMNM.
           MOVE A10-KEY     TO W01-NPDATT.
           MOVE 'RE'        TO W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE = 'GE'
              MOVE SPACES TO NPG01RC
              GO TO A0030-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE 'KO'      TO W04-RCODE
              GO TO A0030-END.
           MOVE W01-NPDATT     TO NPA10TC.
           MOVE W04-ORA-HH     TO WSK-HH.
           MOVE W04-ORA-MM     TO WSK-MM.
           MOVE WSK-HHMM       TO WK-PIC4.
PERINF*    IF WK-PIC4 LESS A10-ORLAV
PERINF*       IF A10-SEGN1 EQUAL '+'
PERINF*          MOVE A10-GGLA1           TO WK-PIC2
PERINF*          PERFORM A0040-SOMMA      THRU A0040-END
PERINF*       ELSE
PERINF*       IF A10-SEGN1 EQUAL '-'
PERINF*          MOVE A10-GGLA1           TO WK-PIC2
PERINF*          PERFORM A0050-SOTTRAI    THRU A0050-END.
PERINF*    IF WK-PIC4 GREATER A10-ORLAV
              IF A10-SEGN2 EQUAL '+'
                 MOVE A10-GGLA2           TO WK-PIC2
                 PERFORM A0040-SOMMA      THRU A0040-END
              ELSE
              IF A10-SEGN2 EQUAL '-'
                 MOVE A10-GGLA2           TO WK-PIC2
                 PERFORM A0050-SOTTRAI    THRU A0050-END.
       A0030-END.
           EXIT.
      *****************************************************************
      **   INCREMENTO DATA SISTEMA PER OTTENERE DATA LAVORAZIONE     **
      *****************************************************************
       A0040-SOMMA.
           MOVE SPACES          TO NPW08RC.
           MOVE ZERO            TO W08-RCODE.
XMTA01**** MOVE '16'            TO W08-RTIPO.
XMTA01     MOVE '18'            TO W08-RTIPO.
           MOVE W04-DATA        TO W08-RDATA.
           MOVE WK-PIC2         TO W08-RGGG.
           PERFORM W0008-RICHIAMO THRU W0008-END.
           IF W08-RCODE EQUAL ZERO
              MOVE W08-RDATA              TO W04-DTLAV.
       A0040-END.
           EXIT.
      *****************************************************************
      **   SOTTRAE DA DATA SISTEMA PER OTTENERE DATA LAVORAZIONE     **
      *****************************************************************
       A0050-SOTTRAI.
           MOVE SPACES          TO NPW08RC.
           MOVE ZERO            TO W08-RCODE.
XMTA01**** MOVE '17'            TO W08-RTIPO.
XMTA01     MOVE '19'            TO W08-RTIPO.
           MOVE W04-DATA        TO W08-RDATA.
           MOVE WK-PIC2         TO W08-RGGG.
           PERFORM W0008-RICHIAMO THRU W0008-END.
           IF W08-RCODE EQUAL ZERO
              MOVE W08-RDATA              TO W04-DTLAV.
       A0050-END.
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
EXPAND*    EXEC SQL INCLUDE NPW08RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW08RR
EXPAND*--------------------------------------------------------
       W0008-RICHIAMO.
           MOVE 'NPW08P'            TO WRK-MODULO.
           MOVE NPW08RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPW08RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0008-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW08RR
EXPAND*--------------------------------------------------------
      *----------------------------------------------------------------
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW04R
EXPAND*--------------------------------------------------------
