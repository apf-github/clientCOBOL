       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW05P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW05R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW05R
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
      *  NPW05R    RECUPERO DATA CONTABILE                            *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSK-PIC8          PIC 9(008) VALUE ZERO.
       01  WSK-DTDB2.
           05 WSK-GG             PIC X(002).
           05 FILLER             PIC X(001) VALUE '.'.
           05 WSK-MM             PIC X(002).
           05 FILLER             PIC X(001) VALUE '.'.
           05 WSK-AAAA           PIC X(004).
       COPY  NPW05RC.
       COPY  NPW08RC.
       COPY  NPG01RC.
       COPY  NPG03RC.
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
           MOVE WRK-NP0500                      TO NPW05RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING THRU A0000-END.
           PERFORM A0020-ELABORA       THRU A0020-END.
           MOVE NPW05RC                         TO WRK-NP0500.
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
      ********************************************************
      *    OPERAZIONI INIZIALI                               *
      ********************************************************
       A0000-HOUSE-KEEPING.
           MOVE SPACES                 TO W05-RCODE.
       A0000-END.
           EXIT.
      *****************************************************************
      **   ACQUISIZIONE DATA SISTEMA                                 **
      **   RICAVO DATA CONTABILE                                     **
      *****************************************************************
       A0020-ELABORA.
970218     MOVE SPACES          TO NPW08RC.
970218     MOVE ZERO            TO W08-RCODE.
970218     MOVE '12'            TO W08-RTIPO.
970218     MOVE W05-DTCON       TO W08-RDATA.
970218*
970218     PERFORM W0008-RICHIAMO THRU W0008-END
970218*
970218     IF W08-RCODE NOT EQUAL ZERO
970218        MOVE SPACES       TO NPW08RC
970218        MOVE ZERO         TO W08-RCODE
970218        MOVE '16'         TO W08-RTIPO
970218        MOVE W05-DTCON    TO W08-RDATA
970218        MOVE 1            TO W08-RGGG
970218*
970218        PERFORM W0008-RICHIAMO THRU W0008-END
970218*
970218        IF W08-RCODE EQUAL ZERO
970218           MOVE W08-RDATA TO W05-DTCON.
           PERFORM A0030-IMPOSTA THRU A0030-END.
       A0020-END.
           EXIT.
      *****************************************************************
      **   ACQUISIZIONE DATA SISTEMA                                 **
      **   RICAVO DATA CONTABILE                                     **
      *****************************************************************
       A0030-IMPOSTA.
           MOVE W05-DATA-AAAA    TO W05-DATA-AAAA1.
           MOVE W05-DATA-MM      TO W05-DATA-MM1.
           MOVE W05-DATA-GG      TO W05-DATA-GG1.
           MOVE W05-DTCON        TO WSK-PIC8.
           MOVE WSK-PIC8         TO W05-DTCO2.
           MOVE W05-DATA-AAAA    TO WSK-AAAA.
           MOVE W05-DATA-MM      TO WSK-MM.
           MOVE W05-DATA-GG      TO WSK-GG.
           MOVE WSK-DTDB2        TO W05-DTDB2.
       A0030-END.
           EXIT.
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
EXPAND* FINE ESPLOSIONE COPY NPW05R
EXPAND*--------------------------------------------------------
