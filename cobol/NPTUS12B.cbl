       IDENTIFICATION DIVISION.
       PROGRAM-ID. NPTUS12B.
      *---------------------------------------------------------------*
      *                                                               *
      *   *****   ******    ***                                       *
      *  **   **  **   **  ****                                       *
      *  *******  ******   ****                                       *
      *  **   **  **         **                                       *
      *  **   **  **         **   I N F O R M A T I C A   S . R . L . *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      * IMPOSTAZIONE FLAG ELABORAZIONE PUMA TRIMESTRALE               *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
DEBUG *SOURCE-COMPUTER. IBM-3090 WITH DEBUGGING MODE.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SKPARAM ASSIGN        TO UT-S-SKPARAM.
       DATA DIVISION.
       FILE SECTION.
       FD  SKPARAM
           BLOCK CONTAINS 0 RECORDS
           LABEL RECORD IS STANDARD.
       01  REC-SKPARAM                  PIC X(080).
       WORKING-STORAGE SECTION.
       01  DUMPX                 PIC X(001) VALUE 'X'.
       01  DUMP REDEFINES DUMPX  PIC 9(001) COMP-3.
       01  IIU                   PIC S9(03) COMP-3 VALUE ZERO.
      *                                                                 01150000
       01  WKS-ORAOP                    PIC  9(007).                    01050000
       01  FILLER REDEFINES WKS-ORAOP.                                  01060000
           03  FILLER                   PIC  X(001).                    01070000
           03  WKS-HHH                  PIC  X(002).                    01080000
           03  WKS-MIN                  PIC  X(002).                    01090000
           03  FILLER                   PIC  X(002).                    01100000
       01  WKS-ORAOP-STA.                                               01110000
           03  WKS-HH-STA               PIC  X(002).                    01120000
           03  FILLER                   PIC  X(001)  VALUE ':'.         01130000
           03  WKS-MIN-STA              PIC  X(002).                    01140000
      *                                                                 01150000
       01  WKS-NPT37TC.
           03 WKS-T37-KEY.
              05 WKS-T37-CODIT            PIC X(003).
              05 WKS-T37-ABIUT            PIC X(005).
              05 WKS-T37-TPROC            PIC X(012).
              05 WKS-T37-FUNZI            PIC X(010).
              05 FILLER                   PIC X(005).
           03 WKS-T37-DATI                PIC X(200).
           03 WKS-T37-CDF02     REDEFINES WKS-T37-DATI.
      * FLAG ELABORAZIONE PUMA TRIMESTRALE SI/NO
              05 WKS-T37-FEPTR            PIC X(002).
      * FLAG ELABORAZIONE VARIAZIONE SOGLIE USURA SI/NO
              05 WKS-T37-FEVSO            PIC X(002).
      * DATA ELABORAZIONE VARIAZIONE SOGLIE USURA(AAAAMMGG)
              05 WKS-T37-DTVSO            PIC 9(008).
              05 FILLER                   PIC X(188).
       01  WKS-SKPARAM.
           03 WKS-NPATAI                PIC 9(08) VALUE ZERO.
           03 WKS-NPATAF                PIC 9(08) VALUE ZERO.
           03 WKS-AELAB                 PIC X(01) VALUE SPACE.
           03 WKS-ATIPUMA               PIC X(02) VALUE SPACES.
           03 WKS-ACODABI               PIC X(05) VALUE SPACES.
           03 WKS-MESI                  PIC X(02) VALUE SPACES.
           03 WKS-AFILLER               PIC X(54) VALUE SPACES.
      *------------------------------------------------------*
      *          C A M P I   D I   C O M O D O               *
      *------------------------------------------------------*
           COPY NPG01RC.
           COPY NPG03RC.
      *------------------------------------------------------*
      *                C O P Y   R O U T I N E               *
      *------------------------------------------------------*
           COPY NPW01RC.
           COPY NPW08RC.
           COPY NPW33RC.
           COPY NPX50RC.
      *------------------------------------------------------*
      *                 C O N T A T O R I                    *
      *------------------------------------------------------*
       01  CONTATORI.
           05 CNT-REASKE             PIC 9(007) COMP-3 VALUE ZERO.
           05 CNT-REAT37             PIC 9(007) COMP-3 VALUE ZERO.
           05 CNT-UPDT37             PIC 9(007) COMP-3 VALUE ZERO.
           05 CNT-ERROR              PIC 9(007) COMP-3 VALUE ZERO.
      *------------------------------------------------------*
      *              F L A G S    E    S W I T C H E S       *
      *------------------------------------------------------*
       01  FLAG-ERR              PIC 99 VALUE 0.
       01  SW-FINE-SKPARAM       PIC  9 VALUE 0.
           88 FINE-SKPARAM              VALUE 1.
       01  SW-FINE-TRI           PIC  9 VALUE 0.
           88 FINE-TRI                  VALUE 1.
           88 FINE-TRI-NO               VALUE 2.
      *------------------------------------------------------*
      *                S T A T I S T I C A                   *
      *------------------------------------------------------*
           COPY NPSTAMC.
       01  STA-RIGA12.
           05  FILLER             PIC X(008) VALUE SPACES.
           05  FILLER             PIC X(027)
                VALUE 'STATISTICHE DI ELABORAZIONE'.
       01  STA-RIGA13.
           05  FILLER             PIC X(001) VALUE SPACES.
           05  FILLER             PIC X(016)
                VALUE 'FILE DI INPUT  :'.
       01  STA-RIGA14.
           05  FILLER             PIC X(014) VALUE SPACES.
           05  FILLER             PIC X(010) VALUE 'SKPARAM  -'.
           05  FILLER             PIC X(028)
                VALUE 'RECORD LETTI        ......= '.
           05  STA-REASKE         PIC ZZZ.ZZ9 VALUE ZERO.
       01  STA-RIGA15.
           05  FILLER             PIC X(014) VALUE SPACES.
           05  FILLER             PIC X(010) VALUE 'NPT37TC  -'.
           05  FILLER             PIC X(028)
                VALUE 'RECORD LETTI        ......= '.
           05  STA-REAT37         PIC ZZZ.ZZ9 VALUE ZERO.
       01  STA-RIGA16.
           05  FILLER             PIC X(001) VALUE SPACES.
           05  FILLER             PIC X(016)
                VALUE 'FILE DI OUTPUT :'.
       01  STA-RIGA17.
           05  FILLER             PIC X(014) VALUE SPACES.
           05  FILLER             PIC X(010) VALUE 'NPT37TC  -'.
           05  FILLER             PIC X(028)
                VALUE 'RECORD AGGIORNATI     ....= '.
           05  STA-UPDT37         PIC ZZZ.ZZ9 VALUE ZERO.
       01  STA-RIGA30.
           05  FILLER             PIC X(014) VALUE SPACES.
           05  FILLER             PIC X(010) VALUE 'NPERRT -'.
           05  FILLER             PIC X(028)
                VALUE 'ERRORI RISCONTRATI    ....= '.
           05  STA-ERRORE         PIC ZZZ.ZZ9 VALUE ZERO.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
           EXEC SQL INCLUDE SQLCA   END-EXEC.
           EXEC SQL INCLUDE NPDATT  END-EXEC.
      ********************************************************
      *         P R O C E D U R E   D I V I S I O N          *
      ********************************************************
       PROCEDURE DIVISION.
DEBUG *DECLARATIVES.
DEBUG *DBG SECTION.
DEBUG *    USE FOR DEBUGGING ON ALL PROCEDURES.
DEBUG *DBG0.
DEBUG *    DISPLAY ' '
DEBUG *    DISPLAY 'NPTUS12B    '
DEBUG**    DEBUG-LINE ' ' DEBUG-NAME ' ' DEBUG-CONTENTS.
DEBUG *END DECLARATIVES.
       INIZIO-PGM.
           PERFORM A0000-HOUSE-KEEPING      THRU A0000-END.
           PERFORM B0000-ELABORA            THRU B0000-END.
           PERFORM Z0010-SEGNALAZIONI       THRU Z0010-END.
       FINE-PGM.
           STOP RUN.
      **************************************************************
      ** OPERAZIONI INIZIALI                                      **
      **************************************************************
       A0000-HOUSE-KEEPING.
           MOVE 'NPTUS12B'                  TO WRK-PGMNM.
           MOVE SPACES                      TO NPW33RC.
           MOVE '02'                        TO W33-FUNZI.
           MOVE WRK-PGMNM                   TO W33-PGMNM.
           PERFORM W0033-RICHIAMO           THRU W0033-END.
           IF W33-RCODE NOT EQUAL SPACES
              MOVE 1                        TO FLAG-ERR
              GO TO Y0000-ERRORE-PGM.
           MOVE 1                           TO IIU.
           MOVE ZERO                        TO SW-FINE-SKPARAM.
           MOVE ZERO                        TO SW-FINE-TRI.
           INITIALIZE                          CONTATORI.
       A0000-END.
           EXIT.
      **************************************************************
      ** ELABORAZIONE ISTITUTO                                    **
      **************************************************************
       B0000-ELABORA.
           PERFORM B0010-LETTURA-SKPARAM    THRU B0010-END.
           PERFORM B0020-FINE-TRIMESTRE     THRU B0020-END.
           IF FINE-TRI
              PERFORM B0030-LETTURA-T37     THRU B0030-END
              PERFORM B0040-UPDATE-T37      THRU B0040-END.
       B0000-END.
           EXIT.
      ****************************************************************
      ** CONTROLLO DATA PARAMETRO                                   **
      ****************************************************************
       B0010-LETTURA-SKPARAM.
           OPEN INPUT SKPARAM.
           READ SKPARAM                     INTO WKS-SKPARAM
             AT END
            SET FINE-SKPARAM                TO TRUE.
           CLOSE SKPARAM.
           IF FINE-SKPARAM
              DISPLAY 'NPTUS12B SCHEDA PARAMETRO VUOTA'
              GO TO Y0000-ERRORE-PGM.
           ADD 1                            TO CNT-REASKE.
           IF WKS-NPATAF EQUAL ZERO
           OR WKS-NPATAF NOT NUMERIC
              DISPLAY 'NPTUS12B ERRORE SCHEDA PARAMETRO '
              DISPLAY 'DATA FINE NON VALORIZZATA ' WKS-NPATAF
              GO TO Y0000-ERRORE-PGM.
       B0010-END.
           EXIT.
      ****************************************************************
      ** CONTROLLO SE DATA FINE E' DATA DI FINE TRIMESTRE           **
      ****************************************************************
       B0020-FINE-TRIMESTRE.
           INITIALIZE                          NPW08RC.
           MOVE 25                          TO W08-RTIPO.
           MOVE WKS-NPATAF                  TO W08-RDATA.
           MOVE ZERO                        TO W08-RGGG.
           PERFORM W0008-RICHIAMO           THRU W0008-END.
           IF W08-RCODE NOT EQUAL ZERO AND 10
              DISPLAY 'NPTUS12B ERRORE RICHIAMO NPW08P '
              DISPLAY 'W08-RDATA ' W08-RDATA
              DISPLAY 'W08-RCODE ' W08-RCODE
              GO                            TO Y0000-ERRORE-PGM.
           IF W08-RCODE EQUAL ZERO
              SET FINE-TRI                  TO TRUE.
           IF W08-RCODE EQUAL 10
              SET FINE-TRI-NO               TO TRUE.
       B0020-END.
           EXIT.
      **************************************************************
      **  LETTURA TABELLA NPDATT T37                              **
      **************************************************************
       B0030-LETTURA-T37.
           MOVE SPACES                      TO WKS-NPT37TC.
           MOVE 'T37'                       TO WKS-T37-CODIT.
           MOVE W33-ABIUT(IIU)              TO WKS-T37-ABIUT.
           MOVE 'VARIAZTRIM'                TO WKS-T37-TPROC.
           MOVE SPACES                      TO WKS-T37-FUNZI.
           MOVE 'RE'                        TO W01-FUNZI.
           MOVE WKS-NPT37TC                 TO W01-NPDATT.
           PERFORM W0001-RICHIAMO           THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              DISPLAY 'ERRORE LETTURA TABELLA T37 '
              GO TO Y0000-ERRORE-PGM.
           MOVE W01-NPDATT                  TO WKS-NPT37TC.
           ADD 1                            TO CNT-REAT37.
       B0030-END.
           EXIT.
      **************************************************************
      **  AGGIORNAMENTO TABELLA NPDATT T37                        **
      **************************************************************
       B0040-UPDATE-T37.
           MOVE 'SI'                        TO WKS-T37-FEPTR.
           MOVE 'UP'                        TO W01-FUNZI.
           MOVE WKS-NPT37TC                 TO W01-NPDATT.
           PERFORM W0001-RICHIAMO           THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              DISPLAY 'ERRORE UPDATE TABELLA T37 '
              GO TO Y0000-ERRORE-PGM.
           ADD 1                            TO CNT-UPDT37.
       B0040-END.
           EXIT.
      **************************************************************
      ** GESTIONE ERRORE                                          **
      **************************************************************
       Y0000-ERRORE-PGM.
           IF FLAG-ERR EQUAL ZERO                                       04710000
              MOVE 12                         TO FLAG-ERR.
      *                                                                 04730000
           IF IIU EQUAL ZERO                                            04740000
              MOVE SPACES                     TO W33-OUTPUT
              MOVE 1                          TO W33-NRUTE
                                                 IIU
              MOVE ZERO                       TO W33-HORAS(IIU)
              MOVE ZERO                       TO W33-DTLAV(IIU)
              MOVE ZERO                       TO W33-DTCON(IIU)
              MOVE ZERO                       TO W33-NOPER(IIU)
              MOVE ZERO                       TO W33-FRQCM(IIU)
              MOVE ZERO                       TO W33-QTERR(IIU).
      *                                                                 04830000
           IF FLAG-ERR EQUAL 12                                         04840000
              MOVE W33-ABIUT(IIU)             TO G01-ABIUT
              MOVE W33-SERVI(IIU)             TO G01-SERVI
              MOVE W33-NOPER(IIU)             TO G01-NOPER
              MOVE W33-CDOPE(IIU)             TO G01-CDOPE
              MOVE W33-DTLAV(IIU)             TO G01-DTOPE
              MOVE W33-HORAS(IIU)             TO G01-ORAOP
              MOVE W33-FILCE(IIU)             TO G01-CFILI
           ELSE                                                         04920000
              ACCEPT COBOL-TIME               FROM TIME
              ACCEPT COBOL-DATE               FROM DATE
              MOVE COBOL-TIME-6               TO G01-ORAOP
              MOVE COBDATE-MM                 TO WRK-MESE
              MOVE COBDATE-GG                 TO WRK-GIORNO
              IF COBDATE-AA GREATER '90'                                04980000
                 MOVE '19'                    TO WRK-SECOLO
                 MOVE COBDATE-AA              TO WRK-AA
                 MOVE WRK-DATA9               TO G01-DTOPE
              ELSE                                                      05020000
                 MOVE '20'                    TO WRK-SECOLO
                 MOVE COBDATE-AA              TO WRK-AA
                 MOVE WRK-DATA9               TO G01-DTOPE.

           MOVE WRK-PGMNM                     TO G01-CTERM
                                                 G01-CTRAN
                                                 G01-PGMNM.
           MOVE 'S'                           TO G01-TPERR.
      *                                                                 05060000
           MOVE SPACES                      TO NPX50RC.
           MOVE NPG01RC                     TO X50-NPERRT.
           MOVE 'IS'                        TO X50-FUNZI.
           DISPLAY '*** NPTUS12B G01-SQLCA = ' G01-SQLCA.
      *                                                                 05100000
           EXEC SQL ROLLBACK END-EXEC.
      *                                                                 05140000
           IF SQLCODE NOT EQUAL ZERO
              MOVE 'S'                      TO W33-FDUMP(IIU)
              MOVE 'ERRORE ROLLBACK'        TO G01-DESC1
              MOVE 'Y0000-ERRORE'           TO G01-LABEL
              MOVE SQLCODE                  TO G01-PIC3
           ELSE
              PERFORM X0050-RICHIAMO        THRU X0050-END
              IF X50-RCODE EQUAL SPACES
                 EXEC SQL COMMIT END-EXEC.
      *                                                                 05190000
           ADD 1                              TO CNT-ERROR.
           PERFORM Z0010-SEGNALAZIONI       THRU Z0010-END.
      *                                                                 05210000
           MOVE 14                            TO   RETURN-CODE.
           IF W33-FDUMP(IIU) EQUAL 'S'                                  05220000
              ADD  1                          TO   DUMP.
      *                                                                 05210000
           STOP RUN.                                                    05250000
      *                                                                 05260000
       Y0000-END.                                                       05270000
           EXIT.                                                        05280000
      **************************************************************
      ** STAMPA SEGNALAZIONI                                      **
      **************************************************************
       Z0010-SEGNALAZIONI.
           MOVE WRK-PGMNM                   TO   STA-PGMNM.
           MOVE W33-DESCR(IIU)              TO   STA-DESCR.
           MOVE W33-ABIUT(IIU)              TO   STA-ABIUT.
           MOVE W33-DESBA(IIU)              TO   STA-DESBA.
           MOVE W33-TESTA(IIU)              TO   STA-TESTA.
           MOVE W33-DTSTA(IIU)              TO   STA-DATA.
           MOVE W33-HRSTA(IIU)              TO   STA-ORA.
           IF FLAG-ERR EQUAL ZERO
              MOVE 'CORRETTAMENTE'          TO   STA-ESITO
           ELSE
              MOVE 'CON ANOMALIE'           TO   STA-ESITO
              MOVE G01-DTOPE                TO   WRK-DATA9
              MOVE WRK-ANNO                 TO   WRK-AASTA
              MOVE WRK-MESE                 TO   WRK-MMSTA
              MOVE WRK-GIORNO               TO   WRK-GGSTA
              MOVE WRK-DATA-STA             TO   STA-DATA
              MOVE G01-ORAOP                TO   WKS-ORAOP
              MOVE WKS-HHH                  TO   WKS-HH-STA
              MOVE WKS-MIN                  TO   WKS-MIN-STA
              MOVE WKS-ORAOP-STA            TO   STA-ORA
              MOVE G01-ROUNM                TO   STA-ROUNM
              MOVE G01-LABEL                TO   STA-LABEL
              MOVE G01-ARCHI                TO   STA-ARCHI
              MOVE G01-CDERR                TO   STA-CDERR
              MOVE G01-DESC1                TO   STA-DESC1
              MOVE G01-DESC2                TO   STA-DESC2
              MOVE G01-DESC3                TO   STA-DESC3
              MOVE G01-DESC4                TO   STA-DESC4
              MOVE G01-DESC5                TO   STA-DESC5.
           DISPLAY STA-TRATT.
           DISPLAY STA-TRATT.
           DISPLAY SPACES.
           DISPLAY STA-RIGA01.
           DISPLAY SPACES.
           DISPLAY STA-RIGA02.
           DISPLAY SPACES.
           DISPLAY STA-RIGA03.
           DISPLAY SPACES.
           IF FLAG-ERR NOT EQUAL ZERO
              DISPLAY STA-RIGA05
              DISPLAY STA-RIGA06
              DISPLAY STA-RIGA07
              DISPLAY STA-RIGA08
              DISPLAY STA-RIGA09
              DISPLAY STA-RIGA10
              DISPLAY STA-RIGA11.
           DISPLAY STA-TRATT.
           PERFORM Z0020-STAMPA-STATI       THRU Z0020-END.
           DISPLAY SPACES.
           DISPLAY STA-RIGA04.
           DISPLAY SPACES.
           DISPLAY STA-TRATT.
           DISPLAY STA-TRATT.
       Z0010-END.
           EXIT.
      **************************************************************
      ** STAMPA STATISTICA ISTITUTO                               **
      **************************************************************
       Z0020-STAMPA-STATI.
           MOVE CNT-REASKE                  TO STA-REASKE.
           MOVE CNT-REAT37                  TO STA-REAT37.
           MOVE CNT-UPDT37                  TO STA-UPDT37.
           DISPLAY SPACES.
           DISPLAY STA-TRATT.
           DISPLAY STA-RIGA12.
           DISPLAY STA-TRATT.
           DISPLAY SPACES.
           DISPLAY STA-RIGA13.
           DISPLAY STA-RIGA14.
           DISPLAY STA-RIGA15.
           DISPLAY STA-TRATT.
           DISPLAY SPACES.
           DISPLAY STA-RIGA16.
           DISPLAY STA-TRATT.
           DISPLAY STA-RIGA17.
           DISPLAY SPACES.
           DISPLAY STA-RIGA30.
       Z0020-END.
           EXIT.
      *--------------------------------------------------------
      * RICHIAMO ROUTINE NPX50P
      *--------------------------------------------------------
       X0050-RICHIAMO.
           MOVE 'NPX50P'            TO WRK-MODULO.
           MOVE NPX50RC             TO WRK-NP1000.
           MOVE NPG01RC             TO WRK-NPG01-1000.
           CALL WRK-MODULO USING WRK-1650.
           MOVE WRK-NP1000          TO NPX50RC.
           MOVE WRK-NPG01-1000      TO NPG01RC.
       X0050-END.
           EXIT.
      *--------------------------------------------------------
      * RICHIAMO ROUTINE NPW01P
      *--------------------------------------------------------
       W0001-RICHIAMO.
           MOVE 'NPW01P'            TO WRK-MODULO.
           MOVE NPW01RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPW01RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0001-END.
           EXIT.
      *--------------------------------------------------------
      * RICHIAMO ROUTINE NPW08P
      *--------------------------------------------------------
       W0008-RICHIAMO.
           MOVE 'NPW08P'            TO WRK-MODULO.
           MOVE NPW08RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPW08RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0008-END.
           EXIT.
      *--------------------------------------------------------
      * RICHIAMO ROUTINE NPW33P
      *--------------------------------------------------------
       W0033-RICHIAMO.
           MOVE 'NPW33P'            TO WRK-MODULO.
           MOVE NPW33RC             TO WRK-NP11000.
           MOVE NPG01RC             TO WRK-NPG01-11000.
           CALL WRK-MODULO USING WRK-11650.
           MOVE WRK-NP11000         TO NPW33RC.
           MOVE WRK-NPG01-11000     TO NPG01RC.
       W0033-END.
           EXIT.
