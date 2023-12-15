       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW33P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW33R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW33R
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
      *  NPW33R    RECUPERO DATI GENERALIZZATI                        *
      *---------------------------------------------------------------*
PEF001* 30/10/07 - OTTIMIZZAZIONE PERFORMANCE                         *
      *---------------------------------------------------------------*
RAT04 * 17/06/08 - PERFORMANCE 2008 - NUOVA FUNZIONE '04' CHE NON     *
RAT04 *            PREVEDE L'INSERT SULLA CKP MA SOLO L'AGGIORNAMENTO *
RAT04 *            DEI CONTATORI PER IL NOPER                         *
SEQUEN* 111109 AGGIUNTA SEQUENCE PER GESTIONE NOPER.                  *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WSK-IND             PIC S9(003) VALUE ZERO.
       01  WKS-NOPER                    PIC 9(015).
       01  WKS-NOPER-R REDEFINES WKS-NOPER.
           05  WKS-PRFIL                PIC 9(005).
           05  WKS-UNOPE                PIC 9(010).
       COPY NPG01RC.
       COPY NPW01RC.
       COPY NPW04RC.
       COPY NPW05RC.
       COPY NPW06RC.
       COPY NPW33RC.
       COPY NPX05RC.
       COPY NPX09RC.
       COPY NPA01TC.
       COPY NPA10TC.
       COPY NPA50TC.
160298 COPY NPA90TC.
           EXEC SQL INCLUDE SQLCA   END-EXEC.
EXPAND*    EXEC SQL INCLUDE NPG03RC END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPG03RC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *                                                               *
      *  *******  **   **  **  *****  **  **                          *
      *  **   **  **   **  **  **     ** **          PRODOTTO         *
      *  **   **  **   **  **  **     ***       ANTICIPO DOCUMENTI    *
      *  ** * **  **   **  **  **     ** **                           *
      *  *******  *******  **  *****  **  **                          *
      *      *                                   INFOSER S.R.L.       *
      *                                                               *
      *                                                               *
      *---------------------------------------------------------------*
      *   AREA DI WORKING UTILIZZATA DAL PRODOTTO ANTICIPO DOCUMENTI  *
      *---------------------------------------------------------------*
       01  WRK-PGMNM               PIC X(08)         VALUE SPACES.
       01  WRK-SERVI               PIC X(04)         VALUE 'NP00'.
       01  WRK-LINGU               PIC X(01)         VALUE '0'.
       01  WRK-SCELT               PIC X(01)         VALUE 'S'.
       01  WRK-RCODE               PIC X(02)         VALUE SPACES.
       01  WRK-MDUTI               PIC X(03)         VALUE SPACES.
       01  WRK-ATTRI               PIC X(02).
       01  WRK-ATTRI-RED REDEFINES WRK-ATTRI PIC S9(004) COMP.
       01  WRK-ATTRI-RE2 REDEFINES WRK-ATTRI.
           05 FILLER               PIC X(01).
           05 WRK-ATTRI-RED2       PIC X(01).
       01  WRK-DESERI              PIC X(70)         VALUE SPACES.
       01  WRK-DESERI-COMP REDEFINES WRK-DESERI.
           05 FILLER               PIC X(12).
           05 WRK-NRERR            PIC X(02).
           05 FILLER               PIC X(02).
           05 WRK-DESCR-STATO      PIC X(54).
       01  WRK-DESERI-OK   REDEFINES WRK-DESERI.
           05 FILLER               PIC X(15).
           05 WRK-NROPE            PIC 9(10).
           05 FILLER               PIC X(45).
       01  WRK-DESERI-GE   REDEFINES WRK-DESERI.
           05 FILLER               PIC X(05).
           05 WRK-SPAZIO           PIC X(01).
           05 WRK-DESCR-GE         PIC X(64).
       01  WRK-DTINS               PIC S9(09) COMP-3 VALUE +0.
       01  WRK-ROUTI               PIC X(11650).
       01  WRK-1150    REDEFINES WRK-ROUTI PIC X(1150).
       01  WRK-1650    REDEFINES WRK-ROUTI PIC X(1650).
       01  WRK-2650    REDEFINES WRK-ROUTI PIC X(2650).
       01  WRK-3650    REDEFINES WRK-ROUTI PIC X(3650).
       01  WRK-4650    REDEFINES WRK-ROUTI PIC X(4650).
       01  WRK-5150    REDEFINES WRK-ROUTI PIC X(5150).
       01  WRK-5650    REDEFINES WRK-ROUTI PIC X(5650).
       01  WRK-6650    REDEFINES WRK-ROUTI PIC X(6650).
       01  WRK-7650    REDEFINES WRK-ROUTI PIC X(7650).
       01  WRK-11650   REDEFINES WRK-ROUTI PIC X(11650).
       01  WRK-NP0500R REDEFINES WRK-ROUTI.
           05 WRK-NP0500            PIC X(0500).
           05 WRK-NPG01-0500        PIC X(0650).
       01  WRK-NP1000R REDEFINES WRK-ROUTI.
           05 WRK-NP1000            PIC X(1000).
           05 WRK-NPG01-1000        PIC X(0650).
       01  WRK-NP2000R REDEFINES WRK-ROUTI.
           05 WRK-NP2000            PIC X(2000).
           05 WRK-NPG01-2000        PIC X(0650).
       01  WRK-NP3000R REDEFINES WRK-ROUTI.
           05 WRK-NP3000            PIC X(3000).
           05 WRK-NPG01-3000        PIC X(0650).
       01  WRK-NP4000R REDEFINES WRK-ROUTI.
           05 WRK-NP4000            PIC X(4000).
           05 WRK-NPG01-4000        PIC X(0650).
       01  WRK-NP5000R REDEFINES WRK-ROUTI.
           05 WRK-NP5000            PIC X(5000).
           05 WRK-NPG01-5000        PIC X(0650).
       01  WRK-NP6000R REDEFINES WRK-ROUTI.
           05 WRK-NP6000            PIC X(6000).
           05 WRK-NPG01-6000        PIC X(0650).
       01  WRK-NP7000R REDEFINES WRK-ROUTI.
           05 WRK-NP7000            PIC X(7000).
           05 WRK-NPG01-7000        PIC X(0650).
       01  WRK-NP11000R REDEFINES WRK-ROUTI.
           05 WRK-NP11000           PIC X(11000).
           05 WRK-NPG01-11000       PIC X(0650).
       01  WRK-NPG000R REDEFINES WRK-ROUTI.
           05 WRK-NPG000            PIC X(0500).
           05 WRK-NPG00-G000        PIC X(4000).
           05 WRK-NPG01-G000        PIC X(0650).
       01  WRK-NPG001R REDEFINES WRK-ROUTI.
           05 WRK-NPG001            PIC X(3000).
           05 WRK-NPG00-G001        PIC X(4000).
           05 WRK-NPG01-G001        PIC X(0650).
       01  WRK-ROU00 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-00         PIC X(4000).
           05 WRK-NPG08-00         PIC X(2500).
       01  WRK-ROU01 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-01         PIC X(4000).
           05 WRK-NPD00-01         PIC X(4000).
           05 WRK-NPG01-01         PIC X(0650).
           05 WRK-NPG02-01         PIC X(2000).
       01  WRK-ROU02 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-02         PIC X(4000).
           05 WRK-NPG01-02         PIC X(0650).
           05 WRK-NPG02-02         PIC X(2000).
       01  WRK-ROU03 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-03         PIC X(4000).
           05 WRK-NPG01-03         PIC X(0650).
       01  WRK-ROU04 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-04         PIC X(4000).
           05 WRK-NPG01-04         PIC X(0650).
       01  WRK-ROU05 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-05         PIC X(4000).
           05 WRK-NPG01-05         PIC X(0650).
           05 WRK-NPG02-05         PIC X(2000).
           05 WRK-NPG05-05         PIC X(4000).
       01  WRK-ROU06 REDEFINES WRK-ROUTI.
           05 WRK-NPG00-06         PIC X(4000).
           05 WRK-NPG01-06         PIC X(0650).
           05 WRK-NPG02-06         PIC X(2000).
       01  WRK-NPG00-LEN           PIC S9(004) COMP VALUE +4000.
       01  WRK-ROU00-LEN           PIC S9(004) COMP VALUE +6500.
       01  WRK-ROU01-LEN           PIC S9(004) COMP VALUE ZERO.
      *  ASSEGNARE A 10650
       01  WRK-ROU02-LEN           PIC S9(004) COMP VALUE +6650.
       01  WRK-ROU03-LEN           PIC S9(004) COMP VALUE +4650.
       01  WRK-ROU04-LEN           PIC S9(004) COMP VALUE +4650.
      *  ASSEGNARE A 10650
       01  WRK-ROU05-LEN           PIC S9(004) COMP VALUE ZERO.
       01  WRK-ROU06-LEN           PIC S9(004) COMP VALUE +6650.
       01  WRK-NP1150-LEN          PIC S9(004) COMP VALUE +1150.
       01  WRK-NP1650-LEN          PIC S9(004) COMP VALUE +1650.
       01  WRK-NP2650-LEN          PIC S9(004) COMP VALUE +2650.
       01  WRK-NP3650-LEN          PIC S9(004) COMP VALUE +3650.
       01  WRK-NP4650-LEN          PIC S9(004) COMP VALUE +4650.
       01  WRK-NP5150-LEN          PIC S9(004) COMP VALUE +5150.
       01  WRK-NP5650-LEN          PIC S9(004) COMP VALUE +5650.
       01  WRK-NP6650-LEN          PIC S9(004) COMP VALUE +6650.
       01  WRK-NP7650-LEN          PIC S9(004) COMP VALUE +7650.
       01  WRK-PIC01               PIC 9(01)         VALUE ZERO.
       01  WRK-PIC02               PIC 9(02)         VALUE ZERO.
       01  WRK-PIC03               PIC 9(03)         VALUE ZERO.
       01  WRK-PIC04               PIC 9(04)         VALUE ZERO.
       01  WRK-PIC05               PIC 9(05)         VALUE ZERO.
       01  WRK-PIC06               PIC 9(06)         VALUE ZERO.
       01  WRK-PIC07               PIC 9(07)         VALUE ZERO.
       01  WRK-PIC08               PIC 9(08)         VALUE ZERO.
       01  WRK-PIC09               PIC 9(09)         VALUE ZERO.
       01  WRK-PIC10               PIC 9(10)         VALUE ZERO.
       01  WRK-PIC11               PIC 9(11)         VALUE ZERO.
       01  WRK-PIC12               PIC 9(12)         VALUE ZERO.
       01  WRK-PIC13               PIC 9(13)         VALUE ZERO.
       01  WRK-PIC14               PIC 9(14)         VALUE ZERO.
       01  WRK-PIC15               PIC 9(15)         VALUE ZERO.
       01  WRK-PIC16               PIC 9(16)         VALUE ZERO.
       01  WRK-PIC17               PIC 9(17)         VALUE ZERO.
       01  WRK-ZZDEC               PIC 99.
       01  WRK-ZZDEC3              PIC 999.
       01  WRK-ZZDEC5              PIC 99999.
       01  WRK-ZZ02                PIC Z9.
       01  WRK-ZZ03                PIC ZZ9.
       01  WRK-ZZ04                PIC ZZZ9.
       01  WRK-ZZ05                PIC ZZZZ9.
       01  WRK-ZZ06                PIC ZZZZZ9.
       01  WRK-ZZ07                PIC ZZZ.ZZ9.
XACO   01  WRK-ZZ07B               PIC ZZZZZZ9.
       01  WRK-ZZ08                PIC ZZZZ.ZZ9.
       01  WRK-ZZ08B               PIC ZZZZZZZ9.
       01  WRK-ZZ09                PIC Z.ZZZ.ZZ9.
XPDA   01  WRK-ZZ09B               PIC ZZZZZZZZ9.
       01  WRK-ZZ10                PIC ZZ.ZZZ.ZZ9.
       01  WRK-ZZ10B               PIC ZZZZZZZZZ9.
EURO   01  WRK-ZZ11                PIC ZZZ.ZZZ.ZZ9.
       01  WRK-ZZ12                PIC ZZZZ.ZZZ.ZZ9.
EURO   01  WRK-ZZ12B               PIC ZZZZZ.ZZZ.ZZ9.
NT2603 01  WRK-ZZ12C               PIC ZZZZZZZZZZZ9.
       01  WRK-ZZ13                PIC Z.ZZZ.ZZZ.ZZ9.
       01  WRK-ZZ14                PIC ZZ.ZZZ.ZZZ.ZZ9.
       01  WRK-ZZ15                PIC ZZZZZZZZZZZZZZ9.
       01  WRK-ZZ17-I              PIC ZZZZZZZZZZZZZZZZ9.
       01  WRK-ZZ17                PIC Z.ZZZ.ZZZ.ZZZ.ZZ9.
 NETTO 01  WRK-IMPOR-99            PIC S9(13)V99 VALUE 9999999999999,99.
       01  WRK-ZETA                PIC Z(17).
       01  CENTO                   PIC S9(08) COMP   VALUE +100.
       01  WRK-FUNZIONI.
           05  GU                  PIC X(4) VALUE 'GU  '.
           05  GN                  PIC X(4) VALUE 'GN  '.
           05  ISRT                PIC X(4) VALUE 'ISRT'.
           05  CHKP                PIC X(4) VALUE 'CHKP'.
           05  ROLB                PIC X(4) VALUE 'ROLB'.
       01  WRK-SETIF.
           05 CREDITORE            PIC X(02)         VALUE '20'.
           05 DEBITORE             PIC X(02)         VALUE '30'.
PANZ   01  WRK-DIVIS.
NTN97      05 LIRE                 PIC X(03)         VALUE 'ITL'.
           05 EURO                 PIC X(03)         VALUE 'EUR'.
       01  WRK-FTPRE.
           05 CARTACEO             PIC X(02)         VALUE '01'.
           05 RIBA                 PIC X(02)         VALUE '02'.
           05 MAV                  PIC X(02)         VALUE '04'.
           05 RID                  PIC X(02)         VALUE '05'.
           05 RIA                  PIC X(02)         VALUE '07'.
           05 DOCUMENTI            PIC X(02)         VALUE '10'.
       01  WRK-ID140-TAB.
           05 FILLER               PIC X(02)         VALUE '01'.
           05 FILLER               PIC X(02)         VALUE '02'.
           05 FILLER               PIC X(02)         VALUE '03'.
           05 FILLER               PIC X(02)         VALUE '04'.
           05 FILLER               PIC X(02)         VALUE '05'.
           05 FILLER               PIC X(02)         VALUE '09'.
           05 FILLER               PIC X(02)         VALUE '21'.
           05 FILLER               PIC X(02)         VALUE '22'.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
           05 FILLER               PIC X(02)         VALUE '  '.
       01  FILLER REDEFINES WRK-ID140-TAB.
           05 WRK-ID140  OCCURS 20 PIC X(002).
       01  WRK-STATE.
           05 IN-ESSERE            PIC X(02)         VALUE '00'.
           05 PAGATO               PIC X(02)         VALUE '01'.
           05 STORNO-AUTOMATICO    PIC X(02)         VALUE '02'.
           05 STORNO-FORZATO       PIC X(02)         VALUE '03'.
       01  WRK-RIMESSA.
           05  WRK-CFILI           PIC 9(05).
           05  WRK-RIMES           PIC 9(10).
       01  WRK-NRRIM-RED REDEFINES WRK-RIMESSA PIC 9(15).
       01  WRK-TACHI.
           05  WRK-TACHI-I         PIC 9(04).
           05  WRK-TACHI-D         PIC 9(05).
       01  WRK-TACHI-RED REDEFINES WRK-TACHI PIC 9(04)V9(5).
       01  WRK-EFFETTO.
           05  WRK-ABIUT           PIC X(05).
           05  WRK-NREFF           PIC X(10).
       01  WRK-NREFF-RED REDEFINES WRK-EFFETTO PIC 9(15).
       01  WRK-IEFFE.
           05  WRK-IEFFI           PIC 9(13).
           05  WRK-IEFFD           PIC 9(02).
       01  WRK-IEFFE-RED REDEFINES WRK-IEFFE         PIC 9(13)V99.
       01  WRK-ERRORE              PIC S9(03) COMP-3 VALUE ZERO.
       01  WRK-TABERR.
           05 WRK-TESTI-ERR OCCURS 5   PIC X(006).
       01  WRK-SW-USCITA           PIC XX VALUE 'SI'.
           88 CONTINUA                    VALUE 'SI'.
       01  WRK-IND                 PIC S9(08) COMP   VALUE +0.
       01  WRK-INDX                PIC S9(08) COMP   VALUE +0.
       01  WRK-MAX-INDX            PIC S9(08) COMP   VALUE 12.
       01  WRK-IMPO                PIC S9(15)V99 COMP-3 VALUE ZERO.
       01  WRK-DATA-COMP           PIC S9(9) COMP-3.
       01  WRK-DATA1               PIC X(008).
       01  WRK-DATA2               PIC X(008).
       01  WRK-DATA-FINE           PIC 9(008) VALUE 21000101.
       01  WRK-DATA.
           05 WRK-ANNO.
              10 WRK-SECOLO        PIC X(002).
              10 WRK-AA            PIC X(002).
           05 WRK-MESE             PIC X(002).
           05 WRK-GIORNO           PIC X(002).
       01  WRK-DATA9   REDEFINES  WRK-DATA    PIC 9(8).
       01  WRK-DATA-10.
           05 WRK-GG-10            PIC X(002).
           05 WRK-BARRA1           PIC X(001).
           05 WRK-MM-10            PIC X(002).
           05 WRK-BARRA2           PIC X(001).
           05 WRK-ANNO-10.
              10 WRK-SS-10         PIC X(002).
              10 WRK-AA-10         PIC X(002).
       01  WRK-DATA10  REDEFINES  WRK-DATA-10 PIC X(10).
       01  COBOL-DATE.
           05 COBDATE-AA           PIC 9(002).
           05 COBDATE-MM           PIC 9(002).
           05 COBDATE-GG           PIC 9(002).
       01  COBOL-TIME.
           05 COBTIME-HH           PIC 9(002).
           05 COBTIME-MM           PIC 9(002).
           05 COBTIME-SS           PIC 9(002).
           05 COBTIME-CC           PIC 9(002).
       01  COBOL-TIME-RED REDEFINES COBOL-TIME PIC 9(008).
       01  COBOL-TIME-7   REDEFINES COBOL-TIME PIC 9(007).
       01  COBOL-TIME-6   REDEFINES COBOL-TIME PIC 9(006).
       01  WRK-DATA-DB2.
           05 WRK-GGDB2            PIC X(002).
           05 FILLER               PIC X(001) VALUE '.'.
           05 WRK-MMDB2            PIC X(002).
           05 FILLER               PIC X(001) VALUE '.'.
           05 WRK-AADB2            PIC X(004).
       01  WRK-DATA-STA.
           05 WRK-GGSTA            PIC X(002).
           05 FILLER               PIC X(001) VALUE '/'.
           05 WRK-MMSTA            PIC X(002).
           05 FILLER               PIC X(001) VALUE '/'.
           05 WRK-AASTA            PIC X(004).
EURO   01  WRK-DATA-STA2.
EURO       05 WRK-GGSTA2           PIC X(002).
EURO       05 FILLER               PIC X(001) VALUE '/'.
EURO       05 WRK-MMSTA2           PIC X(002).
EURO       05 FILLER               PIC X(001) VALUE '/'.
EURO       05 WRK-AASTA2           PIC X(002).
       01  WRK-TAB-IMP.
           03  FILLER        PIC 9(016)    VALUE 9.
           03  FILLER        PIC 9(016)    VALUE 99.
           03  FILLER        PIC 9(016)    VALUE 999.
           03  FILLER        PIC 9(016)    VALUE 9999.
           03  FILLER        PIC 9(016)    VALUE 99999.
           03  FILLER        PIC 9(016)    VALUE 999999.
           03  FILLER        PIC 9(016)    VALUE 9999999.
           03  FILLER        PIC 9(016)    VALUE 99999999.
           03  FILLER        PIC 9(016)    VALUE 999999999.
           03  FILLER        PIC 9(016)    VALUE 9999999999.
           03  FILLER        PIC 9(016)    VALUE 99999999999.
           03  FILLER        PIC 9(016)    VALUE 999999999999.
           03  FILLER        PIC 9(016)    VALUE 9999999999999.
           03  FILLER        PIC 9(016)    VALUE 99999999999999.
           03  FILLER        PIC 9(016)    VALUE 999999999999999.
           03  FILLER        PIC 9(016)    VALUE 9999999999999999.
       01  WRK-TAB-IMP-RED REDEFINES WRK-TAB-IMP.
           03  WRK-ELEM-TAB-IMP OCCURS 16.
               05  WRK-ELEM-IMP PIC 9(16).
       01  WRK-I1            PIC 99.
       01  WRK-I2            PIC 99.
       01  WRK-I3            PIC 99.
       01  WRK-SEGNO         PIC X.
       01  WRK-VIRG          PIC 99.
       01  WRK-TAB-R.
           03  WRK-ZERI      PIC 9(9).
           03  WRK-W-IMP     PIC 9(15).
       01  WRK-W-TAB REDEFINES WRK-TAB-R.
           03  WRK-W-EL   OCCURS 24   PIC X.
       01  WRK-TAB-RR REDEFINES WRK-TAB-R.
           03  FILLER        PIC X(06).
           03  WRK-W-IMP-18  PIC 9(18).
       01  WRK-W-TAB1.
           03  WRK-W-EL1 OCCURS 24 PIC X.
       01  WRK-W-TAB2-R      PIC 999.
       01  WRK-W-TAB2 REDEFINES WRK-W-TAB2-R.
           03  WRK-W-EL2  OCCURS 3    PIC X.
       01  WRK-W-EDIT             PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZZ,ZZZ-.
       01  WRK-SW-CONTR           PIC X(002) VALUE SPACES.
       01  WRK-NROCC              PIC S9(04) COMP VALUE +0.
       01  WRK-NCODA-PAG          PIC S9(04) COMP VALUE +0.
       01  WRK-NCODA-LEN          PIC S9(04) COMP VALUE +0.
       01  WRK-NCODA-ITEM         PIC S9(04) COMP VALUE +0.
       01  WRK-NCODA-AREA         PIC X(300).
       01  SW-NCODA               PIC X(002)      VALUE SPACES.
EURO   01  WRK-ABIUT-W17          PIC X(05)       VALUE SPACES.
EURO   01  WRK-DIVIS-W17          PIC X(03)       VALUE SPACES.
EURO   01  WRK-IDOCU-W17          PIC S9(15)V99   VALUE ZERO.
EURO   01  WRK-IMPOU-W17          PIC S9(15)V99   VALUE ZERO.
EURO   01  WRK-DIOUT-W17          PIC X(003)      VALUE SPACES.
       01  WRK-NCODA.
           05 NCODA-CTERM         PIC X(004).
           05 NCODA-SESSI         PIC X(001).
           05 NCODA-CTRAN         PIC X(001).
           05 NCODA-VARIA         PIC X(002).
       01  WRK-NCODA-COM.
           05 WRK-SESSI-TS        PIC X(002).
           05 WRK-CTRAN-TS.
              10 WRK-CTRAN-TS1    PIC X(002).
              10 WRK-CTRAN-TS2    PIC X(002).
           05 WRK-VARIA-TS        PIC X(002).
      *****************************************************************
      *   CAMPO PER APPOGGIO NOME ROUTINE DA CHIAMARE                 *
      *****************************************************************
       01  WRK-MODULO              PIC X(008) VALUE SPACES.
      *****************************************************************
      *   CAMPO UTILIZZATO PER CHIAMATA NPW14R                        *
      *****************************************************************
       01  WRK-CDRAP               PIC 9(015) VALUE ZEROES.
       01  WRK-CATEG               PIC X(003) VALUE SPACES.
      *****************************************************************
      *   AREA DI APPOGGIO INSPECT                                    *
      *****************************************************************
       01  WRK-AREA-ISP.
           05 WRK-APPOGGIO.
              10 WRK-EL-APPOGGIO PIC X OCCURS 80.
           05 WRK-IND3           PIC 9(003) COMP-3.
           05 WRK-CAMPO1         PIC X(001).
           05 WRK-CAMPO2         PIC X(001).
           05 WRK-USCITA         PIC X(001).
      *****************************************************************
      *   AREA DI APPOGGIO PER CODE TS                                *
      *****************************************************************
       01  WRK-NROCC-X67          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X67          PIC S9(04) COMP VALUE +500.
       01  WRK-NROCC-X70          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X70          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X71          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X71          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X73          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X73          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X74          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X74          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X75          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X75          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X76          PIC S9(04) COMP VALUE +10.
       01  WRK-NCODA-X76          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X77          PIC S9(04) COMP VALUE +12.
       01  WRK-NCODA-X77          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X78          PIC S9(04) COMP VALUE +12.
       01  WRK-NCODA-X78          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X79          PIC S9(04) COMP VALUE +12.
       01  WRK-NCODA-X79          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X80          PIC S9(04) COMP VALUE +12.
       01  WRK-NCODA-X80          PIC S9(04) COMP VALUE +200.
       01  WRK-NROCC-X89          PIC S9(04) COMP VALUE +20.
       01  WRK-NCODA-X89          PIC S9(04) COMP VALUE +200.
      *---------------------------------------------------------------*
       01  WRK-IMP-FORM               PIC 9(15)V9(2)    VALUE ZEROES.
       01  FILLER   REDEFINES         WRK-IMP-FORM.
           05  WRK-IMP-FORM-I         PIC 9(15).
           05  WRK-IMP-FORM-D         PIC 9(02).
      *  CAMPO OUT  LUNGO 22                                          *
       01  WRK-FORM-PIC22.
           05  WRK-FORM-PIC22-I       PIC ZZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC22-V       PIC X(01).
           05  WRK-FORM-PIC22-DX.
               07  WRK-FORM-PIC22-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 21                                          *
       01  WRK-FORM-PIC21.
           05  WRK-FORM-PIC21-I       PIC ZZ.ZZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC21-V       PIC X(01).
           05  WRK-FORM-PIC21-DX.
               07  WRK-FORM-PIC21-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 20                                          *
       01  WRK-FORM-PIC20.
           05  WRK-FORM-PIC20-I       PIC Z.ZZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC20-V       PIC X(01).
           05  WRK-FORM-PIC20-DX.
               07  WRK-FORM-PIC20-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 19                                          *
       01  WRK-FORM-PIC19.
           05  WRK-FORM-PIC19-I       PIC ZZZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC19-V       PIC X(01).
           05  WRK-FORM-PIC19-DX.
               07  WRK-FORM-PIC19-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 18                                          *
       01  WRK-FORM-PIC18.
           05  WRK-FORM-PIC18-I       PIC ZZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC18-V       PIC X(01).
           05  WRK-FORM-PIC18-DX.
               07  WRK-FORM-PIC18-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 17                                          *
       01  WRK-FORM-PIC17.
           05  WRK-FORM-PIC17-I       PIC ZZ.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC17-V       PIC X(01).
           05  WRK-FORM-PIC17-DX.
               07  WRK-FORM-PIC17-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 16                                          *
       01  WRK-FORM-PIC16.
           05  WRK-FORM-PIC16-I       PIC Z.ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC16-V       PIC X(01).
           05  WRK-FORM-PIC16-DX.
               07  WRK-FORM-PIC16-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 16                                          *
       01  WRK-FORM-PIC16A.
           05  WRK-FORM-PIC16A-I      PIC ZZZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC16A-V      PIC X(01).
           05  WRK-FORM-PIC16A-DX.
               07  WRK-FORM-PIC16A-D  PIC 9(02).
      *  CAMPO OUT  LUNGO 15                                          *
       01  WRK-FORM-PIC15.
           05  WRK-FORM-PIC15-I       PIC ZZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC15-V       PIC X(01).
           05  WRK-FORM-PIC15-DX.
               07  WRK-FORM-PIC15-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 14                                          *
       01  WRK-FORM-PIC14.
           05  WRK-FORM-PIC14-I       PIC ZZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC14-V       PIC X(01).
           05  WRK-FORM-PIC14-DX.
               07  WRK-FORM-PIC14-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 13                                          *
       01  WRK-FORM-PIC13.
           05  WRK-FORM-PIC13-I       PIC ZZ.ZZZ.ZZ9.
           05  WRK-FORM-PIC13-V       PIC X(01).
           05  WRK-FORM-PIC13-DX.
               07  WRK-FORM-PIC13-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 12                                          *
       01  WRK-FORM-PIC12.
           05  WRK-FORM-PIC12-I       PIC Z.ZZZ.ZZ9.
           05  WRK-FORM-PIC12-V       PIC X(01).
           05  WRK-FORM-PIC12-DX.
               07  WRK-FORM-PIC12-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 11                                          *
       01  WRK-FORM-PIC11.
           05  WRK-FORM-PIC11-I       PIC ZZZZ.ZZ9.
           05  WRK-FORM-PIC11-V       PIC X(01).
           05  WRK-FORM-PIC11-DX.
               07  WRK-FORM-PIC11-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 10                                          *
       01  WRK-FORM-PIC10.
           05  WRK-FORM-PIC10-I       PIC ZZZ.ZZ9.
           05  WRK-FORM-PIC10-V       PIC X(01).
           05  WRK-FORM-PIC10-DX.
               07  WRK-FORM-PIC10-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 09                                          *
       01  WRK-FORM-PIC09.
           05  WRK-FORM-PIC09-I       PIC ZZ.ZZ9.
           05  WRK-FORM-PIC09-V       PIC X(01).
           05  WRK-FORM-PIC09-DX.
               07  WRK-FORM-PIC09-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 09                                          *
       01  WRK-FORM-PIC09A.
           05  WRK-FORM-PIC09A-I      PIC ZZZZZ9.
           05  WRK-FORM-PIC09A-V      PIC X(01).
           05  WRK-FORM-PIC09A-DX.
               07  WRK-FORM-PIC09A-D  PIC 9(02).
      *  CAMPO OUT  LUNGO 08                                          *
E00014 01  WRK-FORM-PIC08.
E00014     05  WRK-FORM-PIC08-I       PIC ZZZZ9.
E00014     05  WRK-FORM-PIC08-V       PIC X(01).
E00014     05  WRK-FORM-PIC08-DX.
E00014         07  WRK-FORM-PIC08-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 07                                          *
E00014 01  WRK-FORM-PIC07.
E00014     05  WRK-FORM-PIC07-I       PIC ZZZ9.
E00014     05  WRK-FORM-PIC07-V       PIC X(01).
E00014     05  WRK-FORM-PIC07-DX.
E00014         07  WRK-FORM-PIC07-D   PIC 9(02).
      *  CAMPO OUT  LUNGO 07                                          *
L00014 01  WRK-FORM-PIC07A.
L00014     05  WRK-FORM-PIC07A-I       PIC ZZ9.
L00014     05  WRK-FORM-PIC07A-V       PIC X(01).
L00014     05  WRK-FORM-PIC07A-DX.
L00014         07  WRK-FORM-PIC07A-D   PIC 9(03).
      *---------------------------------------------------------------*
       01  WRK-IMP-FORM-2             PIC 9(15)V9(3)    VALUE ZEROES.
       01  FILLER   REDEFINES         WRK-IMP-FORM-2.
           05  WRK-IMP-FORM2-I        PIC 9(15).
           05  WRK-IMP-FORM2-D        PIC 9(03).
      *  CAMPO OUT  LUNGO 06                                          *
       01  WRK-FORM2-PIC06.
           05  WRK-FORM2-PIC06-I      PIC Z9.
           05  WRK-FORM2-PIC06-V      PIC X(01).
           05  WRK-FORM2-PIC06-DX.
               07  WRK-FORM2-PIC06-D  PIC 9(03).
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPG03RC
EXPAND*--------------------------------------------------------
           EXEC SQL INCLUDE NPCKPT  END-EXEC.
           EXEC SQL INCLUDE NPDATT  END-EXEC.
           EXEC SQL INCLUDE NPCTRT  END-EXEC.
      *------------------------------------------------------*
      * CURSORE PER DATI ISTITUTI UTENTI                     *
      *------------------------------------------------------*
           EXEC SQL
                DECLARE CUR-ABI CURSOR FOR
                SELECT  *
                FROM   NPDATT
                WHERE  DAT_CODIT = 'A01'
                ORDER BY DAT_KEYGE
PEF001          WITH UR
           END-EXEC.
      ***************************************************************
      **           I N I Z I O    P R O G R A M M A                **
      ***************************************************************
       LINKAGE SECTION.
       01  WRK-COMMAREA        PIC X(11650).
      *---------------------------------------------------------------*
      *  PROCEDURE DIVISION                                           *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING WRK-COMMAREA.
       INIZIO-PGM.
           MOVE WRK-COMMAREA   TO WRK-NP11000R.
           MOVE WRK-NP11000                     TO NPW33RC.
           MOVE WRK-NPG01-11000                 TO NPG01RC.
           PERFORM A0000-HOUSE-KEEPING THRU A0000-END.
           IF CONTINUA
              PERFORM A0010-ELABORA    THRU A0010-END.
           MOVE NPW33RC                         TO WRK-NP11000.
           MOVE NPG01RC                         TO WRK-NPG01-11000.
           MOVE WRK-NP11000R TO WRK-COMMAREA.
           GOBACK.
       END-PGM.
      ***************************************************************
      ** INIZIALIZZAZIONE DATI                                     **
      ***************************************************************
       A0000-HOUSE-KEEPING.
           MOVE ZERO       TO  WSK-IND.
           MOVE 'NPW33R'   TO  WRK-PGMNM.
           MOVE SPACES     TO  NPG01RC.
           MOVE 99         TO  W33-RCOOK.
           MOVE 99         TO  W33-RCOKO.
RAT04 *    IF W33-FUNZI EQUAL '02'
RAT04      IF W33-FUNZI EQUAL '02' OR '04'
              GO TO A0000-END.
           MOVE SPACES     TO  NPCKPT.
           MOVE W33-PGMNM  TO  CKP-PGMNM.
           MOVE W33-KEYCM  TO  CKP-KEYCM.
           MOVE W33-KEYER  TO  CKP-KEYER.
           MOVE W33-NPATI  TO  CKP-NPATI.
           MOVE NPCKPT     TO  X05-NPCKPT.
           MOVE 'IS'       TO  X05-FUNZI.
           PERFORM X0005-RICHIAMO THRU X0005-END.
           IF X05-RCODE = 'KO'
              MOVE 'KO'   TO W33-RCODE
              MOVE SPACES TO WRK-SW-USCITA.
           IF CONTINUA
              EXEC SQL COMMIT END-EXEC
              IF SQLCODE NOT EQUAL ZERO
                 MOVE 'KO'   TO W33-RCODE
                 MOVE 'A0000-HOUSE-KEEP'    TO  G01-LABEL
                 MOVE 'COMMIT'              TO  G01-ARCHI
                 MOVE SQLCODE               TO  G01-PIC3
                 MOVE 'ERRORE IN COMMIT'    TO  G01-DESC1
                 MOVE SQLCA                 TO  G01-SQLCA
                 MOVE SPACES                TO  WRK-SW-USCITA.
           IF CONTINUA
              IF X05-RCODE EQUAL 'DP'
                 MOVE SPACES              TO   NPG01RC
                 MOVE 'RE'                TO   X05-FUNZI
                 PERFORM X0005-RICHIAMO   THRU X0005-END
                 IF X05-RCODE EQUAL SPACES
                    MOVE X05-NPCKPT      TO   NPCKPT
                    MOVE CKP-KEYCM       TO   W33-KEYCM
                    MOVE CKP-KEYER       TO   W33-KEYER
                    MOVE CKP-NPATI       TO   W33-NPATI
                 ELSE
                    MOVE 'KO'            TO  W33-RCODE
                    MOVE SPACES          TO  WRK-SW-USCITA.
           IF CONTINUA
              IF X05-RCODE EQUAL SPACES
                 MOVE SPACES          TO   NPG01RC.
       A0000-END.
           EXIT.
      ***************************************************************
      ** ELABORAZIONE                                              **
      ***************************************************************
       A0010-ELABORA.
           PERFORM A0020-OPEN-CUR-ABI   THRU A0020-END.
           IF CONTINUA
              PERFORM A0030-FETCH-CUR-ABI  THRU A0030-END.
           IF CONTINUA
              PERFORM A0040-CICLO-CUR-ABI  THRU A0040-END
                  UNTIL SQLCODE EQUAL CENTO
                     OR NOT CONTINUA.
           MOVE WSK-IND                   TO W33-NRUTE.
           IF CONTINUA
              PERFORM A0050-CLOSE-CUR-ABI  THRU A0050-END.
           IF CONTINUA
              EXEC SQL COMMIT END-EXEC
              IF SQLCODE NOT EQUAL ZERO
                 MOVE 'KO'                  TO  W33-RCODE
                 MOVE 'A0010-ELABORA'       TO  G01-LABEL
                 MOVE 'COMMIT'              TO  G01-ARCHI
                 MOVE SQLCODE               TO  G01-PIC3
                 MOVE 'ERRORE IN COMMIT'    TO  G01-DESC1
                 MOVE SQLCA                 TO  G01-SQLCA
                 MOVE SPACES                TO  WRK-SW-USCITA.
           IF W33-NRUTE EQUAL ZERO
              MOVE 'KO'                   TO W33-RCODE.
       A0010-END.
           EXIT.
      **************************************************************
      ** APERTURA CURSORE                                         **
      **************************************************************
       A0020-OPEN-CUR-ABI.
           EXEC SQL
                OPEN CUR-ABI
           END-EXEC.
           IF  SQLCODE NOT EQUAL ZEROES
               MOVE 'KO'                  TO  W33-RCODE
               MOVE 'A0020-OPEN-CUR-ABI'  TO  G01-LABEL
               MOVE 'NPDATT'              TO  G01-ARCHI
               MOVE SQLCODE               TO  G01-PIC3
               MOVE 'ERRORE OPEN CUR-ABI' TO  G01-DESC1
               MOVE SQLCA                 TO  G01-SQLCA
               MOVE SPACES                TO  WRK-SW-USCITA.
       A0020-END.
           EXIT.
      **************************************************************
      ** FETCH CURSORE                                            **
      **************************************************************
       A0030-FETCH-CUR-ABI.
           EXEC SQL FETCH  CUR-ABI
                    INTO  :NPDATT
           END-EXEC.
           IF SQLCODE NOT EQUAL CENTO AND
              SQLCODE NOT EQUAL ZERO
              MOVE SPACES                  TO  WRK-SW-USCITA
              MOVE 'KO'                    TO  W33-RCODE
              MOVE SQLCODE                 TO  G01-PIC3
              MOVE 'ERRORE FETCH CUR-ABI'  TO  G01-DESC1
              MOVE 'A0030-FETCH-CUR-ABI'   TO  G01-LABEL
              MOVE SQLCA                   TO  G01-SQLCA.
           IF SQLCODE EQUAL CENTO
              IF WSK-IND EQUAL ZERO
                 MOVE SPACES               TO  WRK-SW-USCITA
                 MOVE 'KO'                 TO  W33-RCODE
                 MOVE SQLCODE              TO  G01-PIC3
                 MOVE 'TABELLA A01 VUOTA'  TO  G01-DESC1
                 MOVE 'A0030-FETCH-ABI'    TO  G01-LABEL
                 MOVE SQLCA                TO  G01-SQLCA.
           IF SQLCODE EQUAL ZERO
              MOVE NPDATT                  TO NPA01TC.
       A0030-END.
           EXIT.
      **************************************************************
      ** CICLO DI CARICAMENTO DELLA TABELLA ABI/DATA SISTEMA      **
      **************************************************************
       A0040-CICLO-CUR-ABI.
           COMPUTE WSK-IND = WSK-IND + 1.
           MOVE A01-ABIUT               TO  W33-ABIUT(WSK-IND).
           MOVE A01-SERVI               TO  W33-SERVI(WSK-IND).
           MOVE A01-FILCE               TO  W33-FILCE(WSK-IND).
           MOVE A01-FEANT               TO  W33-FEANT(WSK-IND).
           MOVE A01-DESBA               TO  W33-DESBA(WSK-IND).
           MOVE A01-TESTA               TO  W33-TESTA(WSK-IND).
           MOVE A01-FGDEB               TO  W33-FGDEB(WSK-IND).
           MOVE A01-FPAGA               TO  W33-FPAGA(WSK-IND).
           MOVE A01-FILIN               TO  W33-FILIN(WSK-IND).
           MOVE A01-FI365               TO  W33-FI365(WSK-IND).
           MOVE A01-FRETR               TO  W33-FRETR(WSK-IND).
           MOVE A01-FILDG               TO  W33-FILDG(WSK-IND).
ICCREA     MOVE A01-FADIN               TO  W33-FADIN(WSK-IND).
FP2702     MOVE A01-FSPED               TO  W33-FSPED(WSK-IND).
L00083     MOVE A01-FGTEG               TO  W33-FGTEG(WSK-IND).
           MOVE SPACES                  TO  NPW04RC.
           MOVE A01-ABIUT               TO  W04-ABIUT.
           MOVE W33-PGMNM               TO  W04-PGMNM.
           PERFORM W0004-RICHIAMO THRU W0004-END.
           IF  W04-RCODE NOT EQUAL SPACES
               MOVE 'KO'                TO  W33-RCODE
               MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE W04-DTLAV            TO  W33-DTLAV(WSK-IND)
              MOVE W04-DATA-STAMPA      TO  W33-DTSTA(WSK-IND)
              MOVE W04-ORA-DEC          TO  W33-HORAS(WSK-IND)
              MOVE W04-ORA-STAMPA       TO  W33-HRSTA(WSK-IND)
              MOVE SPACES               TO  NPW05RC
              MOVE A01-ABIUT            TO  W05-ABIUT
              MOVE W33-PGMNM            TO  W05-PGMNM
              MOVE W04-DTLAV            TO  W05-DTCON
              MOVE 'B'                  TO  W05-TIPGM
              PERFORM W0005-RICHIAMO THRU W0005-END
              IF  W05-RCODE NOT EQUAL SPACES
                  MOVE 'KO'                TO  W33-RCODE
                  MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE W05-DTCO2               TO  W33-DTCON(WSK-IND)
              PERFORM A0045-LEGGI-A10      THRU A0045-END.
           IF CONTINUA
              PERFORM A0047-LEGGI-A50      THRU A0047-END.
160298     IF CONTINUA
160298        PERFORM A0048-LEGGI-A90      THRU A0048-END.
           IF CONTINUA
              PERFORM A0030-FETCH-CUR-ABI  THRU A0030-END.
       A0040-END.
           EXIT.
      **************************************************************
      ** LETTURA TABELLA A10                                      **
      **************************************************************
       A0045-LEGGI-A10.
           MOVE SPACES                  TO  NPW06RC.
           MOVE A01-ABIUT               TO  W06-ABIUT.
           MOVE A01-FILCE               TO  W06-CFILI.
           MOVE A01-SERVI               TO  W06-SERVI.
           MOVE 'RE'                    TO  W06-FUNZI.
XBDN01     IF  W33-PGMNM  NOT =  'NPSP001B'
               PERFORM W0006-RICHIAMO THRU W0006-END.
           IF  W06-RCODE NOT EQUAL SPACES
               MOVE W06-RCODE           TO  W33-RCODE
               MOVE SPACES              TO  WRK-SW-USCITA
               IF G01-DESC1 EQUAL SPACES AND
                  G01-DESC2 EQUAL SPACES
                  MOVE 'ERR. ROUTINE NPW06R'  TO G01-DESC1
                  MOVE W06-CFILI              TO G01-DESC2.
           IF CONTINUA
              MOVE SPACES                  TO  NPW01RC
              MOVE SPACES                  TO  NPA10TC
              MOVE 'A10'                   TO  A10-CODIT
              MOVE A01-ABIUT               TO  A10-ABIUT
              MOVE W33-PGMNM               TO  A10-PGMNM
              MOVE A10-KEY                 TO  W01-NPDATT
              MOVE 'RE'                    TO  W01-FUNZI
              PERFORM W0001-RICHIAMO THRU W0001-END
              IF W01-RCODE NOT EQUAL SPACES
                 DISPLAY 'NPW33P *******************'
                 DISPLAY 'NPW33P INSERIRE PROGRAMMA ' W33-PGMNM
                 DISPLAY 'NPW33P IN NPDATT          '
                 DISPLAY 'NPW33P *******************'
MANUT *          MOVE 'KO'                TO  W33-RCODE
MANUT            MOVE '01'                TO  W33-RCODE
                 MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE W01-NPDATT             TO  NPA10TC
              MOVE A10-CDOPE              TO  W33-CDOPE(WSK-IND)
              MOVE A10-FRQCM              TO  W33-FRQCM(WSK-IND)
              MOVE A10-FDUMP              TO  W33-FDUMP(WSK-IND)
              MOVE A10-QTERR              TO  W33-QTERR(WSK-IND)
              MOVE A10-IDENT              TO  W33-IDENT(WSK-IND)
              MOVE A10-FCONT              TO  W33-FCONT(WSK-IND)
              MOVE A10-DESCR              TO  W33-DESCR(WSK-IND).
           IF CONTINUA
              MOVE ZERO                   TO  W33-NOPER(WSK-IND)
RAT04 *       IF W33-FUNZI EQUAL '01'
RAT04         IF W33-FUNZI EQUAL '01' OR '04'
L00264           PERFORM A0046-REAUPD-NPCTRT THRU A0046-END
L00264        ELSE
L00264           IF W33-FUNZI EQUAL '03'
L00264        MOVE 1                      TO  W33-NOPER(WSK-IND).
       A0045-END.
           EXIT.
      **************************************************************
      ** LETTURA E AGGIORNAMENTO TABELLA NPCTRT                   **
      **************************************************************
       A0046-REAUPD-NPCTRT.
           MOVE SPACES                  TO  NPX09RC.
           MOVE A01-ABIUT               TO  CTR-ABIUT.
           MOVE W06-PRFIL               TO  CTR-PRFIL.
           MOVE NPCTRT                  TO  X09-NPCTRT.
SEQUEN*    MOVE 'B'                     TO  X09-FOPTB.
SEQUEN     MOVE 'NV'                    TO  X09-FUNZI.
           PERFORM X0009-RICHIAMO THRU X0009-END.
           IF  X09-RCODE NOT EQUAL SPACES
               MOVE 'KO'                TO  W33-RCODE
               MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE X09-NPCTRT              TO  NPCTRT
              MOVE CTR-UNOPE               TO  WKS-UNOPE
              MOVE CTR-PRFIL               TO  WKS-PRFIL
SEQUEN        MOVE WKS-NOPER               TO  W33-NOPER(WSK-IND).
SEQUEN*       ADD  1                       TO  CTR-UNOPE
SEQUEN*       MOVE A10-CDOPE               TO  CTR-CDOPE
SEQUEN*       MOVE NPCTRT                  TO  X09-NPCTRT
SEQUEN*       MOVE 'B'                     TO  X09-FOPTB
SEQUEN*       MOVE 'U6'                    TO  X09-FUNZI
PERINF*       MOVE 'UP'                    TO  X09-FUNZI
SEQUEN*       PERFORM X0009-RICHIAMO THRU X0009-END
SEQUEN*       IF  X09-RCODE NOT EQUAL SPACES
SEQUEN*           MOVE 'KO'                TO  W33-RCODE
SEQUEN*           MOVE SPACES              TO  WRK-SW-USCITA.
       A0046-END.
           EXIT.
      **************************************************************
      ** LETTURA TABELLA PARAMETRI A50                            **
      **************************************************************
       A0047-LEGGI-A50.
           MOVE SPACES                  TO  NPW01RC.
           MOVE SPACES                  TO  NPA50TC.
           MOVE 'A50'                   TO  A50-CODIT.
           MOVE A01-ABIUT               TO  A50-ABIUT.
           MOVE NPA50TC                 TO  W01-NPDATT.
           MOVE 'RE'                    TO  W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE 'KO'                TO  W33-RCODE
              MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE W01-NPDATT             TO  NPA50TC
              MOVE A50-FSFBD              TO  W33-FSFBD(WSK-IND)
              MOVE A50-FSFBE              TO  W33-FSFBE(WSK-IND).
       A0047-END.
           EXIT.
      **************************************************************
      ** LETTURA TABELLA PARAMETRI A90                            **
      **************************************************************
160298 A0048-LEGGI-A90.
           MOVE SPACES                  TO  NPW01RC.
           MOVE SPACES                  TO  NPA90TC.
           MOVE 'A90'                   TO  A90-CODIT.
           MOVE A01-ABIUT               TO  A90-ABIUT.
           MOVE NPA90TC                 TO  W01-NPDATT.
           MOVE 'RE'                    TO  W01-FUNZI.
           PERFORM W0001-RICHIAMO THRU W0001-END.
           IF W01-RCODE NOT EQUAL SPACES
              MOVE 'KO'                TO  W33-RCODE
              MOVE SPACES              TO  WRK-SW-USCITA.
           IF CONTINUA
              MOVE W01-NPDATT             TO  NPA90TC
              MOVE A90-FSPEB              TO  W33-FSPEB(WSK-IND)
L00049        MOVE A90-FILBA              TO  W33-FILBA(WSK-IND)
L00080        MOVE A90-FGETI              TO  W33-FGETI(WSK-IND).
       A0048-END.
           EXIT.
      **************************************************************
      ** CHIUSURA CURSORE                                         **
      **************************************************************
       A0050-CLOSE-CUR-ABI.
           EXEC SQL
                CLOSE CUR-ABI
           END-EXEC.
           IF  SQLCODE NOT EQUAL ZEROES
               MOVE 'KO'                   TO  W33-RCODE
               MOVE 'A0050-CLOSE-CUR-ABI'  TO  G01-LABEL
               MOVE 'NPDATT'               TO  G01-ARCHI
               MOVE SQLCODE                TO  G01-PIC3
               MOVE 'ERRORE CLOSE CUR-ABI' TO  G01-DESC1
               MOVE SQLCA                  TO  G01-SQLCA
               MOVE SPACES                 TO  WRK-SW-USCITA.
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
EXPAND*    EXEC SQL INCLUDE NPW04RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW04RR
EXPAND*--------------------------------------------------------
       W0004-RICHIAMO.
           MOVE 'NPW04P'            TO WRK-MODULO.
           MOVE NPW04RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO   USING WRK-1150.
           MOVE WRK-NP0500          TO NPW04RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0004-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW04RR
EXPAND*--------------------------------------------------------
EXPAND*    EXEC SQL INCLUDE NPW05RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW05RR
EXPAND*--------------------------------------------------------
       W0005-RICHIAMO.
           MOVE 'NPW05P'            TO WRK-MODULO.
           MOVE NPW05RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPW05RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0005-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW05RR
EXPAND*--------------------------------------------------------
EXPAND*    EXEC SQL INCLUDE NPW06RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW06RR
EXPAND*--------------------------------------------------------
       W0006-RICHIAMO.
           MOVE 'NPW06P'            TO WRK-MODULO.
           MOVE NPW06RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPW06RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       W0006-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW06RR
EXPAND*--------------------------------------------------------
EXPAND*    EXEC SQL INCLUDE NPX05RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPX05RR
EXPAND*--------------------------------------------------------
       X0005-RICHIAMO.
           MOVE 'NPX05P'            TO WRK-MODULO.
           MOVE NPX05RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPX05RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       X0005-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPX05RR
EXPAND*--------------------------------------------------------
EXPAND*    EXEC SQL INCLUDE NPX09RR END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPX09RR
EXPAND*--------------------------------------------------------
       X0009-RICHIAMO.
           MOVE 'NPX09P'            TO WRK-MODULO.
           MOVE NPX09RC             TO WRK-NP0500.
           MOVE NPG01RC             TO WRK-NPG01-0500.
           CALL WRK-MODULO USING WRK-1150.
           MOVE WRK-NP0500          TO NPX09RC.
           MOVE WRK-NPG01-0500      TO NPG01RC.
       X0009-END.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPX09RR
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW33R
EXPAND*--------------------------------------------------------
