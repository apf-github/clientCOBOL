 

       01  RVFC007.
           05  F007-CRRRISK.
               10  F007-ENT                PIC X(4).
               10  F007-COD-PROD           PIC X(2).
               10  F007-ENT-CAC            PIC X(4).
               10  F007-CEN-CAC            PIC X(4).
               10  F007-NUM-RISK           PIC X(10).
               10  F007-RISKSEQ            PIC S9(3) COMP-3.
               10  F007-ENT-OWN            PIC X(4).
               10  F007-CEN-OWN            PIC X(4).
               10  F007-TYP-RISK           PIC X(2).
               10  F007-DAT-FML            PIC X(10).
               10  F007-DAT-MAT            PIC X(10).
               10  F007-AMT-FML            PIC S9(15)V9(2) COMP-3.
               10  F007-AMT-AVA            PIC S9(15)V9(2) COMP-3.
               10  F007-DEBTBAL            PIC S9(15)V9(2) COMP-3.
               10  F007-EXPRBAL            PIC S9(15)V9(2) COMP-3.
               10  F007-COD-SLPAY          PIC X(4).
               10  F007-FLG-SLPAYRISK      PIC X.
               10  F007-DAT-NPAY           PIC X(10).
               10  F007-NUM-NPAY           PIC S9(3) COMP-3.
               10  F007-AMT-NPAY           PIC S9(15)V9(2) COMP-3.
               10  F007-AMT-PROV           PIC S9(15)V9(2) COMP-3.
               10  F007-FLG-INTST          PIC X(3).
               10  F007-KEY-CBRISK         PIC X.
               10  F007-FCC                PIC X(3).
               10  F007-KEY-CBTERM         PIC X.
               10  F007-KEY-CBCOLT         PIC X.
               10  F007-KEY-CBST           PIC X.
               10  F007-NCBA               PIC X(5).
               10  F007-COD-SRGN           PIC S9(3) COMP-3.
               10  F007-COD-CBSEC          PIC S9(8) COMP-3.
               10  F007-AMT-NOEXPR         PIC S9(15)V9(2) COMP-3.
               10  F007-AMT-LASTEXPRNPAY   PIC S9(15)V9(2) COMP-3.
               10  F007-AMT-PRVEXPRNPAY    PIC S9(15)V9(2) COMP-3.
               10  F007-IRT                PIC S9(3)V9(6)  COMP-3.
               10  F007-FLG-GUAPLCY        PIC X.
               10  F007-FLG-RFNC           PIC X.
               10  F007-COD-COMMPROD       PIC X(6).
               10  F007-PER-RTN            PIC S9(3) COMP-3.
               10  F007-FLG-INTF           PIC X.
               10  F007-FLG-MOD            PIC X.
               10  F007-ENT-LINKACC        PIC X(4).
               10  F007-BRN-LINKACC        PIC X(4).
               10  F007-ACC-LINK           PIC X(10).
               10  F007-TYP-MATFLG         PIC X.
               10  F007-COD-COLT           PIC X(3).
               10  F007-FLG-LEGJNTRESP     PIC X.
               10  F007-ENT-LASTMOD        PIC X(4).
               10  F007-BRN-LASTMOD        PIC X(4).
               10  F007-COD-LASTMODUSER    PIC X(8).
               10  F007-COD-LASTMODTRM     PIC X(8).
               10  F007-STP                PIC X(26).
               10  F007-TYP-RISKST         PIC X.
               10  F007-ENT-LINK           PIC X(4).
               10  F007-BRN-LINK           PIC X(4).
               10  F007-ACC-LINK-A1        PIC X(10).
               10  F007-ENT-NEGT           PIC X(4).
               10  F007-BRN-NEGT           PIC X(4).
               10  F007-DAT-ST             PIC X(10).
               10  F007-MAXQNTY            PIC S9(5) COMP-3.
               10  F007-AVA1               PIC X(10).
               10  F007-AVA2               PIC X(10).
               10  F007-AVA3               PIC X(10).
               10  F007-FLG-AVA1           PIC X(1).
               10  F007-FLG-AVA2           PIC X(1).
               10  F007-FLG-AVA3           PIC X(1).
               10  F007-AMT-AVA1           PIC S9(13)V9(2) COMP-3.
               10  F007-AMT-AVA2           PIC S9(13)V9(2) COMP-3.
               10  F007-AMT-AVA3           PIC S9(13)V9(2) COMP-3.
               10  F007-DAT-AVA1           PIC X(10).
               10  F007-DAT-AVA2           PIC X(10).
               10  F007-DAT-AVA3           PIC X(10).
               10  FILLER                  PIC X(11).
       05  F007-DTA-RIECCIM.
           10  F007-DAT-RCPT               PIC X(10).
           10  F007-DAT-PROV               PIC X(10).
           10  F007-PER-CAP                PIC X(4).
           10  F007-PER-IRT                PIC X(4).
           10  F007-AMT-CAP                PIC S9(13)V9(2) COMP-3.
           10  F007-AMT-ITR                PIC S9(13)V9(2) COMP-3.
           10  F007-AMT-COM                PIC S9(13)V9(2) COMP-3.
           10  F007-AMT-EXPN               PIC S9(13)V9(2) COMP-3.
           10  F007-AMT-ARR                PIC S9(13)V9(2) COMP-3.
           10  F007-PER-ARR                PIC S9(2)V9(3) COMP-3.
           10  F007-FLG-AVA1-A1            PIC X(1).
      *
           10  F007-AMT-PENDCAP            PIC S9(13)V99 COMP-3.
           10  F007-AMT-PENDCAP-A1   REDEFINES F007-AMT-PENDCAP
                                           PIC X(8).
           10  F007-AMT-LEGALCO            PIC S9(13)V99 COMP-3.
           10  F007-AMT-LEGALCO-A1   REDEFINES F007-AMT-LEGALCO
                                           PIC X(8).
      *
           10  FILLER                      PIC X(02).
       05  F007-DTA-RVECASN.
           10  F007-COD-STOBJDEBT          PIC X(02).
           10  F007-COD-STSUBJDEBT         PIC X(02).
           10  FILLER                      PIC X(06).
       05  F007-DECLIN-RSN                 PIC X(225).
       05  F007-DECLIN-BRN                 PIC X(6).
       05  F007-CUN                        PIC X(8).
       05  F007-COD-CRG                    PIC X(3).
       05  F007-MGT-SECTOR                 PIC X(2).
      *05  F007-COD-BYPROD                 PIC X(4).
       05  F007-COD-OOT                    PIC X(02).
