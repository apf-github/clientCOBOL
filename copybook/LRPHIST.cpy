           EXEC SQL DECLARE IDC015.LOANRPHST TABLE
           ( LOANRPH_CAT                    CHAR(2) NOT NULL,
             LOANRPH_NO                     INTEGER NOT NULL,
             LOANRPH_DATE                   DATE NOT NULL,
             LOANRPH_RP_AMT                 INTEGER NOT NULL,
             LOANRPH_OS_AMT                 INTEGER NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LOANRPHST                          *
      ******************************************************************
       01  DCLLOANRPH61.
      *                       LOANRPH_CAT
           10 RLOANRPH-CAT         PIC X(2).
      *                       LOANRPH_NO
           10 RLOANRPH-NO          PIC S9(9) USAGE COMP.
      *                       LOANRPH_DATE
           10 RLOANRPH-DATE        PIC X(10).
      *                       LOANRPH_RP_AMT
           10 RLOANRPH-RP-AMT      PIC S9(9) USAGE COMP.
      *                       LOANRPH_OS_AMT
           10 RLOANRPH-OS-AMT      PIC S9(9) USAGE COMP.
