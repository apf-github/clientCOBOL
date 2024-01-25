      *SKELETON COBOL COPYBOOK
           EXEC SQL DECLARE IDC015.LOANMSTR TABLE
           ( LOANM_CAT                      CHAR(2) NOT NULL,
             LOANM_NO                       INTEGER NOT NULL,
             LOANM_TERM                     SMALLINT NOT NULL,
             LOANM_TITLE                    CHAR(15),
             LOANM_ROI                      SMALLINT NOT NULL,
             LOANM_MAX_AMT                  INTEGER NOT NULL,
             LOANM_MIN_AGE                  SMALLINT NOT NULL,
             LOANM_MAX_AGE                  SMALLINT NOT NULL,
             LOANM_ST                       CHAR(1) NOT NULL,
             LOANM_CUST_NO                  CHAR(10) NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LOANMSTR                           *
      ******************************************************************
       01  DCLLOANMSTR.
      *                       LOANM_CAT
           10 LLOANM-CAT           PIC X(2).
      *                       LOANM_NO
           10 LLOANM-NO            PIC S9(9) USAGE COMP.
      *                       LOANM_TERM
           10 LLOANM-TERM          PIC S9(4) USAGE COMP.
      *                       LOANM_TITLE
           10 LLOANM-TITLE         PIC X(15).
      *                       LOANM_ROI
           10 LLOANM-ROI           PIC S9(4) USAGE COMP.
      *                       LOANM_MAX_AMT
           10 LLOANM-MAX-AMT       PIC S9(9) USAGE COMP.
      *                       LOANM_MIN_AGE
           10 LLOANM-MIN-AGE       PIC S9(4) USAGE COMP.
      *                       LOANM_MAX_AGE
           10 LLOANM-MAX-AGE       PIC S9(4) USAGE COMP.
      *                       LOANM_ST
           10 LLOANM-ST            PIC X(1).
      *                       CUST NO
           10 LLOANM-CUST-NO       PIC X(10).
