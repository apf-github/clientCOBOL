      ******************************************************************
      * DCLGEN TABLE(IDC015.CUSTM_TABLE)                               *
      *        LIBRARY(IDC015.CUSTMSTR.COBOL)                          *
      *        LANGUAGE(COBOL)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
           EXEC SQL DECLARE IDC015.CUSTM_TABLE TABLE
           ( CUSTM_NO                       INTEGER NOT NULL,
             CUSTM_NAME_FRSTNM              CHAR(15) NOT NULL,
             CUSTM_NAME_MDLNM               CHAR(8),
             CUSTM_NAME_LASTNM              CHAR(15) NOT NULL,
             CUSTM_ADDR                     CHAR(35) NOT NULL,
             CUSTM_DOB                      DATE NOT NULL
           ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE IDC015.CUSTM_TABLE                 *
      ******************************************************************
       01  DCLACN-CUSTM-TABLE.
           10 ACN-CUSTM-NO          PIC S9(9) USAGE COMP.
           10 ACN-CUSTM-NAME-FRSTNM PIC X(15).
           10 ACN-CUSTM-NAME-MDLNM PIC X(8).
           10 ACN-CUSTM-NAME-LASTNM PIC X(15).
           10 ACN-CUSTM-ADDR       PIC X(35).
           10 ACN-CUSTM-DOB        PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 6       *
      ******************************************************************
