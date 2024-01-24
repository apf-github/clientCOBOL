      * RV3C0100: CALCULATION OF PERMISSION AMOUNT                     *
      ******************************************************************
      * FILES USED                                                     *
      * -------------------                                            *
      * I1DQ0001: CURRENT RISKS FILE, WITHOUT REPETITIONS              *
      * O1DQ0001: APPLICATIONS RISKS                                   *
      *                                                                *
      * COPYS USED                                                     *
      * ----------------                                               *
      * QBEC9900                                                       *
      * QBEC999                                                        *
      * QRECDB2                                                        *
      * RVFC007: INTERFACE OF ACTIVE RISKS CONTRACTS WITH PROCESSED    *
      * AND REJECTED STATUS                                            *
      *                                                                *
      * ROUTINES INVOKED                                               *
      * -----------------                                              *
      * QB8C9900                                                       *
      * QR4CDB0                                                        *
      ******************************************************************
      *                  IDENTIFICATION DIVISION                       *
      ******************************************************************
       IDENTIFICATION DIVISION.

       PROGRAM-ID.    RV3C0100.
      ******************************************************************
      *                     MODIFICATIONS LOG                          *
      ******************************************************************
      *     CODE       AUTHOR  DATE     DESCRIPTION                    *
      *     ---------- ------- -------- ------------------------------ *
      ******************************************************************
      ******************************************************************
      *                     ENVIRONMENT DIVISION                       *
      ******************************************************************
       ENVIRONMENT DIVISION.

       CONFIGURATION SECTION.

       SPECIAL-NAMES.

               DECIMAL-POINT IS COMMA.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.

           SELECT INPUT-FILE     ASSIGN I1DQ0001.
           SELECT OUTPUT-FILE    ASSIGN O1DQ0001.

      ******************************************************************
      *                       DATA DIVISION                            *
      ******************************************************************
       DATA DIVISION.
      *
      ******************************************************************
      *                       FILE SECTION                             *
      ******************************************************************
       FILE SECTION.

       FD  INPUT-FILE
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  RE-INPUT                        PIC X(750).


       FD  OUTPUT-FILE
           LABEL RECORD STANDARD
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE IS F.

       01  RE-OUTPUT                       PIC X(750).


      ******************************************************************
      *                  WORKING-STORAGE SECTION                       *
      ******************************************************************
       WORKING-STORAGE SECTION.


           COPY RVFC007.

       01  VA-QBEC9900-01.
           COPY QBEC9900.

           COPY QRECDB2.

       01  VA-QBEC999-01.
           COPY QBEC999.


       01  VA-SW.

           05  SW-INP-END                   PIC X(1).
               88  INP-NO-END                       VALUE 'N'.
               88  INP-YES-END                      VALUE 'S'.

       01  VA-ALCONS.

           05  CA-LOAN                     PIC X(2)    VALUE '96'.

           05  CA-DEPOSIT1                 PIC X(2)    VALUE '01'.
           05  CA-DEPOSIT2                 PIC X(2)    VALUE '02'.
           05  CA-DEPOSIT3                 PIC X(2)    VALUE '07'.

           05  CA-QB8C9900                 PIC X(8)  VALUE 'QB8C9900'.
           05  CA-PROGRAM                  PIC X(8)  VALUE 'RV3C0100'.
           05  CA-QR4CDB0                  PIC X(7)  VALUE 'QR4CDB0'.
           05  CA-A                        PIC X     VALUE 'A'.


      ******************************************************************
      *                       PROCEDURE DIVISION                       *
      ******************************************************************
       PROCEDURE DIVISION.

           PERFORM 100000-START.

           PERFORM 200000-PROCESS.

           PERFORM 300000-END.

      *
      ******************************************************************
      *.PN 100000-START.                                               *
      ******************************************************************
       100000-START.

           SET INP-NO-END TO TRUE.

           OPEN INPUT  INPUT-FILE
                OUTPUT OUTPUT-FILE.


           PERFORM RETRIEVE-ENTITY.


      *
      ******************************************************************
      *.PN RETRIEVE-ENTITY.                                            *
      *  -CALLS ROUTINE QG8C9900 TO RETRIEVE ALL THE PARAMETERS        *
      *  CORRESPONDING TO THE FINANCIAL INSTITUTION                    *
      ******************************************************************
       RETRIEVE-ENTITY.
      *
           INITIALIZE VA-QBEC9900-01

           CALL CA-QB8C9900 USING VA-QBEC9900-01

           EVALUATE TRUE

               WHEN QBEC9900-88-COD-RETURN-OK

                   MOVE QBEC9900-DATA-PARAM  TO QBEC999-DATA-PARAM

               WHEN OTHER

                   INITIALIZE QRECDB2

                   MOVE CA-PROGRAM           TO DB2-DES-PGM
                   MOVE CA-QB8C9900          TO DB2-OBJECT
                   MOVE CA-A                 TO DB2-ABEND-DB2
                   MOVE QBEC9900-COD-RETURN  TO DB2-CODERR
                   MOVE QBEC9900-FILE-STATUS TO DB2-REFERENCE1
                   MOVE SPACES               TO DB2-SQLCA
                                                DB2-STATEMENT

                   PERFORM ABEND-PROGRAM-QR4CDB0

           END-EVALUATE.

      *
      ******************************************************************
      *.PN 200000-PROCESS.                                             *
      ******************************************************************
       200000-PROCESS.



           INITIALIZE RVFC007.


           PERFORM 210000-READ-RECORD.

           PERFORM
             UNTIL INP-YES-END

              PERFORM 220000-CALC-PERMISS-AND-WRITE


              INITIALIZE RVFC007

              PERFORM 210000-READ-RECORD
           END-PERFORM.

      *
      ******************************************************************
      *.PN 210000-READ-RECORD.                                         *
      ******************************************************************
       210000-READ-RECORD.


           READ INPUT-FILE INTO RVFC007

               AT END
                   SET INP-YES-END TO TRUE
               NOT AT END
                   CONTINUE
           END-READ.

      *
      ******************************************************************
      *.PN 220000-CALC-PERMISS-AND-WRITE.                              *
      ******************************************************************
       220000-CALC-PERMISS-AND-WRITE.

          EVALUATE F007-COD-PROD
               WHEN CA-DEPOSIT1
               WHEN CA-DEPOSIT2
               WHEN CA-DEPOSIT3

                   MOVE F007-AMT-FML TO F007-AMT-AVA1

               WHEN CA-LOAN

                   COMPUTE F007-AMT-AVA1 =    F007-DEBTBAL
                                            - F007-AMT-CAP
                                            - F007-AMT-ITR
                                            - F007-AMT-COM

                   IF  F007-AMT-AVA1 < 0

                       MOVE ZEROS TO F007-AMT-AVA1
                   END-IF

               WHEN OTHER

                   MOVE ZEROS          TO F007-AMT-AVA1
           END-EVALUATE.


           IF  F007-TYP-RISKST  NOT EQUAL  '0'
               MOVE  ZEROS  TO F007-AMT-AVA1
           END-IF.



           EVALUATE F007-COD-PROD
              WHEN CA-LOAN
                IF F007-AMT-AVA < F007-AMT-FML
                    COMPUTE F007-PER-RTN =
                    ((F007-AMT-FML - F007-AMT-AVA) -
                     (F007-DEBTBAL - F007-AMT-ITR - F007-AMT-COM)) /
                     (F007-AMT-FML - F007-AMT-AVA) * 100
                ELSE
                    MOVE ZEROS TO F007-PER-RTN
                END-IF

                IF  F007-PER-RTN  <  0
                    MOVE ZEROS  TO  F007-PER-RTN
                END-IF
              WHEN OTHER
                MOVE ZEROS  TO  F007-PER-RTN
           END-EVALUATE


           WRITE RE-OUTPUT FROM RVFC007.


      *
      ******************************************************************
      *.PN 300000-END.                                                 *
      * THIS PARAGRAPH CLOSES THE FILES AND TERMINATES THE PROGRAM     *
      ******************************************************************
       300000-END.

           CLOSE INPUT-FILE
                 OUTPUT-FILE

           STOP RUN.
      *
      ******************************************************************
      *.PN ABEND-PROGRAM-QR4CDB0.                                      *
      ******************************************************************
       ABEND-PROGRAM-QR4CDB0.
      *
           CALL CA-QR4CDB0 USING QRECDB2.


