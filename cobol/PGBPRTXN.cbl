       IDENTIFICATION DIVISION.
       PROGRAM-ID. PGBPRTXN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTINPT ASSIGN TO DDCUSTIN
           ORGANIZATION IS SEQUENTIAL
      *     ORGANIZATION IS INDEXED
      *     ACCESS MODE IS SEQUENTIAL
      *     RECORD KEY IS CUSTIN-PRI-KEY
           FILE STATUS IS WS-CUSTIN-FS.

           SELECT LOANINPT ASSIGN TO DDLOANIN
           ORGANIZATION IS SEQUENTIAL
      *     ORGANIZATION IS INDEXED
      *     ACCESS MODE IS SEQUENTIAL
      *     RECORD KEY IS LOANIN-PRI-KEY
           FILE STATUS IS WS-LOANIN-FS.

           SELECT ERRFILE ASSIGN TO DDERRFIL
           ORGANIZATION IS SEQUENTIAL
           FILE STATUS IS WS-ERRFIL-FS.

       DATA DIVISION.
       FILE SECTION.
       FD CUSTINPT.
       COPY CUSTTRAN.

       FD LOANINPT.
       COPY LOANTRAN.

       FD ERRFILE.
       01 ERROR-REC.
           02 EREC-NO   PIC X(4).
           02 FILLER PIC XX VALUE '-'.
           02 ERROR-MSG  PIC X(74).

       WORKING-STORAGE SECTION.
           EXEC SQL
              INCLUDE SQLCA
           END-EXEC.

           EXEC SQL
      *        INCLUDE CUSTM_TABLE
               INCLUDE CUSTMSTR
           END-EXEC.

           EXEC SQL
               INCLUDE LOANMSTR
           END-EXEC.

           EXEC SQL
               INCLUDE LRPHIST
           END-EXEC.

       01 WS-FILE-STATUS.
          05 WS-CUSTIN-FS          PIC XX.
             88 END-OF-CUSTIN          VALUE '10'.
          05 WS-LOANIN-FS          PIC XX.
             88 END-OF-LOANIN          VALUE '10'.
          05 WS-ERRFIL-FS          PIC XX.

       01 WS-COUNTERS.
          05 WS-CUSTIN-TOTAL-RECD          PIC 9(03).
          05 WS-LOANIN-TOTAL-RECD          PIC 9(03).
          05 WS-ERRFIL-TOTAL-RECD          PIC 9(03).

       01 WS-FLAGS.
          05 WS-ERROR-FLAG                 PIC X(01).
             88  LOANNO-ALEARDY-CLOSED     VALUE '1'.
             88  ERROR-UPDATE-LOANHIST     VALUE '2'.

       01 WS-MISC-VAR.
         05  WS-CUST-NO                           PIC S9(9) COMP.
         05  WS-TEMP-LOAN-NO                      PIC X(10).
         05  WS-LOAN-NO                           PIC S9(9) USAGE COMP.
         05  WS-LOAN-OUTAMT                       PIC 9(10).
         05  WS-LOAN-TERM-YY                         PIC 9(02)V99.

         05  WS-CURR-DATE.
                 10  WS-CURT-DATE-MM     PIC X(02)       VALUE SPACES.
                 10  FILLER              PIC X(01)       VALUE '/'.
                 10  WS-CURT-DATE-DD     PIC X(02)       VALUE SPACES.
                 10  FILLER              PIC X(01)       VALUE '/'.
                 10  WS-CURT-DATE-CC     PIC X(02)       VALUE SPACES.
             10  WS-CURT-DATE-YY     PIC X(02)       VALUE SPACES.

         05  WS-TEMP-DATE.
             10  WS-TEMP-DATE-YY     PIC X(02)       VALUE SPACES.
             10  WS-TEMP-DATE-MM     PIC X(02)       VALUE SPACES.
             10  WS-TEMP-DATE-DD     PIC X(02)       VALUE SPACES.

         05  WS-NEXT-LOAN-NO         PIC S9(9)       USAGE COMP.
         05  WS-MOVE-LOAN-NO.
             10 WS-ONE-LOAN-NO       PIC 9(01).
             10 WS-TEMP1-LOAN-NO      PIC 9(09).

       PROCEDURE DIVISION.
       0000-MAIN-PROCESSING-PARA.
            INITIALIZE WS-COUNTERS
                       WS-MISC-VAR
                       WS-FILE-STATUS.

            PERFORM A1000-OPEN-FILE-PARA
               THRU A1000-EXIT

            PERFORM A2000-PROCESS-CUST-REQST
               THRU A2000-EXIT

            PERFORM A3000-PROCESS-LOAN-REQST
               THRU A3000-EXIT

            PERFORM A4000-CLOSE-FILE-PARA
               THRU A4000-EXIT.
           GOBACK.
       0000-EXIT.
            EXIT.
       A1000-OPEN-FILE-PARA.

      *      CLOSE CUSTINPT
      *      CLOSE LOANINPT
      *      CLOSE ERRFILE

            OPEN INPUT CUSTINPT
            DISPLAY 'CUSTIN FILE STATUS -',WS-CUSTIN-FS

            OPEN INPUT LOANINPT
            DISPLAY 'LOANIN FILE STATUS -',WS-LOANIN-FS

            OPEN OUTPUT ERRFILE
            DISPLAY 'ERROR FILE STATUS -',WS-ERRFIL-FS.

       A1000-EXIT.
             EXIT.
       A2000-PROCESS-CUST-REQST.
             PERFORM R1000-READ-CUST-FILE
             PERFORM A2100-TAKE-CUST-REQ UNTIL END-OF-CUSTIN.
       A2000-EXIT.
             EXIT.
       A2100-TAKE-CUST-REQ.

             EVALUATE TRUE
             WHEN CUSTIN-REQ-TYPE = '01'
                  PERFORM A2200-ADD-CUST-PARA
             WHEN CUSTIN-REQ-TYPE = '02'
                  PERFORM A2300-MOD-CUST-PARA
             WHEN CUSTIN-REQ-TYPE = '03'
                  PERFORM A2400-DEL-CUST-PARA
             END-EVALUATE.

             PERFORM R1000-READ-CUST-FILE.
       A2100-EXIT.
             EXIT.
       A2200-ADD-CUST-PARA.

           MOVE CUSTIN-CUSTM-NAME-FRSTNM  TO ACN-CUSTM-NAME-FRSTNM
           MOVE CUSTIN-CUSTM-NAME-MDLNM   TO ACN-CUSTM-NAME-MDLNM
           MOVE CUSTIN-CUSTM-NAME-LASTNM  TO ACN-CUSTM-NAME-LASTNM
           MOVE CUSTIN-CUSTM-ADDR         TO ACN-CUSTM-ADDR
           MOVE CUSTIN-CUSTM-DOB          TO ACN-CUSTM-DOB

           EXEC SQL
                INSERT INTO CUSTM_TABLE
                (
                  CUSTM_NAME_FRSTNM        ,
                  CUSTM_NAME_MDLNM         ,
                  CUSTM_NAME_LASTNM        ,
                  CUSTM_ADDR               ,
                  CUSTM_DOB
                )
                VALUES
                (
                  :ACN-CUSTM-NAME-FRSTNM   ,
                  :ACN-CUSTM-NAME-MDLNM    ,
                  :ACN-CUSTM-NAME-LASTNM   ,
                  :ACN-CUSTM-ADDR          ,
                  :ACN-CUSTM-DOB
                )
           END-EXEC.

           EVALUATE TRUE
           WHEN SQLCODE = 0
                CONTINUE
           WHEN OTHER
                DISPLAY 'CUST TABLE INSERT SQLCODE -',SQLCODE
                MOVE '0001'                            TO EREC-NO
                MOVE '***UNABLE TO INSERT CUSTOMER***' TO ERROR-MSG
                PERFORM W1000-WRITE-ERRFILE-PARA
           END-EVALUATE.

       A2200-EXIT.
             EXIT.
       A2300-MOD-CUST-PARA.
           MOVE CUSTIN-CUSTM-NO           TO ACN-CUSTM-NO
           MOVE CUSTIN-CUSTM-NAME-FRSTNM  TO ACN-CUSTM-NAME-FRSTNM
           MOVE CUSTIN-CUSTM-NAME-MDLNM   TO ACN-CUSTM-NAME-MDLNM
           MOVE CUSTIN-CUSTM-NAME-LASTNM  TO ACN-CUSTM-NAME-LASTNM
           MOVE CUSTIN-CUSTM-ADDR         TO ACN-CUSTM-ADDR
           MOVE CUSTIN-CUSTM-DOB          TO ACN-CUSTM-DOB

           EXEC SQL
                UPDATE CUSTM_TABLE   SET
                  CUSTM_NAME_FRSTNM = :ACN-CUSTM-NAME-FRSTNM,
                  CUSTM_NAME_MDLNM = :ACN-CUSTM-NAME-MDLNM,
                  CUSTM_NAME_LASTNM = :ACN-CUSTM-NAME-LASTNM,
                  CUSTM_ADDR = :ACN-CUSTM-ADDR,
                  CUSTM_DOB  = :ACN-CUSTM-DOB
                WHERE CUSTM_NO = :ACN-CUSTM-NO
          END-EXEC.

           EVALUATE TRUE
            WHEN SQLCODE = 0
                 CONTINUE
            WHEN OTHER
                 DISPLAY 'CUST TABLE UPDATE SQLCODE -',SQLCODE
           END-EVALUATE.

       A2300-EXIT.
             EXIT.
       A2400-DEL-CUST-PARA.
             MOVE CUSTIN-CUSTM-NO           TO ACN-CUSTM-NO

             EXEC SQL
                DELETE FROM CUSTM_TABLE
                WHERE CUSTM_NO = :ACN-CUSTM-NO
             END-EXEC.

             EVALUATE TRUE
             WHEN SQLCODE = 0
                  CONTINUE
             WHEN OTHER
                  DISPLAY 'CUST TABLE DELETE SQLCODE -',SQLCODE
             END-EVALUATE.

       A2400-EXIT.
             EXIT.
       A3000-PROCESS-LOAN-REQST.
             PERFORM R2000-READ-LOAN-FILE
             PERFORM A3100-TAKE-LOAN-REQ UNTIL END-OF-LOANIN.
       A3000-EXIT.
             EXIT.
       A3100-TAKE-LOAN-REQ.

             EVALUATE TRUE
              WHEN LOANIN-REQ-TYPE = '04'
                   PERFORM A3200-NEW-LOAN-PARA
              WHEN LOANIN-REQ-TYPE = '05'
                   PERFORM A3300-LOAN-REPAY-PARA
             END-EVALUATE.

             PERFORM R2000-READ-LOAN-FILE.
       A3100-EXIT.
             EXIT.
       A3200-NEW-LOAN-PARA.
      * RAGHU BELOW PARA NEED TO CHECK AND UNCOMMENT
      *      PERFORM A3210-SEARCH-LOANMSTR
             PERFORM A3220-SEARCH-CUSTNO-PARA
             PERFORM A3240-UPDATE-LOANMSTR.
       A3200-EXIT.
             EXIT.
       A3210-SEARCH-LOANMSTR.
              INITIALIZE WS-LOAN-NO
                         WS-TEMP-LOAN-NO

              MOVE LOANIN-LLOANM-CAT      TO  LLOANM-CAT
              MOVE LOANIN-LLOANM-CUST-NO  TO  LLOANM-CUST-NO

              EXEC SQL
                   SELECT LOANM_NO INTO :WS-LOAN-NO
                   FROM LOANMSTR
                   WHERE LOANM_CAT = :LLOANM-CAT AND
                         LOANM_CUST_NO = :LLOANM-CUST-NO
              END-EXEC

              EVALUATE TRUE
              WHEN SQLCODE = 0
                 MOVE WS-LOAN-NO               TO WS-TEMP-LOAN-NO
              WHEN SQLCODE = 100
                  CONTINUE
              WHEN OTHER
                  DISPLAY 'LOAN TABLE SELECT SQLCODE -',SQLCODE
              END-EVALUATE.
       A3210-EXIT.
             EXIT.
       A3220-SEARCH-CUSTNO-PARA.
                      INITIALIZE  ACN-CUSTM-NO
                                  WS-CUST-NO

               MOVE LOANIN-LLOANM-CUST-NO TO ACN-CUSTM-NO

               EXEC SQL
                   SELECT CUSTM_NO INTO :WS-CUST-NO
                   FROM CUSTM_TABLE WHERE CUSTM_NO = :ACN-CUSTM-NO
               END-EXEC

               EVALUATE TRUE
               WHEN SQLCODE = 0
                    PERFORM A3230-GENERATE-LOANNO
                       THRU A3230-EXIT
               WHEN SQLCODE = 100
                  DISPLAY ' A3220 PARA CUST NOT FOUND -',SQLCODE
               WHEN OTHER
                  DISPLAY ' A3220 PARA CUST SELECT ERR -',SQLCODE
               END-EVALUATE.
       A3220-EXIT.
             EXIT.
       A3230-GENERATE-LOANNO.
                      INITIALIZE WS-NEXT-LOAN-NO
                                 WS-MOVE-LOAN-NO.

               EXEC SQL
                   SELECT MAX (LOANM_NO) INTO :LLOANM-NO
                   FROM LOANMSTR
               END-EXEC

               EVALUATE TRUE
               WHEN SQLCODE = 0
                  COMPUTE WS-NEXT-LOAN-NO = LLOANM-NO + 1
                  MOVE WS-NEXT-LOAN-NO TO WS-TEMP1-LOAN-NO
      *RAGHU COMMENTS BELOW RETURN CODE WILL BE FOR NOT FOUND ALSO
               WHEN OTHER
                  DISPLAY ' A3230 PARA ERR GENERATING LOAN-',SQLCODE
               END-EVALUATE.
       A3230-EXIT.
             EXIT.
       A3240-UPDATE-LOANMSTR.
            MOVE LOANIN-LLOANM-CAT      TO  LLOANM-CAT
            MOVE LOANIN-LLOANM-TERM     TO  LLOANM-TERM
            MOVE LOANIN-LLOANM-MAX-AMT  TO  LLOANM-MAX-AMT
            MOVE 20                     TO  LLOANM-MIN-AGE
            MOVE 50                     TO  LLOANM-MAX-AGE
            MOVE WS-NEXT-LOAN-NO        TO  LLOANM-NO
            MOVE 'O'                    TO  LLOANM-ST
            MOVE LOANIN-LLOANM-CUST-NO  TO  LLOANM-CUST-NO

            IF LOANIN-LLOANM-CAT = 01
               MOVE 'AUTO LOAN'  TO  LLOANM-TITLE
               MOVE 11           TO  LLOANM-ROI
            END-IF

            IF LOANIN-LLOANM-CAT = 02
               MOVE 'PRSNL LOAN' TO  LLOANM-TITLE
               MOVE 14           TO  LLOANM-ROI
            END-IF

            EXEC SQL
              INSERT INTO LOANMSTR VALUES
             (:LLOANM-CAT,
              :LLOANM-NO,
              :LLOANM-TERM,
              :LLOANM-TITLE,
              :LLOANM-ROI,
              :LLOANM-MAX-AMT,
              :LLOANM-MIN-AGE,
              :LLOANM-MAX-AGE,
              :LLOANM-ST,
              :LLOANM-CUST-NO)
            END-EXEC

            EVALUATE TRUE
            WHEN SQLCODE = 0
                 CONTINUE
            WHEN OTHER
                 DISPLAY 'LOAN TABLE INSERT SQLCODE -',SQLCODE
            END-EVALUATE.
       A3240-EXIT.
             EXIT.
       A3300-LOAN-REPAY-PARA.

             PERFORM A3310-SEARCH-LOANNO-PARA

             IF LOANNO-ALEARDY-CLOSED
                CONTINUE
             ELSE
                PERFORM A3320-GET-DATE-PARA
                PERFORM A3330-INSERT-REPAYMNT-DATA
             END-IF

             IF ERROR-UPDATE-LOANHIST
                CONTINUE
             ELSE
                PERFORM A3340-UPDATE-LOANMSTR-DATA
             END-IF.

       A3300-EXIT.
             EXIT.
       A3310-SEARCH-LOANNO-PARA.
               MOVE LOANIN-LLOANM-NO  TO LLOANM-NO

               EXEC SQL
                   SELECT LOANM_CAT, LOANM_ROI, LOANM_MAX_AMT,
                       LOANM_TERM, LOANM_MIN_AGE, LOANM_MAX_AGE,
                       LOANM_ST,LOANM_CUST_NO
                   INTO :LLOANM-CAT, :LLOANM-ROI, :LLOANM-MAX-AMT,
                       :LLOANM-TERM, :LLOANM-MIN-AGE, :LLOANM-MAX-AGE,
                       :LLOANM-ST,:LLOANM-CUST-NO
                   FROM LOANMSTR WHERE LOANM_NO = :LLOANM-NO

               END-EXEC
               EVALUATE TRUE
               WHEN SQLCODE = 0
                   IF LLOANM-ST = 'C'
                      SET LOANNO-ALEARDY-CLOSED   TO TRUE
                      DISPLAY 'A3310 PARA LOAN ALREADY CLOSED -',
                      LOANIN-LLOANM-NO
                   ELSE
                      COMPUTE WS-LOAN-TERM-YY = (LLOANM-TERM/ 12)

                      COMPUTE WS-LOAN-OUTAMT =  LLOANM-MAX-AMT +
                        ((LLOANM-MAX-AMT*LLOANM-ROI*WS-LOAN-TERM-YY)/
                         100)
                   END-IF
               WHEN SQLCODE = 100
                      DISPLAY 'A3310 PARA LOAN NOT FOUND',
                      LOANIN-LLOANM-NO
               WHEN OTHER
                      DISPLAY ' A3310 PARA ERROR -', SQLCODE
               END-EVALUATE.
       A3310-EXIT.
             EXIT.
       A3320-GET-DATE-PARA.
             ACCEPT WS-TEMP-DATE FROM DATE

             MOVE WS-TEMP-DATE-MM  TO WS-CURT-DATE-MM
             MOVE WS-TEMP-DATE-DD  TO WS-CURT-DATE-DD
             MOVE '20'             TO WS-CURT-DATE-CC
             MOVE WS-TEMP-DATE-YY  TO WS-CURT-DATE-YY.
       A3220-EXIT.
             EXIT.
       A3330-INSERT-REPAYMNT-DATA.
               MOVE LOANIN-LLOANM-CAT     TO RLOANRPH-CAT
               MOVE LOANIN-LLOANM-NO      TO RLOANRPH-NO
               MOVE WS-CURR-DATE          TO RLOANRPH-DATE
               MOVE LOANIN-LLOANM-MAX-AMT TO RLOANRPH-RP-AMT
               MOVE WS-LOAN-OUTAMT        TO RLOANRPH-OS-AMT

               EXEC SQL
                   INSERT INTO LOANRPHST VALUES
                   (:RLOANRPH-CAT, :RLOANRPH-NO, :RLOANRPH-DATE,
                    :RLOANRPH-RP-AMT, :RLOANRPH-OS-AMT)
               END-EXEC.

               EVALUATE TRUE
               WHEN SQLCODE = 0
                    CONTINUE
               WHEN OTHER
                    SET ERROR-UPDATE-LOANHIST   TO TRUE
                    DISPLAY ' A3330 PARA SQLERROR - ',SQLCODE
               END-EVALUATE.

       A3330-EXIT.
             EXIT.
       A3340-UPDATE-LOANMSTR-DATA.
               MOVE LOANIN-LLOANM-NO     TO LLOANM-NO
               MOVE 'C'                  TO LLOANM-ST

               EXEC SQL
                  UPDATE LOANMSTR
                  SET LOANM_ST   = :LLOANM-ST
                  WHERE LOANM_NO = :LLOANM-NO
               END-EXEC.

               EVALUATE TRUE
               WHEN SQLCODE = 0
                    CONTINUE
               WHEN OTHER
                    DISPLAY ' A3340 PARA SQLERROR - ',SQLCODE
      *             PERFORM W1000-WRITE-ERRFILE-PARA
               END-EVALUATE.

       A3340-EXIT.
             EXIT.
       A4000-CLOSE-FILE-PARA.

            CLOSE CUSTINPT
            CLOSE LOANINPT
            CLOSE ERRFILE.

       A4000-EXIT.
             EXIT.
       R1000-READ-CUST-FILE.
             READ CUSTINPT
             END-READ

             EVALUATE TRUE
             WHEN WS-CUSTIN-FS = '00'
              COMPUTE WS-CUSTIN-TOTAL-RECD = WS-CUSTIN-TOTAL-RECD + 1
             WHEN WS-CUSTIN-FS = '10'
              SET END-OF-CUSTIN TO TRUE
             WHEN OTHER
              SET END-OF-CUSTIN TO TRUE
              DISPLAY 'CUST TRAN FILE ERROR -',WS-CUSTIN-FS
              MOVE 'R001'                            TO EREC-NO
              MOVE '***CUST FILE READ ERROR***'      TO ERROR-MSG
              PERFORM W1000-WRITE-ERRFILE-PARA
             END-EVALUATE.
       R1000-EXIT.
             EXIT.
       R2000-READ-LOAN-FILE.
             READ LOANINPT
             AT END SET END-OF-LOANIN TO TRUE
             END-READ

             EVALUATE TRUE
             WHEN WS-LOANIN-FS = '00'
              COMPUTE WS-LOANIN-TOTAL-RECD = WS-LOANIN-TOTAL-RECD + 1
             WHEN WS-LOANIN-FS = '10'
              SET END-OF-LOANIN TO TRUE
             WHEN OTHER
              SET END-OF-LOANIN TO TRUE
              DISPLAY 'LOAN TRAN FILE ERROR -',WS-LOANIN-FS
              MOVE 'R002'                            TO EREC-NO
              MOVE '***LOAN FILE READ ERROR***'      TO ERROR-MSG
              PERFORM W1000-WRITE-ERRFILE-PARA
             END-EVALUATE.
       R2000-EXIT.
             EXIT.
       W1000-WRITE-ERRFILE-PARA.
             WRITE ERROR-REC.
       W1000-EXIT.
             EXIT.
       END PROGRAM PGBPRTXN.
