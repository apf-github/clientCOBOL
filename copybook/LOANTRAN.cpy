       01 LOANINPT-FILE.
      *REQUEST TYPE CAN BE 04-NEW LOAN/05-REPAYMENT
           05 LOANIN-PRI-KEY.
              10 LOANIN-REQ-TYPE          PIC X(02).
              10 LOANIN-LLOANM-NO         PIC X(10).
           05 LOANIN-LLOANM-CAT           PIC X(2).
           05 LOANIN-LLOANM-TERM          PIC X(03).

           05 LOANIN-LLOANM-MAX-AMT       PIC 9(10).

           05 LOANIN-LLOANM-CUST-NO       PIC X(10).
