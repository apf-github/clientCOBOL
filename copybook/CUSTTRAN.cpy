       01 CUSTINPT-FILE.
      * REQUEST TYPE CAN BE 01-ADD/02-MOD/03-DEL
           05 CUSTIN-PRI-KEY.
              10 CUSTIN-REQ-TYPE                PIC X(02).
              10 CUSTIN-CUSTM-NO                PIC X(10).
           05 CUSTIN-CUSTM-NAME-FRSTNM          PIC X(15).
           05 CUSTIN-CUSTM-NAME-MDLNM           PIC X(8).
           05 CUSTIN-CUSTM-NAME-LASTNM          PIC X(15).
           05 CUSTIN-CUSTM-ADDR                 PIC X(35).
           05 CUSTIN-CUSTM-DOB                  PIC X(10).
