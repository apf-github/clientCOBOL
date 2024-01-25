

       01 QRECDB2.
          05 DB2-CODERR                    PIC X(7).
          05 DB2-SQLCA                     PIC X(136).
          05 DB2-DES-PGM                   PIC X(8).
          05 DB2-OBJECT                    PIC X(8).
          05 DB2-STATEMENT                 PIC X(8).
          05 DB2-ABEND                     PIC X(1).
          05 DB2-ABEND-DB2                 PIC X(1).
          05 DB2-REFERENCE1                PIC X(120).
          05 DB2-QR4CDB0                   PIC X(8) VALUE 'QR4CDB0'.

          05 DB2-SW-SQLCA                  PIC X(1).
             88 DB2-SW-SQLCA-ALL                    VALUE '0', ' '.
             88 DB2-SW-SQLCA-PART                   VALUE '1'.
          05 DB2-SQLCODE                   PIC S9(9) COMP.
          05 DB2-SQLERRM                   PIC X(70).

          05 DB2-RETURN-CODE               PIC S9(04) COMP.
          05 FILLER                        PIC X(35).

       01 QRECDB2-RE REDEFINES QRECDB2.
          05 FILLER                        PIC X(7).
          05 FILLER                        PIC X(136).
          05 DB2-PGRNAME                   PIC X(8).
          05 DB2-OBJETO                    PIC X(8).
          05 DB2-SENTENCIA                 PIC X(8).
          05 FILLER                        PIC X(1).
          05 FILLER                        PIC X(1).
          05 DB2-REFERENCIA                PIC X(120).
          05 FILLER                        PIC X(8).
          05 FILLER                        PIC X(112).

