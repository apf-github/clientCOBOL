
       02 QBEC999.
          05 QBEC999-DATA-PARAM.
             10 QBEC999-COD-ENTITY                   PIC X(04).
             10 QBEC999-ENTITY-DATA.
                15 QBEC999-LNG-DATA                  PIC X(01).
                15 QBEC999-COD-SHORT                 PIC X(02).
                15 QBEC999-COD-NB                    PIC X(04).
                15 QBEC999-COD-NATCC                 PIC X(03).
                15 QBEC999-COD-NATCCSHORT            PIC X(01).
                15 QBEC999-COD-OCCCTRY               PIC X(03).
                15 QBEC999-COD-OFCCSHORT             PIC X(01).
                15 QBEC999-COD-RCC                   PIC X(03).
                15 QBEC999-COD-RFCCSHORT             PIC X(01).
                15 QBEC999-COD-NRESFCC               PIC X(03).
                15 QBEC999-COD-NATCTRY               PIC X(03).
                15 QBEC999-FLG-EURCTRY               PIC X(01).
                   88 QBEC999-88-FLGYESEURCTRY       VALUE 'S'.
                   88 QBEC999-88-FLGNONEURCTRY       VALUE 'N'.
                15 QBEC999-FLG-EXCHANGE              PIC X(01).
                   88 QBEC999-88-FLGDIRCHG           VALUE 'D'.
                   88 QBEC999-88-FLGINDIRCHG         VALUE 'I'.
                15 QBEC999-LNG-OFDATA                PIC X(01).
                15 QBEC999-FLG-OFCACC                PIC X(01).
                15 QBEC999-FCCCOEXC                  PIC X(01).

                15 QBEC999-FLG-LNG1                  PIC X(01).
                15 QBEC999-FLG-LNG2                  PIC X(01).
                15 QBEC999-FREE                      PIC X(14).


