
       02 QBEC9900.
          05 QBEC9900-OUTPUTFLD.
             10 QBEC9900-DATA-PARAM.
                15 QBEC9900-COD-ENTITY               PIC X(04).
                15 QBEC9900-LNG-DATA                 PIC X(01).
                15 QBEC9900-COD-SHORT                PIC X(02).
                15 QBEC9900-COD-NB                   PIC X(04).
                15 QBEC9900-COD-NATCC                PIC X(03).
                15 QBEC9900-COD-NATCCSHORT           PIC X(01).
                15 QBEC9900-COD-OCCCTRY              PIC X(03).
                15 QBEC9900-COD-OFCCSHORT            PIC X(01).
                15 QBEC9900-COD-RCC                  PIC X(03).
                15 QBEC9900-COD-RFCCSHORT            PIC X(01).
                15 QBEC9900-COD-NRESFCC              PIC X(03).
                15 QBEC9900-COD-NATCTRY              PIC X(03).
                15 QBEC9900-FLG-EURCTRY              PIC X(01).
                   88 QBEC9900-88-FLGYESEURCTRY      VALUE 'S'.
                   88 QBEC9900-88-FLGNONEURCTRY      VALUE 'N'.
                15 QBEC9900-FLG-EXCHANGE             PIC X(01).
                   88 QBEC9900-88-FLGDIRCHG          VALUE 'D'.
                   88 QBEC9900-88-FLGINDIRCHG        VALUE 'I'.
                15 QBEC9900-LNG-OFDATA               PIC X(01).
                15 QBEC9900-FLG-OFCACC               PIC X(01).
                15 QBEC9900-FCCCOEXC                 PIC X(01).

                15 QBEC9900-FLG-LNG1                 PIC X(01).
                15 QBEC9900-FLG-LNG2                 PIC X(01).
                15 QBEC9900-FREE                     PIC X(14).

             10 QBEC9900-DATA-DESCRIP.
                15 QBEC9900-DES-ENTITY               PIC X(40).
                15 QBEC9900-DES-NB                   PIC X(40).
                15 QBEC9900-DES-NATCC                PIC X(20).
                15 QBEC9900-SDE-FCC                  PIC X(03).
                15 QBEC9900-DES-OCC                  PIC X(20).
                15 QBEC9900-SDES-ORIFCC              PIC X(03).
                15 QBEC9900-DES-REFFCC               PIC X(20).
                15 QBEC9900-DES-NRESFCC              PIC X(20).
                15 QBEC9900-DES-NATCTRY              PIC X(40).
                15 QBEC9900-SDE-ENTITY               PIC X(10).
             10 QBEC9900-FLD-DES-LNGS                OCCURS 3 TIMES.
                15 QBEC9900-LNG-DATA-0               PIC X(01).
                15 QBEC9900-DATA-DESCRIP-0.
                   20 QBEC9900-DES-ENTITY-0          PIC X(40).
                   20 QBEC9900-DES-NB-0              PIC X(40).
                   20 QBEC9900-DES-NATCC-0           PIC X(20).
                   20 QBEC9900-SDE-FCC-0             PIC X(03).
                   20 QBEC9900-DES-OCC-0             PIC X(20).
                   20 QBEC9900-SDES-ORIFCC-0         PIC X(03).
                   20 QBEC9900-DES-REFFCC-0          PIC X(20).
                   20 QBEC9900-DES-NRESFCC-0         PIC X(20).
                   20 QBEC9900-DES-NATCTRY-0         PIC X(40).
                   20 QBEC9900-SDE-ENTITY-0          PIC X(10).
          05 QBEC9900-RETURN-DATA.
             10 QBEC9900-COD-RETURN                  PIC X(02).
                88 QBEC9900-88-COD-RETURN-OK         VALUE '00'.
                88 QBEC9900-88-COD-RETURN-NOTFND     VALUE '10'.
                88 QBEC9900-88-COD-RETURN-OTHERS     VALUE '99'.
             10 QBEC9900-FILE-STATUS                 PIC X(02).

