       IDENTIFICATION DIVISION.
       PROGRAM-ID.    NPW08P.
      *------------------------------------------------------*
      *            D B 2   A R E A S                         *
      *------------------------------------------------------*
EXPAND*    EXEC SQL INCLUDE NPW08R
EXPAND*         END-EXEC.
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NPW08R
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *                                                               *
      *  *******  **   **  **  *****  **  **                          *
      *  **   **  **   **  **  **     ** **          PRODOTTO         *
      *  **   **  **   **  **  **     ***       ANTICIPO DOCUMENTI    *
      *  ** * **  **   **  **  **     ** **                           *
      *  *******  *******  **  *****  **  **                          *
      *       *                                  INFOSER S.R.L.       *
      *                                                               *
      *---------------------------------------------------------------*
      *  NPW08R    ROUTINE GENERALIZZATA CONTROLLO DATE               *
      *---------------------------------------------------------------*
      *------------------- M O D I F I C H E  ------------------------*
      *  991018 MODIFICATO PER L'ERRORE DEL FATTO E01237              *
E01790* 020701 INSERITA IMPLEMENTAZIONE E01790 PER NUOVE DATE         *
E01790*        FESTIVE: 02 GIUGNO E 31 DICEMBRE                       *
XMBI01* 240402 ELIMINATA GESTIONE DATA FESTIVA 31 DICEMBRE            *
XSLU  * 22/02/2008 - CORRETTO PASQUETTA 2008                          *
FES17 * 020211 GESTIONE DATA FESTIVA 17 MARZO 2011                    *
      *---------------------------------------------------------------*
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY NPG01RC.
       COPY NPG03RC.
       COPY NPW08RC.
E01237 01  WKS-GG-FEB     PIC 9(2).
       01  COM-DATA-GGMMAAAA.
           05  COM-GG     PIC 9(2).
           05  COM-MM     PIC 9(2).
           05  COM-SE     PIC 9(2).
           05  COM-AA     PIC 9(2).
       01  COM-DATA8.
           05  COM-DATA2  PIC 9(2).
           05  COM-DATA6  PIC 9(6).
       01  COM-DATA8-RED  REDEFINES COM-DATA8 PIC 9(008).
       01  W08-RDATA-PIC  PIC 9(8)  VALUE ZEROES.
       01  W08-RGGG-PIC   PIC 9(8)  VALUE ZEROES.

       01  KOM1.
           03  KOM1-1     PIC 9.
           03  KOM1-2     PIC 9.
           03  KOM1-3     PIC 9.
           03  KOM1-4     PIC 9.
           03  KOM1-5     PIC 9.
           03  KOM1-6     PIC 9.
       01  KOM1-N REDEFINES KOM1      PIC 9(6).
       01  KOM2.
           03  KOM2-1     PIC 9.
           03  KOM2-2     PIC 9.
           03  KOM2-3     PIC 9.
           03  KOM2-4     PIC 9.
           03  KOM2-5     PIC 9.
           03  KOM2-6     PIC 9.
       01  KOM2-N REDEFINES KOM2      PIC 9(6).

       01  ANNO-COST         PIC 9(5)  COMP-3   VALUE 61.
       01  GGANNO            PIC 9(5)  COMP-3   VALUE 0.
       01  CGGG              PIC 9(5)  COMP-3   VALUE 0.
       01  IND-GGPIU         PIC 9(5)  COMP-3   VALUE 0.
       01  REST              PIC 9(5)  COMP-3   VALUE 0.
       01  QUOZ              PIC 9(5)  COMP-3   VALUE 0.
       01  SW-ERR            PIC 9              VALUE 0.
       01  SW-UNO            PIC 9              VALUE 0.
       01  DATACIVI-PIC      PIC 9(8)           VALUE 0.
       01  DATACIVI REDEFINES DATACIVI-PIC.
E01964     03  CIVISA-X.                                                00800000
E01964        05 CIVISE      PIC 99.                                    00810000
E01964        05 CIVIAA      PIC 99.                                    00820000
E01964     03  CIVISA REDEFINES CIVISA-X PIC 9(004).                    00830000
           03  CIVIMM        PIC 99.
           03  CIVIGG        PIC 99.
       01  CIVIAA3           PIC 999            VALUE 0.
       01  CIVIAA3P REDEFINES CIVIAA3.
           03  CIVIAA3-1     PIC 9.
           03  CIVIAA3-2     PIC 99.
       01  COM-D-GMA         PIC 9(8).
       01  FILLER REDEFINES  COM-D-GMA.
           03 COM-D-GG       PIC 99.
           03 COM-D-MM       PIC 99.
           03 COM-D-SE       PIC 99.
           03 COM-D-AA       PIC 99.
       01  COM-DIVIDE        PIC S9(15) COMP-3.
       01  COM-REST          PIC S9(15) COMP-3.
       01  DATA-COMP         PIC X(8)           VALUE SPACES.
       01  DATA-COMP1 REDEFINES DATA-COMP       PIC 9(8).
       01  DATA-CIVI-PIC     PIC 9(7)           VALUE 0.
       01  ORA-COMP          PIC X(6)           VALUE SPACES.
       01  ORA-COMP1 REDEFINES ORA-COMP         PIC 9(6).
       01  DIF-DAT1          PIC 9(8)           VALUE 0.
       01  DIF-DAT2          PIC 9(8)           VALUE 0.
       01  COMDTOU           PIC 9(8).
       01  RCOMDTOU REDEFINES COMDTOU.
      *                         /* X PARM.W08-RTIPO = '7','8' */
           03      COMSEOU   PIC   99.
           03      COMAAOU   PIC   99.
           03      COMMMOU   PIC   99.
           03      COMGGOU   PIC   99.
       01  AMG6              PIC 9(6).
       01  AMG5              PIC 9(6).
       01  AMG               PIC 9(8).
       01  AMG1 REDEFINES AMG.
           03      SE        PIC 99.
           03      AA        PIC 99.
           03      MM        PIC 99.
           03      GG        PIC 99.
       01  AMG2 REDEFINES AMG.
           03      ANNO      PIC 9999.
           03      FILLER    PIC 99.
           03      FILLER    PIC 99.
       01  AAGGG             PIC 9(5).
       01  AAGGG1 REDEFINES AAGGG.
           03      AA        PIC 99.
           03      GGG       PIC 999.
       01  SWFM              PIC X.
      * INIZIO MOD. PIER ****
       01  SWSE              PIC X VALUE SPACE.
      * FINE   MOD. PIER ****
       01  AA1               PIC 9(5) COMP-3.
       01  MM1               PIC 9(5) COMP-3.
       01  COMMMGG.
           05 COMMMGG-MM     PIC 99.
           05 COMMMGG-GG     PIC 99.
      *                     /* X PARM.W08-RTIPO = '9' */

       01  CAAR.
           02 CSEC                 PIC 9(2).
           02 CANNO                PIC 9(2).
       01  CAA REDEFINES CAAR      PIC 9(4).

       01  C1                PIC 9(4) COMP  VALUE ZEROES.
       01  C2                PIC 9(4) COMP  VALUE ZEROES.
       01  IND               PIC 9(2) VALUE ZEROES.
E01964*01  IND-NS            PIC 9(2) VALUE ZEROES.                     01470000
       01  DAAR              PIC 9999 VALUE 1966.
       01  N-GG              PIC 9(6) COMP  VALUE ZEROES.
       01  K                 PIC 9(4) COMP.
       01  I                 PIC 9(4) COMP.
       01  J                 PIC 9(4) COMP.
       01  L                 PIC 9(4) COMP.
       01  M                 PIC 9(4) COMP.
       01  N                 PIC 9(4) COMP.
       01  INDAA             PIC 9(4) COMP         VALUE 0.
       01  INDMM             PIC 9(4) COMP         VALUE 0.

       01  ANATAB-1.
           03  FILL01        PIC 99 VALUE 31.
           03  FILL02        PIC 99 VALUE 28.
           03  FILL03        PIC 99 VALUE 31.
           03  FILL04        PIC 99 VALUE 30.
           03  FILL05        PIC 99 VALUE 31.
           03  FILL06        PIC 99 VALUE 30.
           03  FILL07        PIC 99 VALUE 31.
           03  FILL08        PIC 99 VALUE 31.
           03  FILL09        PIC 99 VALUE 30.
           03  FILL10        PIC 99 VALUE 31.
           03  FILL11        PIC 99 VALUE 30.
           03  FILL12        PIC 99 VALUE 31.
       01  ANATAB-2 REDEFINES ANATAB-1.
           03  ANATAB-3  OCCURS   12.
               05  ANATABMES PIC 99.

       01  TABCOM-1.
           03  FILL01        PIC 999 VALUE 000.
           03  FILL02        PIC 999 VALUE 030.
           03  FILL03        PIC 999 VALUE 060.
           03  FILL04        PIC 999 VALUE 090.
           03  FILL05        PIC 999 VALUE 120.
           03  FILL06        PIC 999 VALUE 150.
           03  FILL07        PIC 999 VALUE 180.
           03  FILL08        PIC 999 VALUE 210.
           03  FILL09        PIC 999 VALUE 240.
           03  FILL10        PIC 999 VALUE 270.
           03  FILL11        PIC 999 VALUE 300.
           03  FILL12        PIC 999 VALUE 330.
       01  TABCOM-2 REDEFINES TABCOM-1.
           03  TABCOM-3  OCCURS   12.
               05  TABCOM    PIC 999.

       01  TABAA-1.
      *                        /* NUM. GG INTERCORRENTI TRA     */
      *                        /* 1.1.61 E FINE ANNO INDICATO   */
           03  FILLER        PIC 99999 VALUE   365.
      *                                            /* FINE 61   */
           03  FILLER        PIC 99999 VALUE   730.
      *                                            /* FINE 62   */
           03  FILLER        PIC 99999 VALUE  1095.
      *                                            /* FINE 63   */
           03  FILLER        PIC 99999 VALUE  1461.
      *                                            /* FINE 64   */
           03  FILLER        PIC 99999 VALUE  1826.
      *                                            /* FINE 65   */
           03  FILLER        PIC 99999 VALUE  2191.
      *                                            /* FINE 66   */
           03  FILLER        PIC 99999 VALUE  2556.
      *                                            /* FINE 67   */
           03  FILLER        PIC 99999 VALUE  2922.
      *                                            /* FINE 68   */
           03  FILLER        PIC 99999 VALUE  3287.
      *                                            /* FINE 69   */
           03  FILLER        PIC 99999 VALUE  3652.
      *                                            /* FINE 70   */
           03  FILLER        PIC 99999 VALUE  4017.
      *                                            /* FINE 71   */
           03  FILLER        PIC 99999 VALUE  4383.
      *                                            /* FINE 72   */
           03  FILLER        PIC 99999 VALUE  4748.
      *                                            /* FINE 73   */
           03  FILLER        PIC 99999 VALUE  5113.
      *                                            /* FINE 74   */
           03  FILLER        PIC 99999 VALUE  5478.
      *                                            /* FINE 75   */
           03  FILLER        PIC 99999 VALUE  5844.
      *                                            /* FINE 76   */
           03  FILLER        PIC 99999 VALUE  6209.
      *                                            /* FINE 77   */
           03  FILLER        PIC 99999 VALUE  6574.
      *                                            /* FINE 78   */
           03  FILLER        PIC 99999 VALUE  6939.
      *                                            /* FINE 79   */
           03  FILLER        PIC 99999 VALUE  7305.
      *                                            /* FINE 80   */
           03  FILLER        PIC 99999 VALUE  7670.
      *                                            /* FINE 81   */
           03  FILLER        PIC 99999 VALUE  8035.
      *                                            /* FINE 82   */
           03  FILLER        PIC 99999 VALUE  8400.
      *                                            /* FINE 83   */
           03  FILLER        PIC 99999 VALUE  8766.
      *                                            /* FINE 84   */
           03  FILLER        PIC 99999 VALUE  9131.
      *                                            /* FINE 85   */
           03  FILLER        PIC 99999 VALUE  9496.
      *                                            /* FINE 86   */
           03  FILLER        PIC 99999 VALUE  9861.
      *                                            /* FINE 87   */
           03  FILLER        PIC 99999 VALUE 10227.
      *                                            /* FINE 88   */
           03  FILLER        PIC 99999 VALUE 10592.
      *                                            /* FINE 89   */
           03  FILLER        PIC 99999 VALUE 10957.
      *                                            /* FINE 90   */
           03  FILLER        PIC 99999 VALUE 11322.
      *                                            /* FINE 91   */
           03  FILLER        PIC 99999 VALUE 11688.
      *                                            /* FINE 92   */
           03  FILLER        PIC 99999 VALUE 12053.
      *                                            /* FINE 93   */
           03  FILLER        PIC 99999 VALUE 12418.
      *                                            /* FINE 94   */
           03  FILLER        PIC 99999 VALUE 12783.
      *                                            /* FINE 95   */
           03  FILLER        PIC 99999 VALUE 13149.
      *                                            /* FINE 96   */
           03  FILLER        PIC 99999 VALUE 13514.
      *                                            /* FINE 97   */
           03  FILLER        PIC 99999 VALUE 13879.
      *                                            /* FINE 98   */
           03  FILLER        PIC 99999 VALUE 14244.
      *                                            /* FINE 99   */
      *   CORREZIONE GIORNI DI PIER                     *****
           03  FILLER        PIC 99999 VALUE 14610.
      *                                            /* FINE 2000 */
           03  FILLER        PIC 99999 VALUE 14975.
      *                                            /* FINE 2001 */
           03  FILLER        PIC 99999 VALUE 15340.
      *                                            /* FINE 2002 */
           03  FILLER        PIC 99999 VALUE 15705.
      *                                            /* FINE 2003 */
           03  FILLER        PIC 99999 VALUE 16071.
      *                                            /* FINE 2004 */
           03  FILLER        PIC 99999 VALUE 16436.
      *                                            /* FINE 2005 */
           03  FILLER        PIC 99999 VALUE 16801.
      *                                            /* FINE 2006 */
           03  FILLER        PIC 99999 VALUE 17166.
      *                                            /* FINE 2007 */
           03  FILLER        PIC 99999 VALUE 17532.
      *                                            /* FINE 2008 */
           03  FILLER        PIC 99999 VALUE 17897.
      *                                            /* FINE 2009 */
           03  FILLER        PIC 99999 VALUE 18262.
      *                                            /* FINE 2010 */
           03  FILLER        PIC 99999 VALUE 18627.
      *                                            /* FINE 2011 */
           03  FILLER        PIC 99999 VALUE 18993.
      *                                            /* FINE 2012 */
           03  FILLER        PIC 99999 VALUE 19358.
      *                                            /* FINE 2013 */
           03  FILLER        PIC 99999 VALUE 19723.
      *                                            /* FINE 2014 */
           03  FILLER        PIC 99999 VALUE 20088.
      *                                            /* FINE 2015 */
           03  FILLER        PIC 99999 VALUE 20454.
      *                                            /* FINE 2016 */
           03  FILLER        PIC 99999 VALUE 20819.
      *                                            /* FINE 2017 */
           03  FILLER        PIC 99999 VALUE 21184.
      *                                            /* FINE 2018 */
           03  FILLER        PIC 99999 VALUE 21549.
      *                                            /* FINE 2019 */
           03  FILLER        PIC 99999 VALUE 21915.
      *                                            /* FINE 2020 */
           03  FILLER        PIC 99999 VALUE 22280.
      *                                            /* FINE 2021 */
           03  FILLER        PIC 99999 VALUE 22645.
      *                                            /* FINE 2022 */
           03  FILLER        PIC 99999 VALUE 23010.
      *                                            /* FINE 2023 */
           03  FILLER        PIC 99999 VALUE 23376.
      *                                            /* FINE 2024 */
           03  FILLER        PIC 99999 VALUE 23741.
      *                                            /* FINE 2025 */
           03  FILLER        PIC 99999 VALUE 24106.
      *                                            /* FINE 2026 */
           03  FILLER        PIC 99999 VALUE 24464.
      *                                            /* FINE 2027 */
           03  FILLER        PIC 99999 VALUE 24829.
      *                                            /* FINE 2028 */
           03  FILLER        PIC 99999 VALUE 25202.
      *                                            /* FINE 2029 */
           03  FILLER        PIC 99999 VALUE 25567.
      *                                            /* FINE 2030 */
           03  FILLER        PIC 99999 VALUE 25932.
      *                                            /* FINE 2031 */
           03  FILLER        PIC 99999 VALUE 26298.
      *                                            /* FINE 2032 */
           03  FILLER        PIC 99999 VALUE 26663.
      *                                            /* FINE 2033 */
           03  FILLER        PIC 99999 VALUE 27028.
      *                                            /* FINE 2034 */
           03  FILLER        PIC 99999 VALUE 27393.
      *                                            /* FINE 2035 */
           03  FILLER        PIC 99999 VALUE 27759.
      *                                            /* FINE 2036 */
           03  FILLER        PIC 99999 VALUE 28124.
      *                                            /* FINE 2037 */
           03  FILLER        PIC 99999 VALUE 28489.
      *                                            /* FINE 2038 */
           03  FILLER        PIC 99999 VALUE 28854.
      *                                            /* FINE 2039 */
           03  FILLER        PIC 99999 VALUE 29220.
      *                                            /* FINE 2040 */
           03  FILLER        PIC 99999 VALUE 29585.
      *                                            /* FINE 2041 */
           03  FILLER        PIC 99999 VALUE 29950.
      *                                            /* FINE 2042 */
           03  FILLER        PIC 99999 VALUE 30315.
      *                                            /* FINE 2043 */
           03  FILLER        PIC 99999 VALUE 30681.
      *                                            /* FINE 2044 */
           03  FILLER        PIC 99999 VALUE 31046.
      *                                            /* FINE 2045 */
           03  FILLER        PIC 99999 VALUE 31411.
      *                                            /* FINE 2046 */
           03  FILLER        PIC 99999 VALUE 31776.
      *                                            /* FINE 2047 */
           03  FILLER        PIC 99999 VALUE 32142.
      *                                            /* FINE 2048 */
           03  FILLER        PIC 99999 VALUE 32507.
      *                                            /* FINE 2049 */
           03  FILLER        PIC 99999 VALUE 32872.
      *                                             /* FINE 2050 */
E01964     03  FILLER        PIC 99999 VALUE 33237.                     03770000
      *                                            /* FINE 2051 */      03780000
E01964     03  FILLER        PIC 99999 VALUE 33603.                     03790000
      *                                            /* FINE 2052 */      03800000
E01964     03  FILLER        PIC 99999 VALUE 33968.                     03810000
      *                                            /* FINE 2053 */      03820000
E01964     03  FILLER        PIC 99999 VALUE 34333.                     03830000
      *                                            /* FINE 2054 */      03840000
E01964     03  FILLER        PIC 99999 VALUE 34698.                     03850000
      *                                            /* FINE 2055 */      03860000
E01964     03  FILLER        PIC 99999 VALUE 35064.                     03870000
      *                                            /* FINE 2056 */      03880000
E01964     03  FILLER        PIC 99999 VALUE 35429.                     03890000
      *                                            /* FINE 2057 */      03900000
E01964     03  FILLER        PIC 99999 VALUE 35794.                     03910000
      *                                            /* FINE 2058 */      03920000
E01964     03  FILLER        PIC 99999 VALUE 36159.                     03930000
      *                                            /* FINE 2059 */      03940000
E01964     03  FILLER        PIC 99999 VALUE 36525.                     03950000
      *                                            /* FINE 2060 */      03960000
E01964     03  FILLER        PIC 99999 VALUE 36890.                     03970000
      *                                            /* FINE 2061 */      03980000
E01964     03  FILLER        PIC 99999 VALUE 37255.                     03990000
      *                                            /* FINE 2062 */      04000000
E01964     03  FILLER        PIC 99999 VALUE 37620.                     04010000
      *                                            /* FINE 2063 */      04020000
E01964     03  FILLER        PIC 99999 VALUE 37986.                     04030000
      *                                            /* FINE 2064 */      04040000
E01964     03  FILLER        PIC 99999 VALUE 38351.                     04050000
      *                                            /* FINE 2065 */      04060000
E01964     03  FILLER        PIC 99999 VALUE 38716.                     04070000
      *                                            /* FINE 2066 */      04080000
E01964     03  FILLER        PIC 99999 VALUE 39081.                     04090000
      *                                            /* FINE 2067 */      04100000
E01964     03  FILLER        PIC 99999 VALUE 39447.                     04110000
      *                                            /* FINE 2068 */      04120000
E01964     03  FILLER        PIC 99999 VALUE 39812.                     04130000
      *                                            /* FINE 2069 */      04140000
E01964     03  FILLER        PIC 99999 VALUE 40177.                     04150000
      *                                            /* FINE 2070 */      04160000
E01964     03  FILLER        PIC 99999 VALUE 40542.                     04170000
      *                                            /* FINE 2071 */      04180000
E01964     03  FILLER        PIC 99999 VALUE 40908.                     04190000
      *                                            /* FINE 2072 */      04200000
E01964     03  FILLER        PIC 99999 VALUE 41273.                     04210000
      *                                            /* FINE 2073 */      04220000
E01964     03  FILLER        PIC 99999 VALUE 41638.                     04230000
      *                                            /* FINE 2074 */      04240000
E01964     03  FILLER        PIC 99999 VALUE 42003.                     04250000
      *                                            /* FINE 2075 */      04260000
E01964     03  FILLER        PIC 99999 VALUE 42369.                     04270000
      *                                            /* FINE 2076 */      04280000
E01964     03  FILLER        PIC 99999 VALUE 42734.                     04290000
      *                                            /* FINE 2077 */      04300000
E01964     03  FILLER        PIC 99999 VALUE 43099.                     04310000
      *                                            /* FINE 2078 */      04320000
E01964     03  FILLER        PIC 99999 VALUE 43464.                     04330000
      *                                            /* FINE 2079 */      04340000
E01964     03  FILLER        PIC 99999 VALUE 43830.                     04350000
      *                                            /* FINE 2080 */      04360000
E01964     03  FILLER        PIC 99999 VALUE 44195.                     04370000
      *                                            /* FINE 2081 */      04380000
E01964     03  FILLER        PIC 99999 VALUE 44560.                     04390000
      *                                            /* FINE 2082 */      04400000
E01964     03  FILLER        PIC 99999 VALUE 44925.                     04410000
      *                                            /* FINE 2083 */      04420000
E01964     03  FILLER        PIC 99999 VALUE 45291.                     04430000
      *                                            /* FINE 2084 */      04440000
E01964     03  FILLER        PIC 99999 VALUE 45656.                     04450000
      *                                            /* FINE 2085 */      04460000
E01964     03  FILLER        PIC 99999 VALUE 46021.                     04470000
      *                                            /* FINE 2086 */      04480000
E01964     03  FILLER        PIC 99999 VALUE 46386.                     04490000
      *                                            /* FINE 2087 */      04500000
E01964     03  FILLER        PIC 99999 VALUE 46752.                     04510000
      *                                            /* FINE 2088 */      04520000
E01964     03  FILLER        PIC 99999 VALUE 47117.                     04530000
      *                                            /* FINE 2089 */      04540000
E01964     03  FILLER        PIC 99999 VALUE 47482.                     04550000
      *                                            /* FINE 2090 */      04560000
E01964     03  FILLER        PIC 99999 VALUE 47847.                     04570000
      *                                            /* FINE 2091 */      04580000
E01964     03  FILLER        PIC 99999 VALUE 48213.                     04590000
      *                                            /* FINE 2092 */      04600000
E01964     03  FILLER        PIC 99999 VALUE 48578.                     04610000
      *                                            /* FINE 2093 */      04620000
E01964     03  FILLER        PIC 99999 VALUE 48943.                     04630000
      *                                            /* FINE 2094 */      04640000
E01964     03  FILLER        PIC 99999 VALUE 49308.                     04650000
      *                                            /* FINE 2095 */      04660000
E01964     03  FILLER        PIC 99999 VALUE 49674.                     04670000
      *                                            /* FINE 2096 */      04680000
E01964     03  FILLER        PIC 99999 VALUE 50039.                     04690000
      *                                            /* FINE 2097 */      04700000
E01964     03  FILLER        PIC 99999 VALUE 50404.                     04710000
      *                                            /* FINE 2098 */      04720000
E01964     03  FILLER        PIC 99999 VALUE 50769.                     04730000
      *                                            /* FINE 2099 */      04740000
       01  TABAA-2 REDEFINES TABAA-1.
E01964*    03  TABAA-3  OCCURS   90.                                    04770000
E01964     03  TABAA-3  OCCURS  139.                                    04780000
               05  TABAA      PIC 9(5).
       01  TABBIS-1.
      *                              /* INDICE PER TABMM          */
      *                              /*   0 = ANNO NORMALE        */
      *                              /*  12 = ANNO BISESTILE      */
      *                              /*  PER OTTENERE L'INDICE    */
      *                              /*  SOMMARE IL MESE A QUESTI */
      *                              /*  VALORI                   */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      1992 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      1996 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *  IL 2000     E' BISESTILE    MOD. PIER                    */
      *                                                      2000 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2004 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2004 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2008 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2012 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2016 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2020 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2024 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2028 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2032 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2036 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2040 */
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE          12.
      *                                                      2044 */    05910000
           03  FILLER    PIC 99      VALUE           0.
           03  FILLER    PIC 99      VALUE           0.                 05930000
E01964     03  FILLER    PIC 99      VALUE           0.                 05940000
E01964     03  FILLER    PIC 99      VALUE          12.                 05950000
      *                                                      2052 */    05960000
E01964     03  FILLER    PIC 99      VALUE           0.                 05970000
E01964     03  FILLER    PIC 99      VALUE           0.                 05980000
E01964     03  FILLER    PIC 99      VALUE           0.                 05990000
E01964     03  FILLER    PIC 99      VALUE          12.                 06000000
      *                                                      2056 */    06010000
E01964     03  FILLER    PIC 99      VALUE           0.                 06020000
E01964     03  FILLER    PIC 99      VALUE           0.                 06030000
E01964     03  FILLER    PIC 99      VALUE           0.                 06040000
E01964     03  FILLER    PIC 99      VALUE          12.                 06050000
      *                                                      2060 */    06060000
E01964     03  FILLER    PIC 99      VALUE           0.                 06070000
E01964     03  FILLER    PIC 99      VALUE           0.                 06080000
E01964     03  FILLER    PIC 99      VALUE           0.                 06090000
E01964     03  FILLER    PIC 99      VALUE          12.                 06100000
      *                                                      2064 */    06110000
E01964     03  FILLER    PIC 99      VALUE           0.                 06120000
E01964     03  FILLER    PIC 99      VALUE           0.                 06130000
E01964     03  FILLER    PIC 99      VALUE           0.                 06140000
E01964     03  FILLER    PIC 99      VALUE          12.                 06150000
      *                                                      2068 */    06160000
E01964     03  FILLER    PIC 99      VALUE           0.                 06170000
E01964     03  FILLER    PIC 99      VALUE           0.                 06180000
E01964     03  FILLER    PIC 99      VALUE           0.                 06190000
E01964     03  FILLER    PIC 99      VALUE          12.                 06200000
      *                                                      2072 */    06210000
E01964     03  FILLER    PIC 99      VALUE           0.                 06220000
E01964     03  FILLER    PIC 99      VALUE           0.                 06230000
E01964     03  FILLER    PIC 99      VALUE           0.                 06240000
E01964     03  FILLER    PIC 99      VALUE          12.                 06250000
      *                                                      2076 */    06260000
E01964     03  FILLER    PIC 99      VALUE           0.                 06270000
E01964     03  FILLER    PIC 99      VALUE           0.                 06280000
E01964     03  FILLER    PIC 99      VALUE           0.                 06290000
E01964     03  FILLER    PIC 99      VALUE          12.                 06300000
      *                                                      2080 */    06310000
E01964     03  FILLER    PIC 99      VALUE           0.                 06320000
E01964     03  FILLER    PIC 99      VALUE           0.                 06330000
E01964     03  FILLER    PIC 99      VALUE           0.                 06340000
E01964     03  FILLER    PIC 99      VALUE          12.                 06350000
      *                                                      2084 */    06360000
E01964     03  FILLER    PIC 99      VALUE           0.                 06370000
E01964     03  FILLER    PIC 99      VALUE           0.                 06380000
E01964     03  FILLER    PIC 99      VALUE           0.                 06390000
E01964     03  FILLER    PIC 99      VALUE          12.                 06400000
      *                                                      2088 */    06410000
E01964     03  FILLER    PIC 99      VALUE           0.                 06420000
E01964     03  FILLER    PIC 99      VALUE           0.                 06430000
E01964     03  FILLER    PIC 99      VALUE           0.                 06440000
E01964     03  FILLER    PIC 99      VALUE          12.                 06450000
      *                                                      2092 */    06460000
E01964     03  FILLER    PIC 99      VALUE           0.                 06470000
E01964     03  FILLER    PIC 99      VALUE           0.                 06480000
E01964     03  FILLER    PIC 99      VALUE           0.                 06490000
E01964     03  FILLER    PIC 99      VALUE          12.                 06500000
      *                                                      2096 */    06510000
E01964     03  FILLER    PIC 99      VALUE           0.                 06520000
E01964     03  FILLER    PIC 99      VALUE           0.                 06530000
E01964     03  FILLER    PIC 99      VALUE           0.                 06540000
           03  FILLER    PIC 99      VALUE           0.

       01  TABBIS-2 REDEFINES TABBIS-1.
E01964*    03  TABBIS-3  OCCURS   90.                                   06580000
E01964     03  TABBIS-3  OCCURS  140.                                   06590000
               05  TABBIS     PIC 99.
       01  TABMM-1.
      *                      /* NUM. GG INTERCORRENTI TRA 1.1.    **/
      *                      /* E INIZIO MESE INDICATO, DIVISI    **/
      *                      /* PER ANNI NORMALI E BISESTILI      **/
      *                      /* PER LA RICODIFICA VIENE USATA LA  **/
      *                      /* TABELLA BASATA TABMMBAS           **/
           03  FILLER   PIC 999 COMP-3 VALUE       0.
           03  FILLER   PIC 999 COMP-3 VALUE      31.
           03  FILLER   PIC 999 COMP-3 VALUE      59.
           03  FILLER   PIC 999 COMP-3 VALUE      90.
           03  FILLER   PIC 999 COMP-3 VALUE     120.
           03  FILLER   PIC 999 COMP-3 VALUE     151.
           03  FILLER   PIC 999 COMP-3 VALUE     181.
           03  FILLER   PIC 999 COMP-3 VALUE     212.
           03  FILLER   PIC 999 COMP-3 VALUE     243.
           03  FILLER   PIC 999 COMP-3 VALUE     273.
           03  FILLER   PIC 999 COMP-3 VALUE     304.
           03  FILLER   PIC 999 COMP-3 VALUE     334.
           03  FILLER   PIC 999 COMP-3 VALUE       0.
           03  FILLER   PIC 999 COMP-3 VALUE      31.
           03  FILLER   PIC 999 COMP-3 VALUE      60.
           03  FILLER   PIC 999 COMP-3 VALUE      91.
           03  FILLER   PIC 999 COMP-3 VALUE     121.
           03  FILLER   PIC 999 COMP-3 VALUE     152.
           03  FILLER   PIC 999 COMP-3 VALUE     182.
           03  FILLER   PIC 999 COMP-3 VALUE     213.
           03  FILLER   PIC 999 COMP-3 VALUE     244.
           03  FILLER   PIC 999 COMP-3 VALUE     274.
           03  FILLER   PIC 999 COMP-3 VALUE     305.
           03  FILLER   PIC 999 COMP-3 VALUE     335.
       01  TABMM-2 REDEFINES TABMM-1.
           03  TABMM-3.
             04 FILLER  OCCURS   12.
               05  TABMM      PIC 999 COMP-3.
           03  TABMM-4.
             04 FILLER  OCCURS   12.
               05  TABMM      PIC 999 COMP-3.
       01  TABMMBAS-TAB.
           03 FILLER OCCURS 12.
              05 TABMMBAS     PIC 999 COMP-3.
      *-------------------------------------------------------------
      *    TABELLA GIORNI FESTIVI PER ANNI NON BISESTILI
      *-------------------------------------------------------------
       01  TABFEST-1.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* CAPODANNO
           03  FILLER   PIC X(4)       VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* EPIFANIA              06.01 006
           03  FILLER   PIC X(108)     VALUE     SPACES.
FES17 *    03  FILLER   PIC X(069)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
FES17 *                       /* NUOVA FESTA UNITA'    17.03  70
FES17 *    03  FILLER   PIC X(038)     VALUE     SPACES.
FES17 *    03  FILLER   PIC X          VALUE     '1'.
      *                        /* ANNIV.LIBERAZIONE     25.04 115
           03  FILLER   PIC X(5)       VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* FESTA DEL LAVORO      01.05 121
E01790*    03  FILLER   PIC X(105)     VALUE     SPACES.
E01790     03  FILLER   PIC X(031)     VALUE     SPACES.
E01790     03  FILLER   PIC X          VALUE     '1'.
E01790*                        /* 2 GIUGNO              02.06 153
E01790     03  FILLER   PIC X(073)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* FERRAGOSTO            15.08 227
           03  FILLER   PIC X(077)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* OGNISSANTI            01.11 305
           03  FILLER   PIC X(036)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* IMMACOLATA CONCEZ.    08.12 342
           03  FILLER   PIC X(016)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* NATALE                25.12 359
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* S. STEFANO            26.12 360
XMBI01* L'INTERVENTO ERA STATO EFFETTUATO PER LA GESTIONE DEL 31/12/2001
XMBI01* IN TALE DATA TUTTE LE BANCHE RISULTAVANO CHIUSE PER DARE LA
XMBI01* POSSIBILIT{ DI CONVERTIRSI ALL'EURO. LASCIANDO QUESTA GESTIONE
XMBI01* IL 31/12 DI TUTTI GLI ANNI RISULTER{ FESTIVO PERTANTO SI }
XMBI01* PROCEDUTO ALL'ELIMINAZIONE DEL TEST ORMAI INUTILE.
XMBI01*E01790*    03  FILLER   PIC X(005)     VALUE     SPACES.
XMBI01*E01790     03  FILLER   PIC X(004)     VALUE     SPACES.
XMBI01*E01790     03  FILLER   PIC X          VALUE     '1'.
XMBI01*E01790*                /* CAPODANNO             31.12 365
       01  TABFEST-2 REDEFINES TABFEST-1.
           03  TABFEST-3  OCCURS   365.
               05  TABFEST-NORM   PIC X.
      * ------------------------------------------------------------
      *    TABELLA GIORNI FESTIVI PER ANNI BISESTILI
      * ------------------------------------------------------------
       01  TABFESTB-1.
           03  FILLER   PIC X          VALUE     '1'.
      *                        /* CAPODANNO
           03  FILLER   PIC X(4)       VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* EPIFANIA
           03  FILLER   PIC X(109)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* ANNIV.LIBERAZIONE
           03  FILLER   PIC X(5)       VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* FESTA DEL LAVORO
E01790*    03  FILLER   PIC X(105)     VALUE     SPACES.
E01790     03  FILLER   PIC X(031)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
E01790*                        /* 2 GIUGNO
E01790     03  FILLER   PIC X(073)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* FERRAGOSTO
           03  FILLER   PIC X(077)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* OGNISSANTI
           03  FILLER   PIC X(036)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* IMMACOLATA CONCEZ.
           03  FILLER   PIC X(016)     VALUE     SPACES.
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* NATALE
           03  FILLER   PIC X          VALUE     '1'.
      *                       /* S. STEFANO
           03  FILLER   PIC X(005)     VALUE     SPACES.
       01  TABFESTB-2 REDEFINES TABFESTB-1.
           03  TABFESTB-3  OCCURS   366.
               05  TABFEST-BISES  PIC X.
       01  TABFEST-BAS-TAB.
           03 FILLER OCCURS 366.
              05 TABFEST-BAS      PIC X.
       01  TABFEST-MOB1.
      *   GIORNO IN CUI CDDE IL LUNEDI DI PASQUA NELL'ANNO INDICATO
      *   N.B. LE FESTIVITA' DELL'ASCENSIONE E DEL CORPUS DOMINI SONO
      *   STATE SOPPRESSE. COMUNQUE I GIORNI IN CUI CADONO TALI
      *   RICORRENZE SI RICAVANO COSI' :
      *               GG ASCENSIONE   = PASQUETTA + 38
      *               GG CORPUS DOMINI= PASQUETTA + 59
           03  FILLER  PIC 9(5) COMP-3 VALUE    93.
      *    /* 61.04.03 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    478.
      *    /* 62.04.23 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    835.
      *    /* 63.04.15 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    1185.
      *    /* 64.03.30 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    1570.
      *    /* 65.04.19 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    1927.
      *    /* 66.04.11 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    2277.
      *    /* 67.03.27 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    2662.
      *    /* 68.04.15 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    3019.
      *    /* 69.04.07 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    3376.
      *    /* 70.03.30 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    3754.
      *    /* 71.04.12 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    4110.
      *    /* 72.04.02 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    4496.
      *    /* 73.04.23 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    4853.
      *    /* 74.04.15 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    5203.
      *    /* 75.03.31 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    5588.
      *    /* 76.04.19 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    5945.
      *    /* 77.04.11 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    6295.
      *    /* 78.03.27 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    6680.
      *    /* 79.04.16 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    7037.
      *    /* 80.04.07 */
           03  FILLER  PIC 9(5) COMP-3 VALUE    7415.
      *    /* 81.04.20 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   7772.
      *    /* 82.04.12 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   8129.
      *    /* 83.04.04 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   8514.
      *    /* 84.04.23 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   8864.
      *    /* 85.04.08 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   9221.
      *    /* 86.03.31 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   9606.
      *    /* 87.04.20 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   9956.
      *    /* 88.04.04 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   10313.
      *    /* 89.03.27 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   10698.
      *    /* 90.04.16 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   11048.
      *    /* 91.04.01 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   11433.
      *    /* 92.04.20 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   11790.
      *    /* 93.04.12 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   12147.
      *    /* 94.04.04 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   12525.
      *    /* 95.04.17 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   12882.
      *    /* 96.04.08 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   13239.
      *    /* 97.03.31 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   13617.
      *    /* 98.04.13 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   13974.
      *    /* 99.04.05 */
      * INIZ. MOD. PIER ***
           03  FILLER  PIC 9(5) COMP-3 VALUE   14359.
      *    /* 2000.04.24 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   14716.
      *    /* 2001.04.16 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   15066.
      *    /* 2002.04.01 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   15451.
      *    /* 2003.04.21 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   15808.
      *    /* 2004.04.12 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   16158.
      *    /* 2005.03.28 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   16543.
      *    /* 2006.04.17 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   16900.
      *    /* 2007.04.09 */
XSLU***    03  FILLER  PIC 9(5) COMP-3 VALUE   17235.
      *    /* 2008.03.09 */
XSLU       03  FILLER  PIC 9(5) COMP-3 VALUE   17250.
XSLU  *    /* 2008.03.24 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   17635.
      *    /* 2009.04.13 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   17992.
      *    /* 2010.04.05 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   18377.
      *    /* 2011.04.25 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   18727.
      *    /* 2012.04.09 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   19084.
      *    /* 2013.04.01 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   19469.
      *    /* 2014.04.21 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   19819.
      *    /* 2015.04.06 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   20176.
      *    /* 2016.03.28 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   20561.
      *    /* 2017.04.17 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   20911.
      *    /* 2018.04.02 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   21296.
      *    /* 2019.04.22 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   21651.                   09170000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   21653.                   09180000
      *    /* 2020.04.13 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   22010.
      *    /* 2021.04.05 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   22388.
      *    /* 2022.04.18 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   22745.
      *    /* 2023.04.10 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   23102.
      *    /* 2024.04.01 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   23487.
      *    /* 2025.04.21 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   23837.
      *    /* 2026.04.06 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   24194.
      *    /* 2027.03.29 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   24579.
      *    /* 2028.04.17 */
           03  FILLER  PIC 9(5) COMP-3 VALUE   24929.
      *    /* 2029.04.02 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   25274.                   09380000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   25313.                   09381000
      *    /* 2030.04.22 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   25631.                   09400000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   25670.                   09401000
      *    /* 2031.04.14 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   25988.                   09420000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   26028.                   09421000
      *    /* 2032.04.05 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   26366.                   09440000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   26406.                   09441000
      *    /* 2033.04.18 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   26723.                   09460000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   26763.                   09461000
      *    /* 2034.04.10 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   27073.                   09480000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   27113.                   09481000
      *    /* 2035.03.26 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   27458.                   09500000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   27498.                   09501000
      *    /* 2036.04.14 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   27815.                   09520000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   27855.                   09521000
      *    /* 2037.04.06 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   28193.                   09540000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   28233.                   09541000
      *    /* 2038.04.19 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   28550.                   09560000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   28590.                   09561000
      *    /* 2039.04.11 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   28907.                   09580000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   28947.                   09581000
      *    /* 2040.04.02 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   29292.                   09600000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   29332.                   09601000
      *    /* 2041.04.22 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   29642.                   09620000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   29682.                   09621000
      *    /* 2042.04.07 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   29999.                   09640000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   30039.                   09641000
      *    /* 2043.03.30 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   30384.                   09660000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   30324.                   09661000
      *    /* 2044.04.18 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   30741.                   09680000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   30781.                   09681000
      *    /* 2045.04.10 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   31091.                   09700000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   31131.                   09701000
      *    /* 2046.03.26 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   31476.                   09720000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   31516.                   09721000
      *    /* 2047.04.15 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   31833.                   09740000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   31873.                   09741000
      *    /* 2048.04.06 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   32211.                   09760000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   32251.                   09761000
      *    /* 2049.04.19 */
E01964*    03  FILLER  PIC 9(5) COMP-3 VALUE   32568.                   09780000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   32608.                   09781000
      *    /* 2050.04.11 */
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   32964.                   09791000
E01964*    /* 2051.04.02 */                                             09792000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   33349.                   09793000
E01964*    /* 2052.04.21 */                                             09794000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   33699.                   09795000
E01964*    /* 2053.04.06 */                                             09796000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   34056.                   09797000
E01964*    /* 2054.03.29 */                                             09798000
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   34441.                   09799000
E01964*    /* 2055.04.18 */                                             09799100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   34791.                   09799200
E01964*    /* 2056.04.02 */                                             09799300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   35176.                   09799400
E01964*    /* 2057.04.22 */                                             09799500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   35533.                   09799600
E01964*    /* 2058.04.14 */                                             09799700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   35883.                   09799800
E01964*    /* 2059.03.30 */                                             09799900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   36268.                   09800000
E01964*    /* 2060.04.18 */                                             09800100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   36625.                   09800200
E01964*    /* 2061.04.10 */                                             09800300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   36975.                   09800400
E01964*    /* 2062.03.26 */                                             09800500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   37360.                   09800600
E01964*    /* 2063.04.15 */                                             09800700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   67717.                   09800800
E01964*    /* 2064.04.06 */                                             09800900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   38074.                   09801000
E01964*    /* 2065.03.29 */                                             09801100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   38452.                   09801200
E01964*    /* 2066.04.11 */                                             09801300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   38809.                   09801400
E01964*    /* 2067.04.03 */                                             09801500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   39194.                   09801600
E01964*    /* 2068.04.22 */                                             09801700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   39551.                   09801800
E01964*    /* 2069.04.14 */                                             09801900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   39901.                   09802000
E01964*    /* 2070.03.30 */                                             09802100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   40286.                   09802200
E01964*    /* 2071.04.19 */                                             09802300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   40643.                   09802400
E01964*    /* 2072.04.10 */                                             09802500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   40993.                   09802600
E01964*    /* 2073.03.26 */                                             09802700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   41378.                   09802800
E01964*    /* 2074.04.15 */                                             09802900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   41735.                   09803000
E01964*    /* 2075.04.07 */                                             09803100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   42113.                   09803200
E01964*    /* 2076.04.19 */                                             09803300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   42470.                   09803400
E01964*    /* 2077.04.11 */                                             09803500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   42827.                   09803600
E01964*    /* 2078.04.03 */                                             09803700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   43212.                   09803800
E01964*    /* 2079.04.23 */                                             09803900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   43562.                   09804000
E01964*    /* 2080.04.07 */                                             09804100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   43919.                   09804200
E01964*    /* 2081.03.30 */                                             09804300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   44304.                   09804400
E01964*    /* 2082.04.19 */                                             09804500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   44654.                   09804600
E01964*    /* 2083.04.04 */                                             09804700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   45011.                   09804800
E01964*    /* 2084.03.26 */                                             09804900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   45396.                   09805000
E01964*    /* 2085.04.15 */                                             09805100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   45746.                   09805200
E01964*    /* 2086.03.31 */                                             09805300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   46131.                   09805400
E01964*    /* 2087.04.20 */                                             09805500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   46488.                   09805600
E01964*    /* 2088.04.11 */                                             09805700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   46845.                   09805800
E01964*    /* 2089.04.03 */                                             09805900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   47223.                   09806000
E01964*    /* 2090.04.16 */                                             09806100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   47580.                   09806200
E01964*    /* 2091.04.08 */                                             09806300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   47937.                   09806400
E01964*    /* 2092.03.30 */                                             09806500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   48315.                   09806600
E01964*    /* 2093.04.12 */                                             09806700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   48672.                   09806800
E01964*    /* 2094.04.04 */                                             09806900
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   49057.                   09807000
E01964*    /* 2095.04.24 */                                             09807100
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   49414.                   09807200
E01964*    /* 2096.04.15 */                                             09807300
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   49764.                   09807400
E01964*    /* 2097.03.31 */                                             09807500
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   50149.                   09807600
E01964*    /* 2098.04.20 */                                             09807700
E01964     03  FILLER  PIC 9(5) COMP-3 VALUE   50506.                   09807800
E01964*    /* 2099.04.12 */                                             09807900
      *   FINE MOD. PIER ***
       01  TABFEST-MOB2 REDEFINES TABFEST-MOB1.
E01964*    03  TABFEST-MOB3  OCCURS   129.                              09830000
E01964     03  TABFEST-MOB3  OCCURS   178.                              09831000
               05  TABFEST-MOBILI PIC 9(5) COMP-3.
      * ---------------------------------------
      *         TABELLA DATE DI FINE MESE                            */
      * ---------------------------------------
       01  TAB-FMM1.
           03  FILLER  PIC X(4)   VALUE '0131'.
           03  FILLER  PIC X(4)   VALUE '0228'.
           03  FILLER  PIC X(4)   VALUE '0331'.
           03  FILLER  PIC X(4)   VALUE '0430'.
           03  FILLER  PIC X(4)   VALUE '0531'.
           03  FILLER  PIC X(4)   VALUE '0630'.
           03  FILLER  PIC X(4)   VALUE '0731'.
           03  FILLER  PIC X(4)   VALUE '0831'.
           03  FILLER  PIC X(4)   VALUE '0930'.
           03  FILLER  PIC X(4)   VALUE '1031'.
           03  FILLER  PIC X(4)   VALUE '1130'.
           03  FILLER  PIC X(4)   VALUE '1231'.
       01  TAB-FMM2     REDEFINES TAB-FMM1.
           03  TAB-FMM3  OCCURS   12.
               05  TAB-FMM    PIC X(4).
       01  TAB-FMM3     REDEFINES TAB-FMM1.
           03  RTAB-FMM      OCCURS   12.
               05  TAB-FMM-MM  PIC 99.
               05  TAB-FMM-GG  PIC 99.
      * ---------------------------------------
      *         TABELLA DATE INIZIO E FINE TRIMESTRE                 */
      * ---------------------------------------
       01  TAB-TRI1.
           03  FILLER  PIC X(8)   VALUE '01010331'.
           03  FILLER  PIC X(8)   VALUE '04010630'.
           03  FILLER  PIC X(8)   VALUE '07010930'.
           03  FILLER  PIC X(8)   VALUE '10011231'.
       01  TAB-TRI2     REDEFINES TAB-TRI1.
           03  TAB-TRI3  OCCURS   04.
               05  TAB-TRIM   PIC X(8).
       01  RTAB-TRIM1    REDEFINES TAB-TRI1.
           03  RTAB-TRIM  OCCURS   04.
               5   TAB-TRIM-IN.
                   7  TAB-TRIM-IN-MM  PIC 99.
                   7  TAB-TRIM-IN-GG  PIC 99.
               5   TAB-TRIM-FF.
                   7  TAB-TRIM-FF-MM  PIC 99.
                   7  TAB-TRIM-FF-GG  PIC 99.
      *---------------------------------------------*
      * TABELLA GIORNI                              *
      * ------------------------------------------- *
       01  GIORNI.
           02 FILLER               PIC X(9) VALUE 'LUNEDI   '.
           02 FILLER               PIC X(9) VALUE 'MARTEDI  '.
           02 FILLER               PIC X(9) VALUE 'MERCOLEDI'.
           02 FILLER               PIC X(9) VALUE 'GIOVEDI  '.
           02 FILLER               PIC X(9) VALUE 'VENERDI  '.
           02 FILLER               PIC X(9) VALUE 'SABATO   '.
           02 FILLER               PIC X(9) VALUE 'DOMENICA '.
       01  TAB-GIORNI REDEFINES GIORNI.
           02 ELEM-GIORNI OCCURS 7.
              03 GIORNI-B          PIC X(03).
              03 FILLER            PIC X(06).
      *---------------------------------------------*
      * TABELLA MESI                                *
      * ------------------------------------------- *
       01  MESI.
           02 FILLER               PIC X(9) VALUE 'GENNAIO  '.
           02 FILLER               PIC X(9) VALUE 'FEBBRAIO '.
           02 FILLER               PIC X(9) VALUE 'MARZO    '.
           02 FILLER               PIC X(9) VALUE 'APRILE   '.
           02 FILLER               PIC X(9) VALUE 'MAGGIO   '.
           02 FILLER               PIC X(9) VALUE 'GIUGNO   '.
           02 FILLER               PIC X(9) VALUE 'LUGLIO   '.
           02 FILLER               PIC X(9) VALUE 'AGOSTO   '.
           02 FILLER               PIC X(9) VALUE 'SETTEMBRE'.
           02 FILLER               PIC X(9) VALUE 'OTTOBRE  '.
           02 FILLER               PIC X(9) VALUE 'NOVEMBRE '.
           02 FILLER               PIC X(9) VALUE 'DICEMBRE '.
       01  TAB-MESI REDEFINES MESI.
           02 ELEM-MESI OCCURS 12.
              03 MESE-B            PIC X(03).
              03 FILLER            PIC X(06).
      ****************************************
      * DT5  GG/MM/AAAA ALFANUMERICA  L=10   *
      ****************************************
       01  DT5                PIC X(10).
       01  DT5R REDEFINES     DT5.
           03 DT5GIO          PIC XX.
           03 FIL1            PIC X.
           03 DT5MES          PIC XX.
           03 FIL2            PIC X.
           03 DT5ACO.
              04 DT5SEC       PIC XX.
              04 DT5ANN       PIC XX.
      ****************************************
      * DT6  GG/MM/AA   ALFANUMERICA   L=08  *
      ****************************************
       01  DT6                PIC X(08).
       01  DT6R   REDEFINES   DT6.
           03 DT6GIO          PIC XX.
           03 FIL3            PIC X.
           03 DT6MES          PIC XX.
           03 FIL4            PIC X.
           03 DT6ANN          PIC XX.
      **********************************************
      * DT10 GG MMMMMMMMM AAAA  ALFANUMERICO  L=17 *
      **********************************************
       01  DT10               PIC X(17).
       01  DT10R   REDEFINES DT10.
           03 DT10GIO         PIC XX.
           03 FILLE10         PIC X.
           03 DT10MES         PIC X(9).
           03 FILLE11         PIC X.
           03 DT10ACO.
              04 DT10SEC      PIC XX.
              04 DT10ANN      PIC XX.
      ****************************************
      * DT11 GG MMM AA    ALFANUMERICO  L=09 *
      ****************************************
       01  DT11               PIC X(09).
       01  DT11R   REDEFINES DT11.
           03 DT11GIO         PIC XX.
           03 FILLE12         PIC X.
           03 DT11MES         PIC X(3).
           03 FILLE13         PIC X.
           03 DT11ANN         PIC XX.
      ********************************************************
      * DT12 GGGGGGGGG GG MMMMMMMMM AAAA  ALFANUMERICO  L=27 *
      ********************************************************
       01  DT12               PIC X(27).
       01  DT12R   REDEFINES DT12.
           03 DT12GIS         PIC X(9).
           03 FILLE14         PIC X.
           03 DT12GIO         PIC XX.
           03 FILLE15         PIC X.
           03 DT12MES         PIC X(9).
           03 FILLE16         PIC X.
           03 DT12ACO.
              04 DT12SEC      PIC XX.
              04 DT12ANN      PIC XX.
      ******************************************
      * DT13 GGG GG MMM AA   ALFANUMERICO L=13 *
      ******************************************
       01  DT13               PIC X(13).
       01  DT13R   REDEFINES DT13.
           03 DT13GIS         PIC XXX.
           03 FILLE17         PIC X.
           03 DT13GIO         PIC XX.
           03 FILLE18         PIC X.
           03 DT13MES         PIC X(3).
           03 FILLE19         PIC X.
           03 DT13ANN         PIC XX.
      ****************************************
      * DT15 MM/GG/AA   ALFANUMERICA   L=08  *
      ****************************************
       01  DT15               PIC X(08).
       01  DT15R  REDEFINES   DT15.
           03 DT15MES         PIC XX.
           03 FIL5            PIC X.
           03 DT15GIO         PIC XX.
           03 FIL6            PIC X.
           03 DT15ANN         PIC XX.
EXPAND*    EXEC SQL INCLUDE NP0500EC
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP0500EC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER ENTRATA IN ROUTINE AREA APPL. = 0500  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
       LINKAGE SECTION.
       01  WRK-COMMAREA        PIC X(1150).
      *---------------------------------------------------------------*
      *  PROCEDURE DIVISION                                           *
      *---------------------------------------------------------------*
       PROCEDURE DIVISION USING WRK-COMMAREA.
       INIZIO-PGM.
           MOVE WRK-COMMAREA   TO WRK-NP0500R.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP0500EC
EXPAND*--------------------------------------------------------
           MOVE WRK-NP0500                      TO NPW08RC.
           MOVE WRK-NPG01-0500                  TO NPG01RC.
XATV       MOVE ZERO        TO CGGG.
           MOVE ZERO        TO W08-RCODE.
           MOVE ZERO        TO SW-ERR.
INS+       IF W08-RTIPO = '04'
INS+       OR W08-RTIPO = '15'
INS+       OR W08-RTIPO = '16'
INS+       OR W08-RTIPO = '17'
INS+       OR W08-RTIPO = '18'
XATV  *    OR W08-RTIPO = '19'
INS+          IF W08-RGGG = ZERO
INS+              GO TO   NPW08R-LABEL.
           IF W08-RTIPO = '01'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                        /* DECODIFICA CIVILE */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI
               ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '02'
      *                             /* RICODIFICA GG FISSI */
              PERFORM NPW08R-RICOSOL THRU EX-NPW08R-RICOSOL
              PERFORM NPW08R-DTOUT   THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '03'
      *                             /* RICODIFICA GIORNI     */
      *                             /* SE FESTIVA-SUCCESSIVA */
              PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI
              PERFORM NPW08R-DTOUT   THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '04'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* SOMMA A DATA NUMERO */
      *                              /* FISSO DI GIORNI     */
                 PERFORM NPW08R-DECORICO THRU EX-NPW08R-DECORICO
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '05'
      *                               /* RICODIFICA GIORNI     */
      *                             /* SE FESTIVA-PRECEDENTE */
              PERFORM NPW08R-RICOCIVI-5 THRU EX-NPW08R-RICOCIVI-5
              PERFORM NPW08R-DTOUT      THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '06'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                                /* DECODIFICA A PARTIRE */
      *                                /* DA INIZIO ANNO GG.CONV.*/
                 PERFORM NPW08R-DECOCIVI-6 THRU EX-NPW08R-DECOCIVI-6
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '07'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* RICERCA DATA DI     */
      *                              /* FINE MESE PRECEDENTE*/
                 PERFORM NPW08R-FINEMMPR THRU EX-NPW08R-FINEMMPR
                 MOVE W08-RGGG           TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '08'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* RICERCA DATA DI FINE */
      *                              /* TRIMESTRE PRECEDENTE */
                 PERFORM NPW08R-FINETRPR THRU EX-NPW08R-FINETRPR
                 MOVE W08-RGGG           TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '09'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* RICERCA NUMERO      */
      *                              /* TRIMESTRE           */
                 PERFORM NPW08R-NRTRM THRU EX-NPW08R-NRTRM
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '10'
      *                             /* CONTROLLA VALIDITA'   */
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '11'
      *                             /* INVERTI   DATA        */
              PERFORM NPW08R-INVDATA THRU EX-NPW08R-INVDATA
              MOVE    W08-RDATA       TO   DATACIVI-PIC
              PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '12'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* CONTROLLA FESTIVITA'*/
                 PERFORM NPW08R-DATALAV THRU EX-NPW08R-DATALAV
                 PERFORM NPW08R-DTOUT   THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '13'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
              IF W08-RCODE = ZERO
      *                              /* DECODIFICA COMMERCIALE
                 PERFORM NPW08R-DECOCOM THRU EX-NPW08R-DECOCOM
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '14'
      *                             /* RICODIFICA COMMERCIALE */
              PERFORM NPW08R-RICOCOM THRU EX-NPW08R-RICOCOM
              PERFORM NPW08R-DTOUT   THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '15'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                 /* SOTTRAI A DATA GIORNI  */
      *                                 /* RICODIFICA SOLARE      */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-DECORICO-6 THRU EX-NPW08R-DECORICO-6
                 PERFORM NPW08R-DTOUT      THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '16'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOMMA A DATA GIORNI    */
      *                                /* E RICODIFICA CIVILE    */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-RICOLAV3 THRU EX-NPW08R-RICOLAV3
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '17'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOTTRAI DATA GIORNI    */
      *                                /* E RICODIFICA CIVILE    */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-RICOLAV4 THRU EX-NPW08R-RICOLAV4
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '18'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOMMA A DATA NUMERO    */
      *                                /* NUMERO GGG LAVORATIVI  */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-RICOLAV5 THRU EX-NPW08R-RICOLAV5
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '19'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOTTRAI A DATA NUMERO  */
      *                                /* NUMERO GGG LAVORATIVI  */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-RICOLAV6 THRU EX-NPW08R-RICOLAV6
                 PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '20'
              IF W08-RDATA LESS W08-RGGG
                 MOVE 1 TO SW-ERR
                 MOVE W08-RDATA TO DIF-DAT1
                 MOVE W08-RGGG TO DIF-DAT2
                 PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                 IF W08-RCODE NOT = ZERO
                    NEXT SENTENCE
                 ELSE
                    MOVE DIF-DAT2 TO W08-RDATA
                    PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                    IF W08-RCODE NOT = ZERO
                       NEXT SENTENCE
                    ELSE
      *                                 /* CALCOLA DIFFERENZA       */
      *                                 /* CIVILE TRA DUE DATE      */
                       PERFORM NPW08R-DIFFCIVI THRU EX-NPW08R-DIFFCIVI
              ELSE
                 MOVE W08-RDATA TO DIF-DAT1
                 MOVE W08-RGGG TO DIF-DAT2
                 PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                 IF W08-RCODE NOT = ZERO
                    NEXT SENTENCE
                 ELSE
                    MOVE DIF-DAT2 TO W08-RDATA
                    PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                    IF W08-RCODE NOT = ZERO
                       NEXT SENTENCE
                    ELSE
      *                                 /* CALCOLA DIFFERENZA       */
      *                                 /* CIVILE TRA DUE DATE      */
                       PERFORM NPW08R-DIFFCIVI THRU EX-NPW08R-DIFFCIVI
           ELSE
           IF W08-RTIPO = '21'
              IF W08-RDATA LESS W08-RGGG
                 MOVE 1 TO SW-ERR
                 MOVE W08-RDATA TO DIF-DAT1
                 MOVE W08-RGGG TO DIF-DAT2
                 PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                 IF W08-RCODE NOT = ZERO
                    NEXT SENTENCE
                 ELSE
                    MOVE DIF-DAT2 TO W08-RDATA
                    PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                    IF W08-RCODE NOT = ZERO
                       NEXT SENTENCE
                    ELSE
      *                                 /* CALCOLA DIFFERENZA       */
      *                                 /* COMMERCIALE TRA DUE DATE */
                       PERFORM NPW08R-DIFFCOM THRU EX-NPW08R-DIFFCOM
              ELSE
                 MOVE W08-RDATA TO DIF-DAT1
                 MOVE W08-RGGG TO DIF-DAT2
                 PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                 IF W08-RCODE NOT = ZERO
                    NEXT SENTENCE
                 ELSE
                    MOVE DIF-DAT2 TO W08-RDATA
                    PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                    IF W08-RCODE NOT = ZERO
                       NEXT SENTENCE
                    ELSE
      *                                 /* CALCOLA DIFFERENZA       */
      *                                 /* COMMERCIALE TRA DUE DATE */
                       PERFORM NPW08R-DIFFCOM THRU EX-NPW08R-DIFFCOM
           ELSE
           IF W08-RTIPO = '22'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOTTRAI A DATA NUMERO    */
      *                                /*          GGG LAVORATIVI  */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ22 THRU EX-NPW08R-FUNZ22
                 MOVE AMG                TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '23'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* SOTTRAI A DATA NUMERO    */
      *                                /* NUMERO   GGG LAVORATIVI  */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ23 THRU EX-NPW08R-FUNZ23
                 MOVE AMG                TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '24'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* CALCOLA SE LA DATA E'    */
      *                                /* DI FINE MESE             */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ24 THRU EX-NPW08R-FUNZ24
                 MOVE AMG                TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '25'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* CALCOLA SE LA DATA E'    */
      *                                /* DI FINE TRIMESTRE        */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ25 THRU EX-NPW08R-FUNZ25
                 MOVE AMG                TO   DATACIVI-PIC
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '26'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* TRASFORMA LA DATA      */
      *                                /* AAMMGG -----> AAGGG    */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ26 THRU EX-NPW08R-FUNZ26
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '27'
      *                                /* TRASFORMA LA DATA      */
      *                                /* AAGGG -----> AAMMGG   */
              PERFORM NPW08R-FUNZ27 THRU EX-NPW08R-FUNZ27
              PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
           ELSE
           IF W08-RTIPO = '28'
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* CALCOLA GIORNO         */
      *                                /* DELLA SETTIMANA        */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ28 THRU EX-NPW08R-FUNZ28
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '29'
              MOVE W08-RDATA TO KOM1-N
              MOVE KOM1-1 TO KOM2-5
              MOVE KOM1-2 TO KOM2-6
              MOVE KOM1-3 TO KOM2-3
              MOVE KOM1-4 TO KOM2-4
              MOVE KOM1-5 TO KOM2-1
              MOVE KOM1-6 TO KOM2-2
              MOVE KOM2-N TO W08-RDATA
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* GGMMAA IN SSAAMMGG     */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ29 THRU EX-NPW08R-FUNZ29
                 PERFORM NPW08R-DTOUT  THRU EX-NPW08R-DTOUT
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '31'
              MOVE W08-RDATA TO KOM1-N
              MOVE KOM1-1 TO KOM2-5
              MOVE KOM1-2 TO KOM2-6
              MOVE KOM1-3 TO KOM2-3
              MOVE KOM1-4 TO KOM2-4
              MOVE KOM1-5 TO KOM2-1
              MOVE KOM1-6 TO KOM2-2
              MOVE KOM2-N TO W08-RDATA
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      *                                /* GGMMAA IN   AAGGG      */
              IF W08-RCODE = ZERO
                 PERFORM NPW08R-FUNZ31 THRU EX-NPW08R-FUNZ31
              ELSE NEXT SENTENCE
           ELSE
           IF W08-RTIPO = '35'
      *                                /* DECOMPL. DATA E ORA    */
              PERFORM NPW08R-FUNZ35 THRU EX-NPW08R-FUNZ35

           ELSE
           IF W08-RTIPO = '36'
      *                                /* FINE MESE DATA CORRENTE*/
              PERFORM NPW08R-FUNZ36 THRU EX-NPW08R-FUNZ36

           ELSE
280500     IF W08-RTIPO = '37'
280500        PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
280500*                                /* SOMMA A DATA NUMERO    */
280500*                                /* NUMERO GGG LAVORATIVI  */
280500        IF W08-RCODE = ZERO
280500           PERFORM NPW08R-RICOLAV7 THRU EX-NPW08R-RICOLAV7
280500           PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT
280500        ELSE NEXT SENTENCE
280500     ELSE
      *                                /* FUNZIONE NON PREVISTA  */
              MOVE 99 TO W08-RCODE.
INS+   NPW08R-LABEL.
           MOVE NPW08RC                         TO WRK-NP0500.
           MOVE NPG01RC                         TO WRK-NPG01-0500.
EXPAND*    EXEC SQL INCLUDE NP0500FC
EXPAND*--------------------------------------------------------
EXPAND* INIZIO ESPLOSIONE COPY NP0500FC
EXPAND*--------------------------------------------------------
      *---------------------------------------------------------------*
      *  INCLUDE UTILIZZATA PER RIENTRO DA ROUTINE AREA APPL. = 0500  *
      *    VERSIONE BATCH                                             *
      *---------------------------------------------------------------*
           MOVE WRK-NP0500R TO WRK-COMMAREA.
           GOBACK.
      *---------------------------------------------------------------*
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NP0500FC
EXPAND*--------------------------------------------------------
      *****************************************************************
      *       ROUTINE DECODIFICA ANNO SOLARE                          *
      *                                                               *
      *    INPUT :  DATA NELLA FORMA AAMMGG COMPRESA                  *
      *             TRA 620101 E 20501231                             *
      *    OUTPUT : NUMERO GIORNI INTERCORRENTI                       *
      *             TRA LA DATA FORNITA E 610101                      *
      *                                                               *
      *****************************************************************
       NPW08R-DECOCIVI.
           MOVE  W08-RDATA TO DATACIVI-PIC.
           IF CIVIAA LESS 62
              MOVE 1      TO CIVIAA3-1
              MOVE CIVIAA TO CIVIAA3-2
           ELSE
              MOVE CIVIAA TO CIVIAA3.
           COMPUTE INDAA = CIVIAA3 - ANNO-COST.
      *                                  /* CALCOLA IND. TAB. ANNI  */
           ADD 1 TO INDAA.
           COMPUTE INDMM = TABBIS(INDAA) + CIVIMM.
      *                                  /* CALCOLA IND. TAB. MESI  */
           SUBTRACT 1 FROM INDAA.
           IF INDMM > 12
              COMPUTE INDMM = INDMM - 12
              COMPUTE W08-RGGG = TABAA (INDAA) +
                      TABMM OF TABMM-4 (INDMM) + CIVIGG
           ELSE
              COMPUTE W08-RGGG = TABAA (INDAA) +
                      TABMM OF TABMM-3 (INDMM) + CIVIGG.
       EX-NPW08R-DECOCIVI.
           EXIT.
      *****************************************************************
      *       ROUTINE RICODIFICA ANNO SOLARE                          *
      *                                                               *
      *    INPUT :  NUMERO GIORNI INTERCORRENTI                       *
      *             TRA LA DATA DA TROVARE E 610101                   *
      *       N.B.  TALE NUMERO DEVE ESSERE                           *
      *                                     >=    366 (01.01.62)      *
      *                                     <= 32.859 (31.12.2050)    *
      *     OUTPUT : DATA NELLA FORMA AAMMGG                          *
      *                                                               *
      *****************************************************************
       NPW08R-RICOSOL.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOSOL.
      * INIZ. PIER **
           IF W08-RGGG GREATER 32872
      * FINE  PIER **
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOSOL.
           COMPUTE INDAA = W08-RGGG / 366.
      *                        /* CALCOLA INDICE PER TABAA  */
      *                        /* APPROSSIMATO PER DIFETTO  */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDAA FROM INDAA BY 1
              UNTIL TABAA (INDAA) NOT LESS W08-RGGG OR
                 INDAA GREATER 129.
           IF INDAA GREATER 129
              MOVE 5 TO W08-RCODE
              GO TO EX-NPW08R-RICOSOL.
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = W08-RGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
      *                                 /* CALCOLA N. GG */
      *                                 /* DA INIZIO ANNO  */
           IF TABBIS(INDAA) = ZERO
              MOVE TABMM-3 TO TABMMBAS-TAB
      *                                       /* ANNO NORMALE     */
           ELSE
              MOVE TABMM-4 TO TABMMBAS-TAB.
      *                                        /* ANNO BISESTILE   */
           COMPUTE CIVIAA = INDAA + 60.
      *                                   /* CALCOLA ANNO     */
      *                                 /* RICERCA MM UTILIZZANDO  */
      *                                 /* N. GG DA INIZIO ANNO    */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDMM FROM 12 BY -1
              UNTIL INDMM = 1 OR
                 GGANNO GREATER TABMMBAS(INDMM).
           IF GGANNO GREATER TABMMBAS (INDMM)
              MOVE INDMM  TO  CIVIMM
              COMPUTE CIVIGG = GGANNO - TABMMBAS(INDMM)
      *                                        /* GG DEL MESE    */
              MOVE DATACIVI-PIC  TO  W08-RDATA
           ELSE
              MOVE 5 TO W08-RCODE.
       EX-NPW08R-RICOSOL.
           EXIT.
       DO-1.
       EX-DO-1.
           EXIT.
      ****************************************************************
      *      ROUTINE RICODIFICA ANNO CIVILE                          *
      *                                                              *
      *      INPUT :  NUMERO GIORNI INTERCORRENTI                    *
      *               TRA LA DATA DA TROVARE E 610101                *
      *               N.B.  TALE NUMERO DEVE ESSERE                  *
      *                                    >=    366 (01.01.62)      *
      *                                    <= 32.859 (31.12.2050)    *
      *      OUTPUT : DATA NELLA FORMA AAMMGG                        *
      *               (PRIMO GIORNO LAVORATIVO)                      *
      *                                                              *
      ****************************************************************
       NPW08R-RICOCIVI.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI.
      * MOD. PIER **
           IF W08-RGGG GREATER 32872
      * FINE PIER **
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI.
       CALC-REST.
           DIVIDE 7 INTO W08-RGGG GIVING COM-DIVIDE REMAINDER REST.
      *           CONTROLLO SABATO E DOMENICA
      *           POICHE' 1.1.61 CADE DI DOMENICA, SI HA :
      *           REST = 0 --> SABATO   (IMO GG LAV = + 2)
      *           REST = 1 --> DOMENICA (IMO GG LAV = = 1)
           IF REST LESS 2
              COMPUTE W08-RGGG = W08-RGGG + 2 - REST.
           COMPUTE INDAA = W08-RGGG / 366.
      *                        /* CALCOLA INDICE PER TABAA        */
      *                        /* APPROSSIMATO PER DIFETTO        */
      *                            /* RICERCA N.GIORNI IN TABAA
      *  MODIFICA DI PIER DA 39 A 90 *******
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDAA FROM INDAA BY 1
              UNTIL INDAA GREATER 90 OR
                 TABAA (INDAA) NOT LESS W08-RGGG.
           IF INDAA GREATER 90
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI.
      * CONTROLLO PASQUETTA
           IF TABFEST-MOBILI (INDAA) = W08-RGGG
              COMPUTE W08-RGGG = W08-RGGG + 1.
      *                                  /* PASQUETTA     */
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = W08-RGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
      *                                 /* CALCOLA N. GG  */
      *                                 /* DA INIZIO ANNO   */
      *                                 /* ANNO NORMALE  */
           IF TABBIS(INDAA) = ZERO
              MOVE TABMM-3 TO TABMMBAS-TAB
              MOVE TABFEST-2 TO TABFEST-BAS-TAB
            ELSE
      *                                  /* ANNO BISESTILE */
              MOVE TABMM-4 TO TABMMBAS-TAB
              MOVE TABFESTB-2 TO TABFEST-BAS-TAB.
      *             CONTROLLO FESTE FISSE
      *             ---------------------
           IF TABFEST-BAS(GGANNO) = '1'
              COMPUTE W08-RGGG = W08-RGGG + 1
              GO TO CALC-REST.
      *                               /* CALCOLA ANNO  */
           COMPUTE  CIVIAA = INDAA + 60.

      *                              /* RICERCA MM UTILIZZANDO  */
      *                              /* N. GG DA INIZIO ANNO    */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDMM FROM 12 BY -1
              UNTIL INDMM = 1 OR
                 GGANNO GREATER TABMMBAS (INDMM).
           IF GGANNO GREATER TABMMBAS (INDMM)
              MOVE INDMM TO CIVIMM
              COMPUTE CIVIGG = GGANNO - TABMMBAS(INDMM)
              MOVE DATACIVI-PIC TO W08-RDATA
           ELSE
              MOVE 5 TO W08-RCODE.
       EX-NPW08R-RICOCIVI.
           EXIT.
280500****************************************************************
280500*  SI ACCETTA COME DATA DI PARTENZA ANCHE UNA DATA FESTIVA     *
280500*      ROUTINE RICODIFICA ANNO CIVILE - 1                      *
280500*                                                              *
280500*      INPUT :  NUMERO GIORNI INTERCORRENTI                    *
280500*               TRA LA DATA DA TROVARE E 610101                *
280500*               N.B.  TALE NUMERO DEVE ESSERE                  *
280500*                                    >=    366 (01.01.62)      *
280500*                                    <= 32.859 (31.12.2050)    *
280500*      OUTPUT : DATA NELLA FORMA AAMMGG                        *
280500*               (PRIMO GIORNO LAVORATIVO)                      *
280500*                                                              *
280500****************************************************************
280500 NPW08R-RICOCIV1.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIV1.
           IF W08-RGGG GREATER 32872
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIV1.
           COMPUTE INDAA = W08-RGGG / 366.
      *                        /* CALCOLA INDICE PER TABAA        */
      *                        /* APPROSSIMATO PER DIFETTO        */
      *                            /* RICERCA N.GIORNI IN TABAA
      *  MODIFICA DI PIER DA 39 A 90 *******
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDAA FROM INDAA BY 1
              UNTIL INDAA GREATER 90 OR
                 TABAA (INDAA) NOT LESS W08-RGGG.
           IF INDAA GREATER 90
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIV1.
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = W08-RGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
      *                                 /* CALCOLA N. GG  */
      *                                 /* DA INIZIO ANNO   */
      *                                 /* ANNO NORMALE  */
           IF TABBIS(INDAA) = ZERO
              MOVE TABMM-3 TO TABMMBAS-TAB
              MOVE TABFEST-2 TO TABFEST-BAS-TAB
            ELSE
      *                                  /* ANNO BISESTILE */
              MOVE TABMM-4 TO TABMMBAS-TAB
              MOVE TABFESTB-2 TO TABFEST-BAS-TAB.
      *                               /* CALCOLA ANNO  */
           COMPUTE  CIVIAA = INDAA + 60.

      *                              /* RICERCA MM UTILIZZANDO  */
      *                              /* N. GG DA INIZIO ANNO    */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDMM FROM 12 BY -1
              UNTIL INDMM = 1 OR
                 GGANNO GREATER TABMMBAS (INDMM).
           IF GGANNO GREATER TABMMBAS (INDMM)
              MOVE INDMM TO CIVIMM
              COMPUTE CIVIGG = GGANNO - TABMMBAS(INDMM)
              MOVE DATACIVI-PIC TO W08-RDATA
           ELSE
              MOVE 5 TO W08-RCODE.
280500 EX-NPW08R-RICOCIV1.
280500     EXIT.
      *****************************************************************
      *       ROUTINE DECODIFICA/RICODIFICA ANNO SOLARE               *
      * INPUT :                                                       *
      *       DATA NELLA FORMA AAMMGG COMPRESA TRA 620101 E 20501231  *
      *       NUMERO GIORNI DA SOMMARE ALLA DATA                      *
      *       N.B.  TALE NUMERO DEVE ESSERE                           *
      * OUTPUT :                                                      *
      *       DATA CALCOLATA NELLA FORMA AAMMGG                       *
      *                                                               *
      *****************************************************************
       NPW08R-DECORICO.
      *                                  /* CALCOLA IND. TAB. ANNI   */
      *                                  /* CALCOLA IND. TAB. MESI   */
           MOVE W08-RDATA TO DATACIVI-PIC.
           MOVE CIVIAA TO CIVIAA3.
      * INIZIO MODIFICA DI PIER DEL 30-05-95  *
           IF CIVISE = 20
              ADD 100 TO CIVIAA3.
      * FINE MODIFICA *
           COMPUTE INDAA = CIVIAA3 - ANNO-COST.
           ADD 1 TO INDAA.
           COMPUTE INDMM = TABBIS(INDAA) + CIVIMM.
           SUBTRACT 1 FROM INDAA.
           IF INDMM > 12
              COMPUTE INDMM    = INDMM - 12
              COMPUTE W08-RGGG = TABAA(INDAA) +
                                 TABMM OF TABMM-4 (INDMM) +
                                 CIVIGG + W08-RGGG
           ELSE
              COMPUTE W08-RGGG = TABAA(INDAA) +
                                 TABMM OF TABMM-3 (INDMM) +
                                 CIVIGG + W08-RGGG.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-DECORICO.
      * INIZ. PIER **
           IF W08-RGGG GREATER 32872
      * FINE  PIER **
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-DECORICO.
      *                        /* CALCOLA INDICE PER TABAA */
      *                        /* APPROSSIMATO PER DIFETTO */
           COMPUTE INDAA = W08-RGGG / 366.
      *                            /* RICERCA N. GIORNI IN TABAA */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDAA FROM INDAA BY 1
      ****    UNTIL INDAA GREATER 39 OR       *
      *    CORREZIONE PIER              *
              UNTIL INDAA GREATER 90 OR
                 TABAA (INDAA) NOT LESS W08-RGGG.
      **** IF INDAA GREATER 39          *
      *    CORREZIONE PIER              *
           IF INDAA GREATER 90
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-DECORICO.
      *                                        /* CALCOLA N. GG */
      *                                        /* DA INIZIO ANNO */
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = W08-RGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
           IF TABBIS(INDAA) = ZERO
              MOVE TABMM-3 TO TABMMBAS-TAB
           ELSE
              MOVE TABMM-4 TO TABMMBAS-TAB.
           COMPUTE  CIVIAA = INDAA + 60.
      *                              /* RICERCA MM UTILIZZANDO  */
      *                              /* N. GG DA INIZIO ANNO    */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDMM FROM 12 BY -1
              UNTIL INDMM = 1 OR
                 GGANNO GREATER TABMMBAS (INDMM).
           IF GGANNO GREATER TABMMBAS (INDMM)
              MOVE INDMM TO CIVIMM
              COMPUTE CIVIGG = GGANNO - TABMMBAS(INDMM)
              MOVE DATACIVI-PIC TO W08-RDATA
           ELSE
              MOVE 5 TO W08-RCODE.
       EX-NPW08R-DECORICO.
           EXIT.
      *****************************************************************
      *       ROUTINE RICODIFICA ANNO CIVILE AL PRIMO GIORNO          *
      *               PRECEDENTE NON FESTIVO                          *
      *                                                               *
      *       (FUNZIONI GIA' SVOLTE DALLA ROUTINE "RICOCI2")          *
      * INPUT:                                                        *
      *       NUMERO GIORNI INTERCORRENTI TRA LA DATA DA TROVARE      *
      *       E 610101                                                *
      *       N.B.  TALE NUMERO DEVE ESSERE                           *
      *                                     >=    366 (01.01.62)      *
      *                                     <= 32.859 (31.12.2050)    *
      * OUTPUT:                                                       *
      *       DATA NELLA FORMA AAMMGG (GIORNO LAVORATIVO PRECEDENTE)  *
      *                                                               *
      *****************************************************************
       NPW08R-RICOCIVI-5.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI-5.
      * MOD. PIER **
           IF W08-RGGG GREATER 32872
      * FINE PIER **
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI-5.
       CALC-REST-5.
           DIVIDE 7 INTO W08-RGGG GIVING COM-DIVIDE REMAINDER REST.
      *    CONTROLLO SABATO E DOMENICA
      *         POICHE' 1.1.61 CADE DI DOMENICA, SI HA :
      *             REST = 0 --> SABATO   (PRECEDENTE GG LAV = - 1)
      *             REST = 1 --> DOMENICA (PRECEDENTE GG LAV = - 2)
           IF REST LESS 2
              COMPUTE W08-RGGG = W08-RGGG - 1 - REST.
      * PUNTO RIGA PREC. MESSO DA PIER ***
              COMPUTE INDAA = W08-RGGG / 366.
      *                        /* CALCOLA INDICE PER TABAA        */
      *                        /* APPROSSIMATO PER DIFETTO        */
      *                            /* RICERCA N.GIORNI IN TABAA
      *  MODIFICA DI PIER   DA 39 A 90 ********
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDAA FROM INDAA BY 1
              UNTIL INDAA GREATER 90 OR
                 TABAA (INDAA) NOT LESS W08-RGGG.
           IF INDAA GREATER 90
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCIVI-5.
      * CONTROLLO PASQUETTA
           IF TABFEST-MOBILI (INDAA) = W08-RGGG
              COMPUTE W08-RGGG = W08-RGGG - 3.
      *                                  /* PASQUETTA     */
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = W08-RGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
      *                                 /* CALCOLA N. GG  */
      *                                 /* DA INIZIO ANNO   */
      *                                 /* ANNO NORMALE  */
           IF TABBIS(INDAA) = ZERO
              MOVE TABMM-3 TO TABMMBAS-TAB
              MOVE TABFEST-2 TO TABFEST-BAS-TAB
            ELSE
      *                                  /* ANNO BISESTILE */
              MOVE TABMM-4 TO TABMMBAS-TAB
              MOVE TABFESTB-2 TO TABFEST-BAS-TAB.
      *             CONTROLLO FESTE FISSE
      *             ---------------------
           IF TABFEST-BAS(GGANNO) = '1'
              COMPUTE W08-RGGG = W08-RGGG - 1
              GO TO CALC-REST-5.
      *                               /* CALCOLA ANNO  */
           COMPUTE  CIVIAA = INDAA + 60.
      *                              /* RICERCA MM UTILIZZANDO  */
      *                              /* N. GG DA INIZIO ANNO    */
           PERFORM DO-1 THRU EX-DO-1
              VARYING INDMM FROM 12 BY -1
              UNTIL INDMM = 1 OR
                 GGANNO GREATER TABMMBAS (INDMM).
           IF GGANNO GREATER TABMMBAS (INDMM)
              MOVE  INDMM  TO  CIVIMM
              COMPUTE CIVIGG = GGANNO - TABMMBAS(INDMM)
              MOVE DATACIVI-PIC TO W08-RDATA
           ELSE
              MOVE 5 TO W08-RCODE.
       EX-NPW08R-RICOCIVI-5.
           EXIT.
      *****************************************************************
      *  ROUTINE DI DECODIFICA DAL PRIMO GIORNO DELL'ANNO IN CORSO    *
      *                                                               *
      *      (FUNZIONE GIA' SVOLTA DALLA ROUTINE GGGDECO)             *
      *  INPUT :                                                      *
      *       DATA NELLA FORMA AAMMGG                                 *
      *  OUTPUT:                                                      *
      *       NUMERO GIORNI INTERCORRENTI TRA LA DATA FORNITA E       *
      *       IL PRIMO DELL'ANNO IN CORSO                             *
      *****************************************************************
       NPW08R-DECOCIVI-6.
           IF CIVIAA LESS 62
              MOVE 1      TO CIVIAA3-1
              MOVE CIVIAA TO CIVIAA3-2
           ELSE
              MOVE CIVIAA TO CIVIAA3.
           COMPUTE INDAA = CIVIAA3 - ANNO-COST.
      *                                  /* CALCOLA IND. TAB. ANNI  */
           ADD 1 TO INDAA.
           COMPUTE INDMM = TABBIS(INDAA) + CIVIMM.
           SUBTRACT 1 FROM INDAA.
      *                                  /* CALCOLA IND. TAB. MESI  */
           IF INDMM > 12
              COMPUTE INDMM = INDMM - 12
              COMPUTE W08-RGGG =
                      TABMM OF TABMM-4 (INDMM) + CIVIGG
           ELSE
              COMPUTE W08-RGGG =
                      TABMM OF TABMM-3 (INDMM) + CIVIGG.
       EX-NPW08R-DECOCIVI-6.
           EXIT.
      ***************************************************************
      *  CALCOLO FINE MESE PRECEDENTE RISPETTO AD UNA CERTA DATA    *
      *  FORNITA IN PARM.W08-RDATA (AAMMGG)                         *
      *  PARM.W08-RGGG = DATA FINE MESE PRECEDENTE                  *
      *  PARM.W08-RDATA= DATA INIZIO MESE PRECEDENTE                *
      ***************************************************************
       NPW08R-FINEMMPR.
      * INIZIO MOD. PIER *****
           MOVE 'D' TO SWSE.
      * FINE   MOD. PIER *****
           COMPUTE INDMM = CIVIMM - 1.
           IF INDMM = ZERO
              MOVE 12 TO INDMM.
           MOVE CIVIAA             TO COMAAOU.
           IF CIVIAA  LESS 60
              MOVE 20              TO COMSEOU
           ELSE
              MOVE 19              TO COMSEOU.
           MOVE TAB-FMM-MM (INDMM) TO COMMMOU.
           MOVE TAB-FMM-GG (INDMM) TO COMGGOU.
           IF COMMMOU = 12 AND COMAAOU = ZERO
              MOVE 99  TO COMAAOU
      * INIZIO MOD. PIER ***
              MOVE 19  TO COMSEOU
      * FINE   MOD. PIER ***
              GO TO FFMMPR.
           IF COMMMOU = 12
              COMPUTE COMAAOU = COMAAOU - 1.
           DIVIDE 4 INTO COMAAOU GIVING COM-DIVIDE REMAINDER COM-REST.
           IF COMMMOU = 02  AND COM-REST = ZERO
              MOVE 29 TO COMGGOU.
       FFMMPR.
           MOVE COMDTOU TO W08-RGGG.
           MOVE 1       TO COMGGOU.
      *    MOVE COMDTOU TO W08-RDATA.
       EX-NPW08R-FINEMMPR.
           EXIT.
      *****************************************************************
      *CALCOLO FINE TRIMESTRE PRECEDENTE RISPETTO AD UNA CERTA DATA   *
      *FORNITA IN PARM.W08-RDATA (AAAAMMGG)                           *
      *PARM.W08-RGGG = DATA FINE TRIMESTRE PRECEDENTE (AAMMGG)        *
      *PARM.W08-RDATA= DATA INIZIO TRIMESTRE PRECEDENTE (AAMMGG)      *
      *****************************************************************
       NPW08R-FINETRPR.
           MOVE CIVIMM TO COMMMGG-MM.
           MOVE CIVIGG TO COMMMGG-GG.
      * INIZIO MODIFICA PIER *****
           MOVE 'D'    TO SWSE.
      * FINE   MODIFICA PIER *****
       DOFTR.
           MOVE ZERO TO SW-UNO.
           PERFORM UNO THRU EX-UNO
              VARYING K FROM 1 BY 1
              UNTIL K GREATER 4 OR
                 SW-UNO NOT = ZERO.
           SUBTRACT 1 FROM K.
           MOVE CIVIAA TO COMAAOU.
           IF CIVIAA  LESS 60
              MOVE 20              TO COMSEOU
           ELSE
              MOVE 19              TO COMSEOU.
           MOVE TAB-TRIM-FF-MM (K) TO COMMMOU.
           MOVE TAB-TRIM-FF-GG (K) TO COMGGOU.
           IF COMMMOU = 12 AND COMAAOU = ZERO
              MOVE 99  TO COMAAOU
      * INIZIO MODIFICA PIER *****
              MOVE 19  TO COMSEOU
      * FINE   MODIFICA PIER *****
              GO TO FFTRPR.
           IF COMMMOU = 12
              COMPUTE COMAAOU = COMAAOU - 1.
       FFTRPR.
           MOVE COMDTOU TO W08-RGGG.
           COMPUTE DIF-DAT1  = COMMMOU - 2.
           MOVE DIF-DAT1 TO COMMMOU.
           IF DIF-DAT1 LESS ZERO
              COMPUTE COMAAOU = COMAAOU - 1
              COMPUTE COMMMOU = 13 + DIF-DAT1.
           MOVE 1 TO COMGGOU.
      *    MOVE COMDTOU TO W08-RDATA.
       EX-NPW08R-FINETRPR.
           EXIT.
       UNO.
           IF COMMMGG NOT LESS TAB-TRIM-IN(K) AND
              COMMMGG NOT GREATER TAB-TRIM-FF(K)
              MOVE 1 TO SW-UNO
              COMPUTE K = K - 1
              IF K = ZERO
                 MOVE 4 TO K.
       EX-UNO.
           EXIT.
      *****************************************************************
      *CALCOLO NUMERO TRIMESTRE DI APPARTENENZA DI UNA CERTA DATA     *
      *FORNITA IN PARM.W08-RDATA (AAMMGG)                             *
      *PARM.W08-RGGG = NUMERO TRIMESTRE DI APPARTENZA                 *
      *****************************************************************
       NPW08R-NRTRM.
           MOVE CIVIMM TO COMMMGG-MM.
           MOVE CIVIGG TO COMMMGG-GG.
       DONTR.
           PERFORM DO-1 THRU EX-DO-1
              VARYING K FROM 1 BY 1
              UNTIL K GREATER 4 OR
                 (COMMMGG NOT LESS TAB-TRIM-IN(K) AND
                  COMMMGG NOT GREATER TAB-TRIM-FF(K)).
           MOVE K TO W08-RGGG.
       EX-NPW08R-NRTRM.
           EXIT.
      *****************************************************************
      *                                                               *
      *        ANALISI VALIDITA' DELLA DATA                           *
      *                                                               *
      *****************************************************************
       NPW08R-CNTLDATA.
      *                                                                 20691000
           MOVE W08-RDATA TO DATACIVI-PIC.
E01964     IF CIVISE NOT NUMERIC                                        20711000
E01964        MOVE ZEROES TO CIVISE.                                    20712000
      *                                                                 20713000
INS        IF W08-RTIPO NOT = '29'
E01237              AND NOT = '31'
E01964              AND NOT = '10'                                      20731000
              IF CIVISE NOT EQUAL 20 AND
                 CIVISE NOT EQUAL 19
                 MOVE 1 TO W08-RCODE.
BYXSLU*    IF CIVIAA LESS 61 AND
E01964     IF W08-RTIPO NOT EQUAL '10'                                  20781000
BYXSLU        IF CIVIAA LESS 62 AND                                     20790000
                 CIVIAA GREATER 50                                      20800000
                 MOVE 1 TO W08-RCODE.                                   20810000
      *                                                                 20811000
E01964     IF W08-RTIPO EQUAL '10'                                      20812000
E01964        IF CIVISA LESS    1962                                    20813000
E01964        OR CIVISA GREATER 2099                                    20814000
E01964           MOVE 1 TO W08-RCODE.                                   20815000
      *                                                                 20816000
           IF CIVIMM LESS 01 OR
              CIVIMM GREATER 12
              MOVE 2 TO W08-RCODE.
      *                                                                 20841000
           IF CIVIGG LESS 01 OR
              CIVIGG GREATER 31
              MOVE 3 TO W08-RCODE.
      *                                                                 20871000
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-CNTLDATA.
            IF CIVIGG NOT GREATER ANATABMES (CIVIMM)
               GO TO EX-NPW08R-CNTLDATA.
            IF CIVIMM NOT = 02
               MOVE 2 TO W08-RCODE
            ELSE
               IF CIVIGG GREATER 29
                  MOVE 3 TO W08-RCODE
               ELSE
E01964*           DIVIDE 4 INTO CIVIAA GIVING COM-DIVIDE                20980000
E01964            DIVIDE 4 INTO CIVISA GIVING COM-DIVIDE                20981000
                     REMAINDER COM-REST
                  IF  CIVIGG GREATER 28                                 21000000
                     AND COM-REST NOT = ZERO
                     MOVE 3 TO W08-RCODE                                21020000
E01964            ELSE                                                  21021000
E01964               DIVIDE 100 INTO CIVISA GIVING COM-DIVIDE           21022000
E01964                        REMAINDER COM-REST                        21023000
E01964               IF  CIVIGG GREATER 28                              21024000
E01964               AND COM-REST NOT = ZERO                            21025000
E01964                  NEXT SENTENCE                                   21026000
E01964               ELSE                                               21027000
E01964                  DIVIDE 400 INTO CIVISA GIVING COM-DIVIDE        21028000
E01964                           REMAINDER COM-REST                     21029000
E01964                  IF  CIVIGG GREATER 28                           21029100
E01964                  AND COM-REST NOT = ZERO                         21029200
E01964                     MOVE 3 TO W08-RCODE.                         21029300
      *                                                                 21029400
       EX-NPW08R-CNTLDATA.
           EXIT.
      *****************************************************************
      *                                                               *
      *         INVERSIONE DELLA DATA                                 *
      *                                                               *
      *****************************************************************
       NPW08R-INVDATA.
           MOVE W08-RDATA         TO DATACIVI-PIC.
           MOVE CIVISE            TO COM-SE.
           MOVE CIVIAA            TO COM-AA.
           MOVE CIVIMM            TO COM-MM.
           MOVE CIVIGG            TO COM-GG.
           MOVE COM-DATA-GGMMAAAA TO W08-RGGG.
       EX-NPW08R-INVDATA.
           EXIT.
      *****************************************************************
      *                                                               *
      *    ANALISI FESTIVITA' DELLA DATA                              *
      *                                                               *
      *****************************************************************
       NPW08R-DATALAV.
           MOVE 6 TO W08-RCODE.
DEL   *    IF CIVIAA LESS 62
DEL   *       MOVE 1 TO W08-RCODE
DEL   *       GO TO EX-NPW08R-DATALAV.
INS        IF CIVIAA LESS 62
INS           MOVE 1      TO CIVIAA3-1
INS           MOVE CIVIAA TO CIVIAA3-2
INS        ELSE
INS           MOVE CIVIAA TO CIVIAA3.
MOD        COMPUTE INDAA = CIVIAA3 - ANNO-COST.
           ADD 1 TO INDAA.
           COMPUTE INDMM = TABBIS(INDAA) + CIVIMM.
           SUBTRACT 1 FROM INDAA.
           IF INDMM > 12
              COMPUTE INDMM = INDMM - 12
              COMPUTE CGGG     = TABAA (INDAA) +
                      TABMM OF TABMM-4 (INDMM) + CIVIGG
           ELSE
              COMPUTE CGGG     = TABAA (INDAA) +
                      TABMM OF TABMM-3 (INDMM) + CIVIGG.
           DIVIDE 7 INTO CGGG GIVING COM-DIVIDE REMAINDER COM-REST.
      *   CONTROLLO SABATO E DOMENICA
      *         POICHE' 1.1.61 CADE DI DOMENICA, SI HA :
      *             REST = 0 --> SABATO
      *             REST = 1 --> DOMENICA
           IF COM-REST LESS 2
             GO TO EX-NPW08R-DATALAV.
      *    SI CONTROLLA SE E' "IL LUNEDI' DELL'ANGELO" (PASQUETTA)
           ADD 1 TO INDAA.
           IF TABFEST-MOBILI (INDAA) = CGGG
              SUBTRACT 1 FROM INDAA
              GO TO EX-NPW08R-DATALAV.
      *    SI CONTROLLA SE E' UNA FESTIVITA' FISSA:
           SUBTRACT 1 FROM INDAA.
           COMPUTE GGANNO = CGGG - TABAA (INDAA).
           ADD 1 TO INDAA.
           IF TABBIS(INDAA) = ZERO
              SUBTRACT 1 FROM INDAA
              MOVE TABFEST-2 TO TABFEST-BAS-TAB
           ELSE
              SUBTRACT 1 FROM INDAA
              MOVE TABFESTB-2 TO TABFEST-BAS-TAB.
           IF GGANNO = ZERO OR
              TABFEST-BAS(GGANNO) = '1'
              GO TO EX-NPW08R-DATALAV.
            MOVE ZERO TO W08-RCODE.
       EX-NPW08R-DATALAV.
           EXIT.
      *****************************************************************
      *                                                               *
      *           DECODIFICA COMMERCIALE                              *
      *                                                               *
      *****************************************************************
       NPW08R-DECOCOM.
E01237     MOVE  W08-RDATA TO DATACIVI-PIC.
E01237
E01237     IF CIVIAA LESS 62
E01237        MOVE 1      TO CIVIAA3-1
E01237        MOVE CIVIAA TO CIVIAA3-2
E01237     ELSE
E01237        MOVE CIVIAA TO CIVIAA3.
E01237     COMPUTE INDAA = CIVIAA3 - ANNO-COST.
E01237*                                  /* CALCOLA IND. TAB. ANNI  */
E01237     ADD 1 TO INDAA.
E01237     IF TABBIS(INDAA) = 0
E01237        MOVE 28 TO WKS-GG-FEB
E01237     ELSE
E01237        MOVE 29 TO WKS-GG-FEB.

           MOVE W08-RDATA TO DATACIVI-PIC.
      * INIZIO MOD. PIER ASTERISCHI *******
      *    IF CIVIAA LESS 62
      *       MOVE 1 TO W08-RCODE
      *       GO TO EX-NPW08R-DECOCOM.
      * FINE   MOD. PIER ***
           MOVE CIVIAA TO GGANNO.
      * INIZIO MOD. PIER *******
           IF GGANNO LESS 60
              ADD 100 TO GGANNO.
      * FINE   MOD. PIER *******
           COMPUTE W08-RGGG = (GGANNO - ANNO-COST) * 360.
           MOVE CIVIMM TO GGANNO.
           COMPUTE W08-RGGG = W08-RGGG + ((GGANNO - 1) * 30).
E01237*    IF CIVIGG LESS 28 GO TO COM2.
E01237     IF CIVIGG LESS WKS-GG-FEB GO TO COM2.
           IF CIVIMM = 02 GO TO COM1.
           IF CIVIGG LESS 30 GO TO COM2.
       COM1.
           COMPUTE W08-RGGG = W08-RGGG + 30.
           GO TO EX-NPW08R-DECOCOM.
       COM2.
           COMPUTE W08-RGGG = W08-RGGG + CIVIGG.
       EX-NPW08R-DECOCOM.
           EXIT.
      *****************************************************************
      *                                                               *
      *           RICODIFICA COMMERCIALE                              *
      *                                                               *
      *****************************************************************
       NPW08R-RICOCOM.
           IF W08-RGGG LESS 366
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCOM.
      * MOD. PIER **
           IF W08-RGGG GREATER 32872
      * FINE PIER **
              MOVE 4 TO W08-RCODE
              GO TO EX-NPW08R-RICOCOM.
           DIVIDE 360 INTO W08-RGGG GIVING COM-DIVIDE REMAINDER REST.
           COMPUTE QUOZ = W08-RGGG / 360.
           IF REST = ZERO
              COMPUTE GGANNO = QUOZ + (ANNO-COST - 1)
              MOVE GGANNO TO CIVIAA
              MOVE 12     TO CIVIMM
              MOVE 31     TO CIVIGG
              GO TO COM7.
           COMPUTE GGANNO = QUOZ + ANNO-COST.
           MOVE GGANNO TO CIVIAA.
           PERFORM DO-1 THRU EX-DO-1
              VARYING K FROM 12 BY -1
              UNTIL K LESS 1 OR
              REST GREATER TABCOM(K) OR
              REST = TABCOM(K).
           IF REST GREATER TABCOM(K) GO TO COM3A.
           IF REST = TABCOM(K) GO TO COM4.
       COM3A.
           MOVE K TO GGANNO.
           MOVE GGANNO TO CIVIMM.
           COMPUTE CIVIGG = REST - TABCOM(K).
           GO TO COM7.
       COM4.
           COMPUTE K = K - 1.
           MOVE K TO GGANNO.
           MOVE GGANNO TO CIVIMM.
           IF CIVIMM = 04 GO TO COM6.
           IF CIVIMM = 06 GO TO COM6.
           IF CIVIMM = 09 GO TO COM6.
           IF CIVIMM = 11 GO TO COM6.
           IF CIVIMM NOT = 02 GO TO COM5.
           DIVIDE 4 INTO CIVIAA GIVING COM-DIVIDE REMAINDER COM-REST.
           IF COM-REST = ZERO MOVE 29 TO CIVIGG
           ELSE MOVE 28 TO CIVIGG.
           GO TO COM7.
       COM5.
           MOVE 31 TO CIVIGG.
           GO TO COM7.
       COM6.
           MOVE 30 TO CIVIGG.
           GO TO COM7.
       COM7.
           MOVE DATACIVI-PIC TO W08-RDATA.
       EX-NPW08R-RICOCOM.
           EXIT.
      *****************************************************************
      *                                                               *
      *    SOTTRAI AD UNA DATA "ENNE" GIORNI FISSI                    *
      *                                                               *
      *****************************************************************
       NPW08R-DECORICO-6.
           MOVE W08-RGGG TO CGGG.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-DECORICO-6.
           COMPUTE W08-RGGG = W08-RGGG - CGGG.
           PERFORM NPW08R-RICOSOL THRU EX-NPW08R-RICOSOL.
       EX-NPW08R-DECORICO-6.
           EXIT.
      *****************************************************************
      *                                                               *
      *    SOMMA AD UNA DATA "ENNE" GIORNI E RICODIFICA CIVILE        *
      *                                                               *
      *****************************************************************
       NPW08R-RICOLAV3.
           IF W08-RGGG GREATER ZERO
              MOVE W08-RGGG TO CGGG
              PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI
              IF W08-RCODE NOT = ZERO
                 GO TO EX-NPW08R-RICOLAV3
              ELSE
                 COMPUTE W08-RGGG = W08-RGGG + CGGG
                 PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI.
       EX-NPW08R-RICOLAV3.
           EXIT.
      *****************************************************************
      *                                                               *
      *  SOTTRAI AD UNA DATA "ENNE" GIORNI E RICODIFICA CIVILE        *
      *                                                               *
      *****************************************************************
       NPW08R-RICOLAV4.
           IF W08-RGGG GREATER ZERO
              MOVE W08-RGGG TO CGGG
              PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI
              IF W08-RCODE NOT = ZERO
                 GO TO EX-NPW08R-RICOLAV4
              ELSE
                 COMPUTE W08-RGGG = W08-RGGG - CGGG
                 PERFORM NPW08R-RICOCIVI-5 THRU EX-NPW08R-RICOCIVI-5.
       EX-NPW08R-RICOLAV4.
           EXIT.
      *****************************************************************
      *                                                               *
      *    SOMMA AD UNA DATA "ENNE" GIORNI LAVORATIVI                 *
      *                                                               *
      *****************************************************************
       NPW08R-RICOLAV5.
           IF W08-RGGG GREATER ZERO
              MOVE W08-RGGG TO CGGG
              PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI
              IF W08-RCODE NOT = ZERO
                 GO TO EX-NPW08R-RICOLAV5
      * CONTROLLO SABATO E DOMENICA
      *             REST = 0 --> SABATO   (IESIMO GG LAV = + 2)
      *             REST = 1 --> DOMENICA (IESIMO GG LAV = + 1)
              ELSE
                 PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI
                 IF W08-RCODE NOT = ZERO
                    GO TO EX-NPW08R-RICOLAV5
                 ELSE
                    PERFORM DUE THRU EX-DUE
                       VARYING IND-GGPIU FROM 1 BY 1
                       UNTIL IND-GGPIU GREATER CGGG
                    PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI.
       EX-NPW08R-RICOLAV5.
           EXIT.
       DUE.
      *                                /* 1 ALLA VOLTA PER   */
      *                                /* CTL FESTE MOBILI   */
           ADD 1 TO W08-RGGG.
           PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-DUE.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
       EX-DUE.
           EXIT.
      *****************************************************************
      *                                                               *
      *  SOTTRAI AD UNA DATA "ENNE" GIORNI LAVORATIVI                 *
      *                                                               *
      *****************************************************************
       NPW08R-RICOLAV6.
           IF W08-RGGG GREATER ZERO
              MOVE W08-RGGG TO CGGG.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-RICOLAV6.
      * CONTROLLO SABATO E DOMENICA
      *                REST = 0 --> SABATO   (IESIMO GG LAV = - 2)
      *                REST = 1 --> DOMENICA (IESIMO GG LAV = - 1)
           DIVIDE 7 INTO W08-RGGG GIVING COM-DIVIDE REMAINDER COM-REST.
           IF COM-REST LESS 2
              COMPUTE W08-RGGG = W08-RGGG - 1 - COM-REST.
           PERFORM NPW08R-RICOCIVI-5 THRU EX-NPW08R-RICOCIVI-5.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-RICOLAV6
           ELSE
              PERFORM TRE THRU EX-TRE
                 VARYING IND-GGPIU FROM CGGG BY -1
                 UNTIL IND-GGPIU LESS 1
              PERFORM NPW08R-RICOCIVI-5 THRU EX-NPW08R-RICOCIVI-5.
       EX-NPW08R-RICOLAV6.
           EXIT.
       TRE.
      *                                /* 1 ALLA VOLTA PER   */
      *                                /* CTL FESTE MOBILI   */
           SUBTRACT 1 FROM W08-RGGG.
           PERFORM NPW08R-RICOCIVI-5 THRU EX-NPW08R-RICOCIVI-5.
           IF W08-RCODE NOT = ZERO
              GO TO EX-TRE.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
       EX-TRE.
           EXIT.
      *****************************************************************
      *                                                               *
280500*    SOMMA AD UNA DATA "ENNE" GIORNI LAVORATIVI                 *
280500*    ANCHE SE LA DATA DI PARTENZA E' FESTIVA                    *
280500*****************************************************************
280500 NPW08R-RICOLAV7.
           IF W08-RGGG GREATER ZERO
              MOVE W08-RGGG TO CGGG
              PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI
              IF W08-RCODE NOT = ZERO
                 GO TO EX-NPW08R-RICOLAV7
      * CONTROLLO SABATO E DOMENICA
      *             REST = 0 --> SABATO   (IESIMO GG LAV = + 2)
      *             REST = 1 --> DOMENICA (IESIMO GG LAV = + 1)
              ELSE
                 PERFORM NPW08R-RICOCIV1 THRU EX-NPW08R-RICOCIV1
                 IF W08-RCODE NOT = ZERO
                    GO TO EX-NPW08R-RICOLAV7
                 ELSE
                    PERFORM DUE THRU EX-DUE
                       VARYING IND-GGPIU FROM 1 BY 1
                       UNTIL IND-GGPIU GREATER CGGG
                    PERFORM NPW08R-RICOCIVI THRU EX-NPW08R-RICOCIVI.
280500 EX-NPW08R-RICOLAV7.
280500     EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DIFFERENZA GIORNI CIVILI TRA DUE RATE                *
      *                                                               *
      *****************************************************************
       NPW08R-DIFFCIVI.
           MOVE DIF-DAT1 TO W08-RDATA.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-DIFFCIVI.
           MOVE W08-RGGG TO CGGG.
           MOVE DIF-DAT2 TO W08-RDATA.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-DIFFCIVI.
           MOVE ZERO TO W08-RDATA.
           IF SW-ERR = ZERO
              COMPUTE W08-RGGG = CGGG - W08-RGGG
           ELSE
              COMPUTE W08-RGGG = CGGG + W08-RGGG
              MOVE 8 TO W08-RCODE.
       EX-NPW08R-DIFFCIVI.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DIFFERENZA GIORNI COMMERCIALI TRA DUE RATE           *
      *                                                               *
      *****************************************************************
       NPW08R-DIFFCOM.
           MOVE DIF-DAT1 TO W08-RDATA.
           PERFORM NPW08R-DECOCOM THRU EX-NPW08R-DECOCOM.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-DIFFCOM.
           MOVE W08-RGGG TO CGGG.
           MOVE DIF-DAT2 TO W08-RDATA.
           PERFORM NPW08R-DECOCOM THRU EX-NPW08R-DECOCOM.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-DIFFCOM.
           MOVE ZERO TO W08-RDATA.
           IF SW-ERR = ZERO
              COMPUTE W08-RGGG = CGGG - W08-RGGG
           ELSE
              COMPUTE W08-RGGG = CGGG + W08-RGGG
              MOVE 8 TO W08-RCODE.
       EX-NPW08R-DIFFCOM.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA SUCCESSIVA DI N MESI A DAT.                     *
      *  IL NUMERO DEI MESI SI TROVA IN GIOR.                         *
      *  SE GIOR E' NEGATIVO VIENE RICHIAMATA LA ROUTINE FUNZ23.      *
      *  LA DATA CALCOLATA VIENE MESSA IN GIOR.                       *
      *  SE DAT E' DI FINE MESE ANCHE GIOR SARA' DI FINE MESE.        *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ22.
      * MODIFICA PIER     *
           MOVE 'D'       TO SWSE.
      * FINE MODIFICA     *
           IF W08-RGGG LESS ZERO
              PERFORM NPW08R-FUNZ23 THRU EX-NPW08R-FUNZ23.
           MOVE W08-RDATA TO AMG.
E01237     MOVE  AMG TO DATACIVI-PIC.
E01237
E01237     IF CIVIAA LESS 62
E01237        MOVE 1      TO CIVIAA3-1
E01237        MOVE CIVIAA TO CIVIAA3-2
E01237     ELSE
E01237        MOVE CIVIAA TO CIVIAA3.
E01237     COMPUTE INDAA = CIVIAA3 - ANNO-COST.
E01237*                                  /* CALCOLA IND. TAB. ANNI  */
E01237     ADD 1 TO INDAA.
           COMPUTE MM1 = MM + W08-RGGG.
       LOOP22.
           IF MM1 GREATER 12
              COMPUTE MM1 = MM1 - 12
              IF AA OF AMG1 = 99
                 MOVE ZERO TO AA OF AMG1
      * MODIFICA PIER        *
                 GO TO LOOP22
      * FINE MODIFICA        *
              ELSE
                 COMPUTE  AA OF AMG1 = AA OF AMG1 + 1
                 GO TO LOOP22.
           MOVE '0' TO SWFM.
E01237     IF MM OF AMG1 NOT EQUAL 2
E01237        IF TAB-FMM-GG (MM OF AMG1) = GG OF AMG1
E01237           GO TO FM22
E01237        ELSE
E01237           GO TO OUTFM22
E01237        END-IF
E01237     ELSE
E01237        IF TABBIS(INDAA) = 0
E01237           IF GG OF AMG1 = 28
E01237              GO TO FM22
E01237           ELSE
E01237              GO TO OUTFM22
E01237        ELSE
E01237           IF GG OF AMG1 = 29
E01237              GO TO FM22
E01237           ELSE
E01237              GO TO OUTFM22.
E01237*    IF TAB-FMM-GG (MM OF AMG1) = GG OF AMG1
E01237*       GO TO FM22.
E01237*    IF TAB-FMM-GG(MM1) LESS GG OF AMG1 GO TO FM22.
E01237*    GO TO OUTFM22.
      *        /* SITUAZIONE DI FINE MESE  */
       FM22.
           MOVE TAB-FMM-GG(MM1) TO GG OF AMG1.
           MOVE '1' TO SWFM.
       OUTFM22.
      *           /* CONTROLLO SE NUOVA DATA = FINE 02 BISESTILE */
      *           /* VA BENE ANCHE PER IL 2000 ? */
           DIVIDE 4 INTO AA OF AMG1 GIVING COM-DIVIDE REMAINDER COM-REST
      * MODIFICA PIER   *
           IF SWFM = '1' AND MM1 = 2 AND AA OF AMG1 NOT EQUAL ZERO
      * FINE MODIFICA   *
              IF COM-REST = ZERO
                 MOVE 29 TO GG OF AMG1.
E01237     IF AA OF AMG1 LESS 60
E01237        MOVE 20 TO SE OF AMG1.
E01237     IF SWFM = '1' AND MM1 = 2
E01237             AND SE OF AMG1 = 20 AND AA OF AMG1 EQUAL ZERO
E01237        IF COM-REST = ZERO
E01237           MOVE 29 TO GG OF AMG1.
           MOVE MM1 TO MM OF AMG1.
      * MODIFICA PIER ***
E01237*    IF AA OF AMG1 LESS 60
E01237*       MOVE 20 TO SE OF AMG1.
      * FINE MOD.******
           MOVE AMG TO W08-RGGG.
       EX-NPW08R-FUNZ22.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA PRECEDNTE DI N MESI A DAT                       *
      *  ARROTONDATA AL PRIMO DEL MESE                                *
      *  IL NUMERO DEI MESI SI TROVA IN GIOR.                         *
      *  LA DATA CALCOLATA VIENE MESSA IN GIOR.                       *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ23.
           IF W08-RGGG LESS ZERO
              MOVE 99 TO W08-RCODE
              GO TO EX-NPW08R-FUNZ23.
           MOVE W08-RDATA TO AMG.
           MOVE W08-RGGG  TO MM1.
           MOVE ZEROES    TO AA1.
           IF W08-RGGG NOT LESS  12
              DIVIDE 12 INTO W08-RGGG GIVING AA1 REMAINDER MM1.
           IF MM1 NOT LESS MM OF AMG1
              ADD 12 TO MM OF AMG1
              ADD 1  TO AA1.
      * A QUESTO PUNTO MM1 CONTIENE I MESI DA SOTTRARRE < 12,
      * E AA1 CONTIENE GLI ANNI DA SOTTRARRE.
           SUBTRACT MM1 FROM MM OF AMG1.
           SUBTRACT AA1 FROM ANNO OF AMG2.
           MOVE 1 TO GG OF AMG1.
           MOVE AMG TO W08-RGGG.
       EX-NPW08R-FUNZ23.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA SE LA DATA E' DI FINE MESE                           *
      *  SE DAT E' DI FINE MESE IN GIOR SI TROVA '1'                  *
      *  SE DAT NON E' DI FINE MESE IN GIOR SI TROVA '-1'             *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ24.
           MOVE W08-RDATA TO AMG.
           MOVE 9 TO W08-RCODE.
           DIVIDE 4 INTO AA OF AMG1 GIVING COM-DIVIDE REMAINDER COM-REST
           IF COM-REST = ZERO AND MM OF AMG1 = 2
              IF GG OF AMG1 = 29
                 MOVE ZERO TO W08-RCODE
                 GO TO EX-NPW08R-FUNZ24
              ELSE
                 MOVE 29 TO GG OF AMG1
                 MOVE AMG TO W08-RGGG
                 GO TO EX-NPW08R-FUNZ24.
           IF TAB-FMM-GG(MM OF AMG1) = GG OF AMG1
              MOVE ZERO TO W08-RCODE
           ELSE
              MOVE TAB-FMM-GG(MM OF AMG1) TO GG OF AMG1
              MOVE AMG TO W08-RGGG.
       EX-NPW08R-FUNZ24.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA SE LA DATA E' DI FINE TRIMESTRE                      *
      *  SE DAT E' DI FINE MESE IN GIOR SI TROVA '1'                  *
      *  SE DAT NON E' DI FINE MESE IN GIOR SI TROVA '-1'             *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ25.
           MOVE W08-RDATA TO AMG.
           MOVE 10 TO W08-RCODE.
           DIVIDE 3 INTO MM OF AMG1 GIVING COM-DIVIDE REMAINDER COM-REST
           IF COM-REST = ZERO AND ANATABMES (MM OF AMG1) = GG OF AMG1
              MOVE ZERO TO W08-RCODE
           ELSE
              DIVIDE 3 INTO MM OF AMG1 GIVING COM-DIVIDE
                 REMAINDER COM-REST
              PERFORM DO-2 THRU EX-DO-2
                 UNTIL COM-REST NOT = ZERO
              MOVE ANATABMES (MM OF AMG1) TO GG OF AMG1
              MOVE AMG TO W08-RGGG.
       EX-NPW08R-FUNZ25.
           EXIT.
       DO-2.
           ADD 1 TO MM OF AMG1.
           DIVIDE 3 INTO MM OF AMG1 GIVING COM-DIVIDE
              REMAINDER COM-REST.
       EX-DO-2.
           EXIT.
      *****************************************************************
      *                                                               *
      *  TRASFORMA LA DATA DALLA FORMA AAMMGG                         *
      *         NELLA FORMA AAGGG                                     *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ26.
           MOVE ZERO TO W08-RCODE.
           MOVE W08-RDATA TO AMG.
           PERFORM NPW08R-DECOCIVI-6 THRU EX-NPW08R-DECOCIVI-6.
           IF W08-RCODE = ZERO
              COMPUTE W08-RGGG = W08-RGGG + (AA OF AMG1 * 1000).
       EX-NPW08R-FUNZ26.
           EXIT.
      *****************************************************************
      *                                                               *
      *  TRASFORMA LA DATA DALLA FORMA AAGGG                          *
      *         NELLA FORMA AAMMGG                                    *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ27.
           MOVE ZERO TO W08-RCODE.
           MOVE W08-RGGG TO AAGGG.
           COMPUTE  W08-RDATA = (AA OF AAGGG1 * 10000) + 0101.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-FUNZ27.
           COMPUTE W08-RGGG = W08-RGGG - 1 + GGG OF AAGGG1.
           PERFORM NPW08R-RICOSOL THRU EX-NPW08R-RICOSOL.
           MOVE AAGGG TO W08-RGGG.
       EX-NPW08R-FUNZ27.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA IL GIORNO DELLA SETTIMANA                            *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ28.
           MOVE ZERO TO W08-RCODE.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-FUNZ28.
           DIVIDE 7 INTO W08-RGGG GIVING COM-DIVIDE REMAINDER REST.
      *           /*--->  CONTROLLO SABATO E DOMENICA
      *                   POICHE' 1.1.61 CADE DI DOMENICA, SI HA :
      *                   REST = 0 --> SABATO
      *                   REST = 1 --> DOMENICA
      *                   REST = 2 --> LUNEDI'
      *                   REST = 3 --> MARTEDI'
      *                   REST = 4 --> MERCOLEDI'
      *                   REST = 5 --> GIOVEDI'
      *                   REST = 6 --> VENERDI' <--------*/
           IF REST GREATER 1
              COMPUTE W08-RGGG = REST - 1
           ELSE
              COMPUTE W08-RGGG = REST + 6.
       EX-NPW08R-FUNZ28.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA GGMMAA IN SSAAMMGG                           *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ29.
           MOVE W08-RDATA TO DATACIVI-PIC.
           IF CIVIAA LESS 62
              ADD 20000000 TO W08-RDATA
           ELSE
              ADD 19000000 TO W08-RDATA.
       EX-NPW08R-FUNZ29.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA SSAAMMGG IN GGMMAA                           *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ30.
           MOVE W08-RDATA TO DATACIVI-PIC.
           MOVE CIVIAA TO COM-D-AA.
           MOVE CIVIMM TO COM-D-MM.
           MOVE CIVIGG TO COM-D-GG.
           MOVE COM-D-GMA TO W08-RDATA.
       EX-NPW08R-FUNZ30.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA GGMMAA IN AAMMGG                             *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ31.
           MOVE ZERO TO W08-RCODE.
           PERFORM NPW08R-DECOCIVI-6 THRU EX-NPW08R-DECOCIVI-6.
           IF W08-RCODE = ZERO
              COMPUTE W08-RGGG = W08-RGGG + (CIVIAA    * 1000).
       EX-NPW08R-FUNZ31.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA AAGGG  IN GGMMAA                             *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ32.
           MOVE ZERO TO W08-RCODE.
           MOVE W08-RGGG TO AAGGG.
           COMPUTE W08-RDATA = (AA OF AAGGG1 * 10000) + 0101.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-FUNZ32.
           COMPUTE W08-RGGG = W08-RGGG - 1 + GGG OF AAGGG1.
           PERFORM NPW08R-RICOSOL THRU EX-NPW08R-RICOSOL.
           MOVE AAGGG TO W08-RGGG.
       EX-NPW08R-FUNZ32.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA SSAAMMGG IN AAGGG                            *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ33.
           MOVE ZERO TO W08-RCODE.
           MOVE W08-RDATA TO AMG.
           PERFORM NPW08R-DECOCIVI-6 THRU EX-NPW08R-DECOCIVI-6.
           IF W08-RCODE = ZERO
              COMPUTE  W08-RGGG = W08-RGGG + (AA OF AMG1 * 1000).
       EX-NPW08R-FUNZ33.
           EXIT.
      *****************************************************************
      *                                                               *
      *  CALCOLA DATA DA AAGGG IN SSAAMMGG                            *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ34.
           MOVE ZERO TO W08-RCODE.
           MOVE W08-RGGG TO AAGGG.
           COMPUTE W08-RDATA = (AA OF AAGGG1 * 10000) + 0101.
           PERFORM NPW08R-DECOCIVI THRU EX-NPW08R-DECOCIVI.
           IF W08-RCODE NOT = ZERO
              GO TO EX-NPW08R-FUNZ34.
           COMPUTE W08-RGGG = W08-RGGG - 1 + GGG OF AAGGG1.
           PERFORM NPW08R-RICOSOL THRU EX-NPW08R-RICOSOL.
           MOVE AAGGG TO W08-RGGG.
       EX-NPW08R-FUNZ34.
           EXIT.
      *****************************************************************
      *                                                               *
      *        CALCOLA DEL COMPLEMENTO DELLA DATA E L'ORA             *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ35.
           COMPUTE W08-RDATA = 99999999 - W08-RDATA.
           COMPUTE W08-RGGG  = 99999999 - W08-RGGG.
       EX-NPW08R-FUNZ35.
           EXIT.
      *****************************************************************
      *                                                               *
      *        CALCOLA IL FINE MESE DELLA DATA CORRENTE               *
      *                                                               *
      *****************************************************************
       NPW08R-FUNZ36.
           MOVE W08-RDATA TO DATACIVI-PIC.
           MOVE 31 TO CIVIGG.
           MOVE DATACIVI-PIC TO W08-RDATA.
           PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA.
           IF W08-RCODE NOT EQUAL ZERO
              MOVE ZERO        TO W08-RCODE
              MOVE W08-RDATA TO DATACIVI-PIC
              MOVE 30 TO CIVIGG
              MOVE DATACIVI-PIC TO W08-RDATA
              PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
      * INIZIO MOD. PIER ***
              IF W08-RCODE NOT EQUAL ZERO
                 MOVE ZERO        TO W08-RCODE
                 MOVE W08-RDATA TO DATACIVI-PIC
                 MOVE 29 TO CIVIGG
                 MOVE DATACIVI-PIC TO W08-RDATA
                 PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA
                 IF W08-RCODE NOT EQUAL ZERO
                    MOVE ZERO        TO W08-RCODE
                    MOVE W08-RDATA TO DATACIVI-PIC
                    MOVE 28 TO CIVIGG
                    MOVE DATACIVI-PIC TO W08-RDATA
                    PERFORM NPW08R-CNTLDATA THRU EX-NPW08R-CNTLDATA.
      * FINE   MOD. PIER ***
           PERFORM NPW08R-DTOUT    THRU EX-NPW08R-DTOUT.
       EX-NPW08R-FUNZ36.
           EXIT.
      *****************************************************************
      *                                                               *
      *  PREPARA  FORMATI DATE PER OUTPUT                             *
      *                                                               *
      *****************************************************************
       NPW08R-DTOUT.
      *                                                                 28371000
           IF W08-RCODE NOT EQUAL ZERO
              GO TO EX-NPW08R-DTOUT.
E01964     IF W08-RTIPO EQUAL '10'                                      28392000
E01964        MOVE CIVISE  TO CSEC                                      28393000
E01964        MOVE CIVIAA  TO CANNO.                                    28394000
      *                                                                 28395000
E01964     IF W08-RTIPO NOT EQUAL '10'                                  28396000
E01964        MOVE ZEROES   TO CAA                                      28397000
E01964        MOVE CIVIAA   TO CANNO.                                   28398000
    ****   MOVE ZEROES     TO CAA.                                      28400000
    ****   MOVE CIVIAA     TO CANNO.                                    28410000
      *                                                                 28420000
      * MOD. PIER *
E01237*    IF SWSE NOT EQUAL 'D'
      * FINE MOD. *
E01964     IF W08-RTIPO NOT EQUAL '10'                                  28451000
              IF CIVIAA LESS 62
                 MOVE 20      TO CSEC
              ELSE
                 MOVE 19      TO CSEC.
      * MOD. PIER *
E01964     IF W08-RTIPO NOT EQUAL '10'                                  28511000
              IF SWSE NOT EQUAL 'D'                                     28520000
                 IF CIVIAA LESS 60                                      28540000
                    MOVE W08-RDATA     TO COM-DATA6                     28550000
                    MOVE '20'          TO COM-DATA2                     28560000
                    MOVE COM-DATA8-RED TO W08-RDATA                     28570000
                 ELSE                                                   28580000
                    MOVE W08-RDATA     TO COM-DATA6                     28590000
                    MOVE '19'          TO COM-DATA2                     28600000
                    MOVE COM-DATA8-RED TO W08-RDATA.                    28610000
      * MOD. PIER *
           MOVE SPACE TO SWSE.
      * FINE MOD. *
      *********************************************
      *  DATA IN FORMATO GG/MM/AAAA               *
      *********************************************
           MOVE    CIVIGG     TO   DT5GIO.
           MOVE    CIVIMM     TO   DT5MES.
           MOVE    CSEC       TO   DT5SEC.
           MOVE    CIVIAA     TO   DT5ANN.
           MOVE    '/'        TO   FIL1 FIL2.
           MOVE    DT5        TO   W08-RDT5.

      *********************************************
      *  DATA IN FORMATO GG/MM/AA                 *
      *********************************************
           MOVE    CIVIGG     TO   DT6GIO.
           MOVE    CIVIMM     TO   DT6MES.
           MOVE    CIVIAA     TO   DT6ANN.
           MOVE    '/'        TO   FIL3 FIL4.
           MOVE    DT6        TO   W08-RDT6.

      *********************************************
      *  DATA IN FORMATO MM/GG/AA                 *
      *********************************************
           MOVE    CIVIGG     TO   DT15GIO.
           MOVE    CIVIMM     TO   DT15MES.
           MOVE    CIVIAA     TO   DT15ANN.
           MOVE    '/'        TO   FIL5 FIL6.
           MOVE    DT15       TO   W08-RDT15.

      *********************************************
      *  DATA IN FORMATO GG MMMMMMMMM AAAA        *
      *********************************************
           MOVE CIVIGG           TO   DT10GIO.
           MOVE ELEM-MESI(CIVIMM) TO  DT10MES.
           MOVE CSEC             TO   DT10SEC.
           MOVE CIVIAA           TO   DT10ANN.
           MOVE DT10             TO   W08-RDT10.

      *********************************************
      *  DATA IN FORMATO GG MMM AA                *
      *********************************************
           MOVE CIVIGG           TO   DT11GIO.
           MOVE MESE-B(CIVIMM)   TO   DT11MES.
           MOVE CIVIAA           TO   DT11ANN.
           MOVE DT11             TO   W08-RDT11.

      ************************************************
      *  DATA IN FORMATO GGGGGGGGG GG MMMMMMMMM AAAA *
      ************************************************
           PERFORM GIO-SET THRU GIO-SET-EX.
           MOVE ELEM-GIORNI(IND)    TO   DT12GIS.
           MOVE CIVIGG              TO   DT12GIO.
           MOVE ELEM-MESI(CIVIMM)   TO   DT12MES.
           MOVE CSEC                TO   DT12SEC.
           MOVE CIVIAA              TO   DT12ANN.
           MOVE DT12                TO   W08-RDT12.

      *********************************************
      *  DATA IN FORMATO GGG GG MMM AA            *
      *********************************************
           MOVE GIORNI-B(IND)    TO   DT13GIS.
           MOVE CIVIGG           TO   DT13GIO.
           MOVE MESE-B(CIVIMM)   TO   DT13MES.
           MOVE CIVIAA           TO   DT13ANN.
           MOVE DT13             TO   W08-RDT13.
       EX-NPW08R-DTOUT.
           EXIT.
      *********************************************
      *  ROUTINE GIORNO DELLA SETTIMANA           *
      *********************************************
       GIO-SET.
      *                                                                 29341000
E01964     IF W08-RTIPO EQUAL '10'                                      29342000
E01964        IF CIVISA GREATER 1999                                    29343000
E01964           MOVE 1      TO CIVIAA3-1                               29344000
E01964           MOVE CIVIAA TO CIVIAA3-2                               29345000
E01964        ELSE                                                      29346000
E01964           MOVE CIVIAA TO CIVIAA3.                                29347000
      *                                                                 29348000
E01964     IF W08-RTIPO NOT EQUAL '10'                                  29349000
              IF CIVIAA LESS 62                                         29350000
                 MOVE 1      TO CIVIAA3-1                               29360000
                 MOVE CIVIAA TO CIVIAA3-2                               29370000
              ELSE                                                      29380000
                 MOVE CIVIAA TO CIVIAA3.                                29390000
      *                                                                 29391000
           COMPUTE INDAA = CIVIAA3 - ANNO-COST.
      *                                  /* CALCOLA IND. TAB. ANNI  */
           ADD 1 TO INDAA.
           COMPUTE INDMM = TABBIS(INDAA) + CIVIMM.
      *                                  /* CALCOLA IND. TAB. MESI  */
           SUBTRACT 1 FROM INDAA.
           IF INDMM > 12
              COMPUTE INDMM = INDMM - 12
              COMPUTE CGGG     = TABAA (INDAA) +
                      TABMM OF TABMM-4 (INDMM) + CIVIGG
           ELSE
              COMPUTE CGGG     = TABAA (INDAA) +
                      TABMM OF TABMM-3 (INDMM) + CIVIGG.
           DIVIDE 7 INTO CGGG     GIVING COM-DIVIDE REMAINDER REST.
      *           /*--->  CONTROLLO SABATO E DOMENICA
      *                   POICHE' 1.1.61 CADE DI DOMENICA, SI HA :
      *                   REST = 0 --> SABATO
      *                   REST = 1 --> DOMENICA
      *                   REST = 2 --> LUNEDI'
      *                   REST = 3 --> MARTEDI'
      *                   REST = 4 --> MERCOLEDI'
      *                   REST = 5 --> GIOVEDI'
      *                   REST = 6 --> VENERDI' <--------*/
           IF REST GREATER 1
              COMPUTE CGGG     = REST - 1
           ELSE
              COMPUTE CGGG     = REST + 6.
           MOVE CGGG          TO IND.
      *    COMPUTE C2 = CAA - DAAR.
      *    COMPUTE C1 = C2 - 2.
      *    DIVIDE C1 BY 4 GIVING QUOZ    REMAINDER REST.
      *    IF (REST NOT EQUAL ZERO)
      *       ADD 1        TO QUOZ.
      *    COMPUTE N-GG = (365 * (C2 - 1) + QUOZ  + CGGG)
      *    DIVIDE N-GG BY 7 GIVING QUOZ  REMAINDER REST.
      *    ADD 1    TO REST.
      *    MOVE REST    TO IND.
       GIO-SET-EX.
           EXIT.
EXPAND*--------------------------------------------------------
EXPAND* FINE ESPLOSIONE COPY NPW08R
EXPAND*--------------------------------------------------------
-
