      *----------------------------------------------------------
      * Copybook with unusual featueres that have caused problems in cb2xml:
      * * Hex constanstants
      * * fields starting with a number
      * * Filler redefines
      * * 88 levels with hex values
      *-------------------------------------------------------------
002400  01 COBL-XREF-REC.                                               00003210
002450     05  XREF-HEADER.                                             00003306
             07 another-level.
002600         10  CB-SORT-CODE           PIC X VALUE X'FF'.                        
002800         10  STATISTICAL-IND        PIC X.                        00003706
003000         10  HDR-PORTION-LENGTH     PIC XX.                       00003906
003200         10  POL-BODY-LENGTH        PIC XX.                       00004106
003300         10  XBOLG-HEX REDEFINES                                  00004206
003350                  POL-BODY-LENGTH PIC 9(02) COMP.                 00004306
003500         10  OVERFLOW-LENGTH        PIC XX.                       00004506
               10  FILLER      REDEFINES OVERFLOW-LENGTH.
                   15 1st-byte            pic 9.
                   15 2nd-byte            pic 9.
003700         10  ARRANGEMENT-TYPE       PIC X.                        00004706
               10  FILLER      REDEFINES ARRANGEMENT-TYPE.
                   15 1-field             pic 9.
003900         10  ARRANGEMENT-NUMB       PIC 9(05) COMP-3.             00004906
004100         10  PAY-POINT-NUMB         PIC 9(03) COMP-3.             00005106
004300         10  XRCCT                  PIC 9(01) COMP-3.             00005306
004305         10  RECORD-COUNT           PIC X.                        00005412
004310             88 ONEREC              VALUE X'F1'.                  00005512
004330             88 FIRSTREC            VALUE X'01'.                  00005612
004500         10  CSO                    PIC X.                        00005806
004700         10  STATUS-CD              PIC X.                        00006006
