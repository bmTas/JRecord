      01  CompFields.
         03 NumA             pic --,---,---,---,---,--9.99.
         03 NumB             pic 9V99.
         03 NumC             pic 999.
         03 text             pic x(20).               
         03 NumD             pic VPPP999.
         03 NumE             pic 999PPP.
         03 float                          comp-1.
         03 double                         comp-2.  
         03 filler.
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 1 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
         05  SFIELD-SEP            PIC S9(7)V99 SIGN IS                       
                                              LEADING SEPARATE.          
       
         03 88-levels.
000025     05  REN-RETURNED-DATE.
000026         10  REN-RETURNED-YEAR     PIC 9(2).
000027         10  REN-RETURNED-MONTH    PIC 9(2).
000028             88  VALID-MONTHS          VALUES 1 THRU 12.
000029             88  FEBRUARY              VALUE 2.
000030             88  D30-DAY-MONTH         VALUES 4 6 9 11.
000031             88  D31-DAY-MONTH         VALUES 1 3 5 7 8 10 12.
000032         10  REN-RETURNED-DAY      PIC 9(2).
000033     05  REN-CAR-TYPE              PIC X.
000034         88  VALID-CAR-TYPES           VALUES 'E' 'C' 'M' 'F' 'L'.
000035     05  REN-DAYS-RENTED           PIC 99.
000036         88  ZERO-DAYS-RENTED          VALUE  0.
000037         88  VALID-DAYS-RENTED         VALUES 1 THRU 35.         
         03 occurs-items.
            15 Store-Num                pic 9(4).
            15 Store-Name               pic x(30).
            
            15 Department-Dtls occurs 2.
               20 Department-Num        pic 9(4).
               20 Department-name       pic x(20).
               20 Product-details occurs 2.
                  25 keycode            pic 9(8).
                  25 A-Sale occurs 1.
                     30 Qty             pic -(5)9.
                     30 Price           pic -(5)9.99.
                     30 trans-type      pic x.
               20 Summary.
                  25 Qty                pic -(5)9.
                  25 Price              pic -(5)9.99.
                  25 Sku-Count          pic -(5)9.
           15 Orders   occurs 2.
              20 keycode                pic 9(8).
              20 Qty                    pic -(5)9.     
           15 array-fld occurs 3 pic xx.
           15 data-field     pic x(10) value '1234567890'.
           15 array-redef redefines data-field.
              20 rc occurs  2 pic 99.
          15 filler redefines data-field.
              20 rc1 occurs 2 pic 99.
          15  redefines data-field.
              20 filler occurs 2.
              25 rc2          pic 99.
          15  redefines data-field.
              20  occurs 1.
              25 rc3          pic 99.

         03 Signed-Comp.
             15 sep0         pic x.
             15 Num0         pic s99 comp.
             15 sep1         pic x.
             15 Num1         pic s9v99  comp.
             15 sep2         pic x.
             15 Num2         pic s9(02)v99 comp.
         03 UnSigned-Comp.
          05 aa.
           07 bb.
            09 cc.
             11 dd.
               15 sep0       pic x.
               15 Num0       pic 99 comp.
               15 sep1       pic x.
               15 Num1       pic 9v99  comp.
               15 sep2       pic x.
               15 Num2       pic 9(02)v99 comp.
        03 Signed-Comp-3.
         05 aa.
             15 sep0         pic x.
             15 Num0         pic s99 comp-3.
             15 sep1         pic x.
             15 Num1         pic s9v99  comp-3.
        03 UnSigned-Comp-3.
         05 aa.
             15 sep0         pic x.
             15 Num0         pic 99 comp-3.
             15 sep1         pic x.
             15 Num1         pic 99v99  comp-3.
         03 Signed-Comp-sync.
             15 sep0         pic x.
             15 Num0         pic s99 comp sync.
             15 sep1         pic x.
             15 Num1         pic s9v99  comp sync.
       03 G-Comp-4.
         05 aa.
             15 sep0         pic x.                                                               
             15 Num0         pic s99 comp-4.
             15 sep1         pic x.
             15 Num1         pic s9v99  comp-4.
             15 Num2         pic 9v99   comp-4.
       03 G-Comp-5.
         05 aa.
             15 sep0         pic x.
             15 Num0         pic s99 comp-5.
             15 sep1         pic x.
             15 Num1         pic s9v99  comp-5.
             15 Num2         pic 9v99   comp-5.
       03 some-more-items.
            10 TOP-LEVEL-ITEM.

                20 PIC-TEST-1 PIC X.
                20 PIC-TEST-2 PIC XX.
                20 PIC-TEST-3 PIC X(3).
                20 PIC-TEST-4 PIC S9.
                20 PIC-TEST-5 PIC S9.9.
                20 PIC-TEST-6 PIC S9V99.
                20 PIC-TEST-7 PIC $ZZZZ9999.
      *
      *    -----------------------------------------------------------
      *    DOM PARSER SHOULD INSERT SPACES IN THESE DASHED LINES
      *    TO AVOID CLASHING WITH THE XML COMMENT TAG / DELIMITER
      *    -----------------------------------------------------------
      *    DIFFERENT VALUE CLAUSES AND CONSTANTS

                20 VALUE-TEST-GROUP.
                    30 VALUE-TEST-1 PIC XXX VALUE SPACES.
                    30 VALUE-TEST-2 PIC 999 VALUE ZEROS.
                    30 VALUE-TEST-3 PIC XXX VALUE NULLS.
                    30 VALUE-TEST-4 PIC 999 VALUE 300.
                    30 VALUE-TEST-5 PIC XXXXX VALUE \"HELLO\".

      *    THE RARELY USED \"DEPENDING ON\" OCCURS SYNTAX

            20 DEPENDING-ON-TEST-GROUP.

                30 DEPENDING-TEST-1 PIC 9.
                30 DEPENDING-TEST-2 PIC X OCCURS 1 TO 3 TIMES
                    DEPENDING ON DEPENDING-TEST-1.
      *    88 LEVELS CAN APPEAR FOR GROUP DECLARATIONS

                20 GROUP-WITH-88-LEVEL.
                    88 GROUP-WITH-88-LEVEL-CONDITION-1 VALUE \"HELLO\".
                    88 GROUP-WITH-88-LEVEL-CONDITION-2 VALUE \"WORLD\".
                        30 GROUP-WITH-88-LEVEL-ITEM-1 PIC XX.
                        30 GROUP-WITH-88-LEVEL-ITEM-2 PIC XXX.

      *    COBOL KEYWORDS CAN BE LOWERCASE -
      *    AND CB2XML IS CASE INSENSITIVE.
      *    88 VALUES ARE CAPTURED CORRECTLY -
      *    AND NOT BULK CONVERTED TO UPPERCASE.

                20 GROUP-WITH-LOWERCASE.
                    30 LOWERCASE-KEYWORDS pic 9 value null.
                    30 lowercase-identifier PIC X.
                    30 MiXeDcAsE-TEST PiC XXX VaLuE SpAcEs.
                    30 LOWERCASE-88-VALUE PIC X(5).

                        88 LOWER-AND-UPPER-CASE-88-VALUES
                            VALUES 'HELLO', 'hello'.

      *    THE \"/\" CHARACTER IN THE FIRST COLUMN SIGNIFIES A COMMENT

                20 INDICATOR-COLUMN-SLASH-TEST-GROUP.
                    30 UNCOMMENTED-1 PIC X.
      /             30 INDICATOR-SLASH-COMMENTED PIC X.
                    30 UNCOMMENTED-2 PIC X.

      *    THERE CAN BE MULTIPLE RECORD LAYOUTS
      *    DEFINED WITHIN A SINGLE COPYBOOK

            10 ANOTHER-TOP-LEVEL-ITEM.

                20 PIC-TEST-99 PIC X.
                20 PIC-TEST-98 PIC 9.
      01  Fields-2.
         03 NumA             pic --,---,---,---,---,--9.99.
         03 NumB             pic 9V99.
         03 NumC             pic 999.
         03 text             pic x(20).               
         03 NumD             pic VPPP999.
         03 NumE             pic 999PPP.
         03 float                          comp-1.
         03 double                         comp-2.  
         03 filler.
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 1 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
         05  SFIELD-SEP            PIC S9(7)V99 SIGN IS                       
                                              LEADING SEPARATE.          
       
         03 88-levels.
000025     05  REN-RETURNED-DATE.
000026         10  REN-RETURNED-YEAR     PIC 9(2).
000027         10  REN-RETURNED-MONTH    PIC 9(2).
000028             88  VALID-MONTHS          VALUES 1 THRU 12.
000029             88  FEBRUARY              VALUE 2.
000030             88  D30-DAY-MONTH         VALUES 4 6 9 11.
000031             88  D31-DAY-MONTH         VALUES 1 3 5 7 8 10 12.
000032         10  REN-RETURNED-DAY      PIC 9(2).
000033     05  REN-CAR-TYPE              PIC X.
000034         88  VALID-CAR-TYPES           VALUES 'E' 'C' 'M' 'F' 'L'.
000035     05  REN-DAYS-RENTED           PIC 99.
000036         88  ZERO-DAYS-RENTED          VALUE  0.
000037         88  VALID-DAYS-RENTED         VALUES 1 THRU 35.
      01  CompFields-3.
         03 NumA             pic --,---,---,---,---,--9.99.
         03 NumB             pic 9V99.
         03 NumC             pic 999.
         03 text             pic x(20).               
         03 NumD             pic VPPP999.
         03 NumE             pic 999PPP.
         03 float                          comp-1.
         03 double                         comp-2.  
         03 filler.
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 1 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
         05  SFIELD-SEP            PIC S9(7)V99 SIGN IS                       
                                              LEADING SEPARATE.          
       
         03 88-levels.
000025     05  REN-RETURNED-DATE.
000026         10  REN-RETURNED-YEAR     PIC 9(2).
000027         10  REN-RETURNED-MONTH    PIC 9(2).
000028             88  VALID-MONTHS          VALUES 1 THRU 12.
000029             88  FEBRUARY              VALUE 2.
000030             88  D30-DAY-MONTH         VALUES 4 6 9 11.
000031             88  D31-DAY-MONTH         VALUES 1 3 5 7 8 10 12.
000032         10  REN-RETURNED-DAY      PIC 9(2).
000033     05  REN-CAR-TYPE              PIC X.
000034         88  VALID-CAR-TYPES           VALUES 'E' 'C' 'M' 'F' 'L'.
000035     05  REN-DAYS-RENTED           PIC 99.
000036         88  ZERO-DAYS-RENTED          VALUE  0.
000037         88  VALID-DAYS-RENTED         VALUES 1 THRU 35.  
                
