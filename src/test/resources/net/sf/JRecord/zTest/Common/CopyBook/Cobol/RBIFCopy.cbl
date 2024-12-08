       01  RBI-RECORD.
           05  RBI-TEXT-1          PIC X(50).
           05  RBI-QTY             PIC 9(03).
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 1 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
                   15  RBI-NUMBER-S96CMP3  PIC S9(06) COMP-3.
                   15  RBI-NUMBER-S96CMP   PIC S9(06) COMP.
           05  RBI-TEXT-2          PIC X(50).
           
