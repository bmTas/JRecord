       01  RBI-RECORD-T1.
           05  RBI-TEXT-1      PIC X(50).
      *        Next variable is discriminator. Should contain 1 in this case     
           05  RBI-QTY         PIC 9(03).
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 1 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
                   15  RBI-NUMBER-S96CMP3  PIC S9(06) COMP-3.
                   15  RBI-NUMBER-S96CMP   PIC S9(06) COMP.
           05  RBI-TEXTE-2     PIC X(50).
           
       01  RBI-RECORD-T2.
           05  RBI-TEXT-1      PIC X(50).
      *        Next variable is discriminator. Should contain 2 in this case     
           05  RBI-QTY         PIC 9(03).
           05  RBI-REPETITIVE-AREA.
               10  RBI-REPEAT OCCURS 2 TIMES. 
                   15  RBI-NUMBER-S96SLS   PIC S9(06) 
                                           SIGN LEADING SEPARATE.
                   15  RBI-NUMBER-S96DISP  PIC S9(06).
                   15  RBI-NUMBER-S96CMP3  PIC S9(06) COMP-3.
                   15  RBI-NUMBER-S96CMP   PIC S9(06) COMP.
           05  RBI-TEXTE-2     PIC X(50).
