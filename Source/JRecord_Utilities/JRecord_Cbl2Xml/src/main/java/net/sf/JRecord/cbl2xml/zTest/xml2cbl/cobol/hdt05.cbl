*************************************************************************
      * Header / Detail / Trailer Record (based on position
      *******************************************************************
      
          05  Header-Record.
              10 Field-1                    pic x(8).
           
          05  Detail-Record.
              10 Field-2                    pic x(10).
              10 Field-3                    pic 9(6).
           
          05 Trailer-Record.
             10 Record-Count                pic 9(7).
             10 Field-4                     pic x(9).