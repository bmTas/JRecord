*************************************************************************
      * Header / Detail / Trailer Record (based on position
      *******************************************************************
      
       01  Header-Record.
           03 Field-1                    pic x(8).
           
       01  Detail-Record.
           03 Field-2                    pic x(10).
           03 Field-3                    pic 9(6).
           
       01 Trailer-Record.
          03 Record-Count                pic 9(7).
          03 Field-4                     pic x(9).