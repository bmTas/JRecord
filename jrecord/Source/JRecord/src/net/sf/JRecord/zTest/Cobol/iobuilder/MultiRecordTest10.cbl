      ************************************************************************
      * Purpose: multi Record Test File
      * Author:  Bruce Martin
      ************************************************************************
      
      01 Detail-Record-A.
          05 Record-Type                            Pic X.
             88 Detail-Record         value 'A'.
          05 Field-1a                                Pic X(11).
          05 Field-2a                                Pic X(12).
          05 Field-3a                                Pic X(11).
          05 Field-4a                                Pic X(44).

       01 Header-Record.
          05 Record-Type                            Pic X.
             88 Header-Record         value 'H'.
         05 Creation-Date                           Pic 9(8).
         05 Version                                 pic 9(3)V99.
         
       01 Detail-Record.
          05 Record-Type                            Pic X.
             88 Detail-Record         value 'D'.
          05 Field-1                                Pic X(10).
          05 Field-2                                Pic X(20).
          05 Field-3                                Pic X(10).
              
       01 Trailer-Record.
          05 Record-Type                            Pic X.
             88 Trailer-Record        value 'T'.
          05 Record-Count                           Pic 9(9).  
          
       01 Detail-Record-B.
          05 Record-Type                            Pic X.
             88 Detail-Record         value 'B'.
          05 Field-1b                                Pic X(22).
          05 Field-2b                                Pic X(33).
          05 Field-3b                                Pic X(11).
          05 Field-4b                                Pic X(11).


