      ************************************************************************
      * Purpose: multi Record Test File
      * Author:  Bruce Martin
      ************************************************************************
      
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

