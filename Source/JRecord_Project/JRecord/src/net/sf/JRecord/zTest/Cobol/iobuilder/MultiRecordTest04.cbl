      ************************************************************************
      * Purpose: multi Record Test File
      * Author:  Bruce Martin
      ************************************************************************
      
       
          03 A-Header-Record.
              05 Record-Type                        Pic X.
                 88 Header-Record         value '0'.
              05 A-Creation-Date                    Pic 9(8).
              05 A-Version                          pic 9(3)V99.
         
           03 A-Detail-Record.
              05 Record-Type                        Pic X.
                 88 Detail-Record         value '1'.
              05 A-Field-1                          Pic X(10).
              05 A-Field-2                          Pic X(20).
              05 A-Field-3                          Pic X(10).
              
           03 A-Trailer-Record.
              05 Record-Type                        Pic X.
                 88 Trailer-Record        value '2'.
              05 A-Record-Count                     Pic 9(9).  


