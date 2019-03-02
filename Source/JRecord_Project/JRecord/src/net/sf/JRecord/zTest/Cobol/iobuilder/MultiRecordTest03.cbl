      ************************************************************************
      * Purpose: multi Record Test File
      * Author:  Bruce Martin
      ************************************************************************
      
       01 master-record.
           03 Record-Type                        Pic X.
                 88 Header-Record         value 'H'.
                 88 Detail-Record         value 'D'.
                 
           03 Header-Record.
              05 Creation-Date                   Pic 9(8).
              05 Version                         pic 9(3)V99.
         
           03 Detail-Record redefines Header-Record.
              05 Field-1                         Pic X(10).
              05 Field-2                         Pic X(20).
              05 Field-3                         Pic X(10).
              
           03 Trailer-Record redefines Header-Record.
              05 Record-Count                    Pic 9(9).  

