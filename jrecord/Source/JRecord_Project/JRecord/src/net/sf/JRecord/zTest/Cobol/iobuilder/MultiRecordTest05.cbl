      ************************************************************************
      * Purpose: multi Record Test File
      * Author:  Bruce Martin
      ************************************************************************
      
       01 B-master-record.
           03 Record-Type                        Pic X.
                 88 Header-Record         value 'H'.
                 88 Detail-Record         value 'D'.
                 
           03 B-Header-Record.
              05 B-Creation-Date                   Pic 9(8).
              05 B-Version                         pic 9(3)V99.
         
           03 B-Detail-Record redefines B-Header-Record.
              05 B-Field-1                         Pic X(10).
              05 B-Field-2                         Pic X(20).
              05 B-Field-3                         Pic X(10).
              
           03 B-Trailer-Record redefines B-Header-Record.
              05 B-Record-Count                    Pic 9(9).  

