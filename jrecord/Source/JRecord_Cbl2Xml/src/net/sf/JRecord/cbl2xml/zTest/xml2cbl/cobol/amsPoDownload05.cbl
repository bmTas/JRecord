      *******************************************************************
      *  Ams PO download file
      *******************************************************************
               
        05 PO-Record.   
           10 Record-Type            Pic X(2).
           10 Sequence-Number        Pic 99v999. 
           10 Vendor                 Pic 9(10).  
           10 PO                     Pic 9(12).  
           10 Entry-Date             Pic X(6).
           10 Filler                 Pic X(8).
           10 beg01-code             Pic X(2).
           10 beg02-code             Pic X(2).
           10 Department             Pic X(4).
           10 Expected-Reciept-Date  Pic X(6).
           10 Cancel-by-date         Pic X(6).
           10 Filler                 Pic X(4).
           10 EDI-Type               Pic X(1).
           10 Add-Date               Pic X(6).
           10 Filler                 Pic X(1).
           10 Department-Name        Pic X(10).
           10 Prcoess-Type           Pic X(1).
           10 Order-Type             Pic X(2).
      
        05 Product-Record.
           10 Record-Type            pic xx.
           10 Pack-Qty               Pic 9(5)V9999.
           10 Pack-Cost              Pic 9(9)V9999.
           10 APN                    Pic 9(13).                                           .
           10 Filler                 Pic X(1).
           10 Product                Pic 9(8).
           10 Filler                 Pic X(25).
           10 pmg-dtl-tech-key       Pic X(15).
           10 Case-Pack-id           Pic X(15).
           10 Product-Name           Pic X(50).
         
        05 Location-Record.
           10 Record-Type            pic xx.
           10 location occurs 10.
              15 DC-Number           pic 9(4).
              15 Pack-Quantity       pic 9(8).         

