      *******************************************************************
      *  Ams PO download file
      *******************************************************************
               
      01 PO-Record.   
         03 Record-Type            Pic X(2).
         03 Sequence-Number        Pic 99v999. 
         03 Vendor                 Pic 9(10).  
         03 PO                     Pic 9(12).  
         03 Entry-Date             Pic X(6).
         03 Filler                 Pic X(8).
         03 beg01-code             Pic X(2).
         03 beg02-code             Pic X(2).
         03 Department             Pic X(4).
         03 Expected-Reciept-Date  Pic X(6).
         03 Filler                 Pic X(4).
         03 Cancel-by-date         Pic X(6).
         03 EDI-Type               Pic X(1).
         03 Add-Date               Pic X(6).
         03 Filler                 Pic X(1).
         03 Department-Name        Pic X(10).
         03 Prcoess-Type           Pic X(1).
         03 Order-Type             Pic X(2).
      
      01 Product-Record.
         03 Record-Type            pic xx.
         03 Pack-Qty               Pic 9(5)V9999.
         03 Pack-Cost              Pic 9(9)V9999.
         03 APN                    Pic 9(13).                                           .
         03 Filler                 Pic X(1).
         03 Product                Pic 9(8).
         03 Filler                 Pic X(25).
         03 pmg-dtl-tech-key       Pic X(15).
         03 Case-Pack-id           Pic X(15).
         03 Product-Name           Pic X(50).
         
      01 Location-Record.
         03 Record-Type            pic xx.
         03 location occurs 10.
            05 DC-Number           pic 9(4).
            05 Pack-Quantity       pic 9(8).         

