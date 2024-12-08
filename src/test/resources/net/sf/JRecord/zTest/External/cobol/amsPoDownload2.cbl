      *******************************************************************
      *  Ams PO download file
      *******************************************************************
               
        01 amsPoDownload.   
           03 Record-Type            Pic X(2).
           03 PO-Record.
              05 Sequence-Number        Pic 99v999. 
              05 Vendor                 Pic 9(10).  
              05 PO                     Pic 9(12).  
              05 Entry-Date             Pic X(6).
              05 Filler                 Pic X(8).
              05 beg01-code             Pic X(2).
              05 beg02-code             Pic X(2).
              05 Department             Pic X(4).
              05 Expected-Reciept-Date  Pic X(6).
              05 Cancel-by-date         Pic X(6).
              05 Filler                 Pic X(4).
              05 EDI-Type               Pic X(1).
              05 Add-Date               Pic X(6).
              05 Filler                 Pic X(1).
              05 Department-Name        Pic X(10).
              05 Prcoess-Type           Pic X(1).
              05 Order-Type             Pic X(2).
       
           03 Product-Record redefines PO-Record.
              05 Pack-Qty               Pic 9(5)V9999.
              05 Pack-Cost              Pic 9(9)V9999.
              05 APN                    Pic 9(13).                                           .
              05 Filler                 Pic X(1).
              05 Product                Pic 9(8).
              05 Filler                 Pic X(25).
              05 pmg-dtl-tech-key       Pic X(15).
              05 Case-Pack-id           Pic X(15).
              05 Product-Name           Pic X(50).
          
           03 Location-Record redefines PO-Record.
              05 location occurs 10.
                 07 DC-Number           pic 9(4).
                 07 Pack-Quantity       pic 9(8).         
 
 