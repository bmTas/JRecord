

         10 Store-dtls.
            15 Store-Num                pic 9(4).
            15 Store-Name               pic x(30).
            
            15 Department-Dtls occurs 5.
               20 Department-Num        pic 9(4).
               20 Department-name       pic x(20).
               20 Product-details occurs 10.
                  25 keycode            pic 9(8).
                  25 A-Sale occurs 5.
                     30 Qty             pic -(5)9.
                     30 Price           pic -(5)9.99.
                     30 trans-type      pic x.
               20 Summary.
                  25 Qty                pic -(5)9.
                  25 Price              pic -(5)9.99.
                  25 Sku-Count          pic -(5)9.
            15 Orders   occurs 15.
               20 keycode                pic 9(8).
               20 Qty                    pic -(5)9.
            15 Summary.
               20 Qty                    pic -(7)9.
               20 Price                  pic -(7)9.99.
               20 Sku-Count              pic -(7)9.
