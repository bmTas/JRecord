        01 Location-Details.
           03 Test-id                Pic x(9).
           03 Month-Count            Pic 9.
           03 a4 occurs  0 to 9 depending on Month-Count.
              05 text                Pic xxx.
              05 week-of-month       Pic 9.
              05 week occurs 1 to 5 depending on week-of-month.
                 10 Sep              Pic x.
                 10 daily-sales      Pic -9(4).
  
