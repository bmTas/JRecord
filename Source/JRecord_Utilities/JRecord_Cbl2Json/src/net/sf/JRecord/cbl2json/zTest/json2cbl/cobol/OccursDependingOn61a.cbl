        01 Location-Details.
           03 Test-id                Pic x(10).
           03 fillers occurs  4.
              05 text                Pic xxx.
              05 week-of-month       Pic 9.
              05 occurs 1 to 5 depending on week-of-month.
                 10 Sep              Pic x.
                 10 daily-sales      Pic -9(4).
  
