        01 Location-Details.
           03 Record-Type               pic x.
           03 Level-Count               pic s99.
           03 Attr-count                pic s99.
           03 Location-Levels.
              05 occurs 1 to 5 depending on Level-Count.
                 10 Level               pic 999.
           03 Record-Details            pic x(120).
           03 Store-details redefines Record-Details.
              05 months                 Pic s99.    
              05 week-of-month          Pic s9.
              05 days                   Pic s9.
              05 fillers occurs 1 to 12
                         depending on months.
                 10 sales-count         Pic s9(7).
                 10 sales-value         Pic s9(9)v99.
              05 total-sales            Pic s9(9)v99.
              05 week-no                Pic s99.
