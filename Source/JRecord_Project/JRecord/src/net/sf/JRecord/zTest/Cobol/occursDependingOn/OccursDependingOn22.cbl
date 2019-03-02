        01 Location-Details.
           10 Location-details.
              15 Location-Number     Pic 9(4).                     
              15 Location-Name       Pic X(25).
           10 months                 Pic s99.    
           10 week-of-month          Pic s9.
           10 days                   Pic s9.
           10 fillers occurs 1 to 12
                      depending on months.
              15 occurs 1 to 5 depending on week-of-month.
                    20 daily-sales   Pic s9(7).
              15 sales-count         Pic s9(7).
              15 sales-value         Pic s9(9)v99.
           10 total-sales            Pic s9(9)v99.
           10 week-no                Pic s99.
           10 filler occurs 1 to 52
                      depending on week-no.
              15 purchase-count      Pic s9(7).
              15 purchase-value      Pic s9(9)v99.
           10 total-purchase-count   Pic s9(9).
           10 total-purchase-value   Pic s9(9)v99. 