        01 Location-Details.
           03 Location-details.
              05 Location-Number     Pic 9(4).                     
              05 Location-Name       Pic X(25).
           03 months                 Pic s99.    
           03 week-of-month          Pic s9.
           03 days                   Pic s9.
           03 hours                  Pic s9.
           03 fillers occurs 1 to 12
                      depending on months.
              05 occurs 1 to 5 depending on week-of-month.
                 10 occurs 1 to 7 depending on days.
                    15 occurs 1 to 7 depending on hours.
                       20 daily-sales   Pic s9(7).
              05 sales-count         Pic s9(7).
              05 sales-value         Pic s9(9)v99.
           03 total-sales            Pic s9(9)v99.
           03 week-no                Pic s99.
           03 filler occurs 1 to 52
                      depending on week-no.
              05 purchase-count      Pic s9(7).
              05 purchase-value      Pic s9(9)v99.
           03 total-purchase-count   Pic s9(9).
           03 total-purchase-value   Pic s9(9)v99. 