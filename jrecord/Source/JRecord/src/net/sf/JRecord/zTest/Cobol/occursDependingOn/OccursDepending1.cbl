        01 Location-Details.
           03 Location-details.
              05 Location-Number   Pic 9(4).                     
              05 Location-Name     Pic X(25).
           03 months               pic s99.                              
           03 Monthly-Sales occurs 1 to 12
                      depending on months.
              05 sales-count       pic s9(7).
              05 sales-value       pic s9(9)v99.
           03 total-sales          pic s9(9)v99.
           03 week-no              pic s99.
           03 Weekly-Purchases occurs 1 to 52
                      depending on week-no.
              05 purchase-count    pic s9(7).
              05 purchase-value    pic s9(9)v99.
           03 total-purchase-count pic s9(9).
           03 total-purchase-value pic s9(9)v99.                                                               