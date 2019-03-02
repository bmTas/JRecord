        01 Location-Details.
           03 Location-details.
              05 Location-Number   Pic 9(4).                     
              05 Location-Name     Pic X(25).
           03 months               pic s99.                              
           03 week-no              pic s99.
           03 total-sales          pic s9(9)v99.

           03 sales-count     occurs 1 to 12
                              depending on months  
                                   pic s9(7).
           03 sales-value     occurs 1 to 12
                              depending on months  
                                   pic s9(9)v99.
                                                     