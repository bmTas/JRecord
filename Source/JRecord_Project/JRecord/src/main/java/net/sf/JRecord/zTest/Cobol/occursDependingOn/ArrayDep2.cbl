        01 Location-Details.
           03 Record-Type               pic x.
           03 Level-Count               pic s99.
           03 Attr-count                pic s99.
           03 Location-Levels.
              05 occurs 1 to 5 depending on Level-Count.
                 10 sep1                pic x.
                 10 Level               pic 999.
                 10 occurs 1 to 5 depending on Attr-count.
                    15 sep2             pic x.
                    15 Attr             pic 99.