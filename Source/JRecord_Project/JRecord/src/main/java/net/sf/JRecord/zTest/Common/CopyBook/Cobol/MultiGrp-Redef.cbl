      01 Parent.          
        05 Record-Type                  pic x.
        05 Group-1.    
           10 Field-11                  pic s99.
           10 Field-12                  pic s99.
           10 Field-13                  pic x(20).
           
        05 Group-2 redefines Group-1.    
           10 Field-21                  pic s999.
           10 Filler.
              15 Field-22               pic x(20).
           
        05 Group-3 redefines Group-1.    
           10 Field-31                  pic s9999.
           10 Group-31.
              15 Field-32               pic x(25).
           
