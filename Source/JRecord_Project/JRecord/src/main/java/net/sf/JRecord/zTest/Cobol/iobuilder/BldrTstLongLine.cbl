      ******************************************************************
      *  Copybook for testing CobolIOBuilders
      *            This test very long lines
      ******************************************************************
          
         05 BldrTst-Group-comp.        
           10 BldrTst-Record-Type                                                                 pic X(1).
              88 Group-Comp  value 'C'.                                                         
           10 BldrTst-comp-l2                                                                     pic s99 comp.
           10 BldrTst-comp-l4                                                                     pic s9(4) comp.
           10 BldrTst-comp-l6                                                                     pic s9(6) comp.
           10 BldrTst-comp-l8                                                                     pic s9(8) comp.
           10 BldrTst-comp-l9                                                                     pic s9(9) comp.       
                                                                                                
         05 BldrTst-Group-comp-5.                                                               
           10 BldrTst-Record-Type                                                                 pic X(1).
              88 Group-Comp  value '5'.                                                         
           10 BldrTst-comp-5-l2                                                                   pic s99 comp-5.
           10 BldrTst-comp-5-l4                                                                   pic s9(4) comp-5.
           10 BldrTst-comp-5-l6                                                                   pic s9(6) comp-5.
           10 BldrTst-comp-5-l8                                                                   pic s9(8) comp-5.
           10 BldrTst-comp-5-l9                                                                   pic s9(9) comp-5.
                                                                                                
         05 BldrTst-Group-Other.                                                        
           10 BldrTst-Record-Type                                                                 pic X(1).
              88 Group-Comp  value 'O'.                                                         
           10 BldrTst-char-field                                                                  pic x(4).
           10 BldrTst-Zoned                                                                       pic s9(4).
                                                                                                
