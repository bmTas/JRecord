      ******************************************************************
      *
      *  Test Data for Testing JRecord types
      *
      *  This Cobol copybook describes on record of Test Data and the
      *  Expected Result.
      *    Type-Number:     Type number to be tested
      *    Field-Length:    Length of the field
      *    Decimal-Length:  Number of places after the decimal point
      *    Test-Value:      Value to assign to variable
      *    Test-Result:     Expected result (Text value) 
      *    Test-Result-Hex: Expected result in hex.
      *
      *  You may wonder why use a Cobol-Copybook for java
      *   - Why not
      *   - It serves as an Example for using JRecord
      *   - I can view / manipulate the file in the RecordEditor
      *   - I can do formated compares in the RecordEditor
      *
      ******************************************************************
      
       01  Test-Record.
           03 Type-Description                pic x(44).
           03 sp1                             pic x.
           03 Type-Number                     pic zz9.
           03 sp2                             pic x.
           03 Field-Length                    pic z9.
           03 sp3                             pic x.
           03 Decimal-Length                  pic 9.
           03 sp4                             pic x.
           03 Test-Value                      pic x(08).
           03 sp5                             pic x.
           03 Test-Result                     pic x(08).
           03 sp6                             pic x.
           03 Test-Result-Hex                 pic x(16).
      