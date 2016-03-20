      *  *-----------------------------------------------------------
      *    Testing arrays in CodeGen
      *    Author Bruce Martin
      *  *-----------------------------------------------------------

       01 ArrayCopybook.
          03 field-1               pic x(5).
          03 field-2               pic x(6).
          03 group-3 occurs 7 times.
             05 field-4            pic x(7).
             05 field-5            pic 9(5).
             05 group-6 occurs 6 times.
                07 field-7         pic 9(6).
                07 group-8 occurs 5 times.
                   10 field-a      pic x(7).
                   10 field-b      pic -(4)9.
                   10 field-c      pic s9(6)V99.
