      *  *-----------------------------------------------------------
      *    Testing arrays in CodeGen
      *    Author Bruce Martin
      *  *-----------------------------------------------------------

       01 ArrayCopybook.
          03 field-1               pic x(12).
          03 field-2               pic x(14).
          03 group-3 occurs 7 times.
             05 field-4            pic x(17).
             05 field-5            pic 9(12).
             05 group-6 occurs 6 times.
                07 field-7         pic 9(15).
                07 group-8 occurs 5 times.
                   10 field-a      pic x(14).
                   10 field-b      pic -(12)9.
                   10 field-c      pic s9(12)V99.
