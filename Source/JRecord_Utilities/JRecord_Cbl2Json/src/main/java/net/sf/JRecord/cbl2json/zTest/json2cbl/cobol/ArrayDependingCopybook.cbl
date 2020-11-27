      *  *-----------------------------------------------------------
      *    Testing arrays in CodeGen
      *    Author Bruce Martin
      *  *-----------------------------------------------------------

       01 Array-Depending-Copybook.
          03 field-1               pic x(12).
          03 field-2               pic x(14).
          03 group-3-count         pic 99.
          03 group-6-count         pic 99.
          03 group-8-count         pic 99.
          03 group-3 occurs 7 times depending on group-3-count.
             05 field-4            pic x(17).
             05 field-5            pic 9(12).
             05 group-6 occurs 6 times depending on group-6-count.
                07 field-7         pic 9(15).
                07 group-8 occurs 5 times depending on group-8-count.
                   10 field-a      pic x(14).
                   10 field-b      pic -(12)9.
                   10 field-c      pic s9(12)V99.
