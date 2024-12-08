      *******************************************************************
      *   Test field types record
      *******************************************************************

         01 Cbl-Line-Test-Record.
           03 Fld-Char                     pic X(10).
           03 Fld-Char-right-just          pic X(10).
           03 Fld-Decimal                  pic s9(9) comp-3.
           03 Filler-1                     pic x(8).
           03 Filler-2                     pic x(4).
           03 Filler-3                     pic x(8).
           03 Fld-Num-Right-Just           pic -(7)9.
           03 Fld-Num-Right-Just1          pic 9(7)9.
           03 Fld-Postive-Int              pic s9(9) comp.
           03 Fld-Assummed-Decimal         pic s9(6)v9999.
           03 Fld-Num-2-decimal            pic -(7)9v99.
           03 Fld-Decimal-2-digits         pic s9(7)v99 comp-3.
           03 Fld-Positive-Int-2-digit     pic 9(7)v99  comp.
           03 Filler-4                     pic x(6).
           03 Fld-Mainframe-Int            pic s9(9) comp.
           03 Fld-MainframeInt2decimal     pic s9(7)v99  comp.
           03 Filler-5                     pic x.
           03 Filler-6                     pic xx.
           03 Filler-7                     pic x(4).
           03 Filler-8                     pic x(8).
           03 Fld-Mainframe-Small-Int      pic s9(4)  comp.
           03 Fld-Mainframe-Long           pic s9(15) comp.
           03 Mainframe-Packed-Decimal     pic s9(5)  comp-3.
           03 Mainframe-Packed-DecimalP    pic s9(4)V9 comp-3.
           03 Fld-Zoned                    pic s99.
           03 Fld-Zoned-decimalp           pic s99v99.
           03 Filler-3                     pic x(5).
	   
	   
