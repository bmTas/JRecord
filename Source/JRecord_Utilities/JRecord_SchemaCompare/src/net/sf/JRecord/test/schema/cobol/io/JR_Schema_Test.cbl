      **************************************************************
      * 
      *  Purpose: Copybook for Testing Schema-Conversion
      *   Author: Bruce Martin
      *
      **************************************************************
      
       01  Copybook-Record.
           03  Record-Type                        pic 99.
               88 is-copybook-record value 10.
           03  Copybook-name                      pic x(60).
           03  Do-Compare                         pic x.
           
       01  Dialect-Record.
           03  Record-Type                        pic 99.
               88 Is-Dialect-Record value 20.
           03  Dialect-Code                       pic 9(4).
           03  Dialect-Name                       pic x(25).
           03  Font-Name                          pic x(10).
           03  Has-Binary-Fields                  pic x.
           03  is-binary                          pic x.
           03  File-Structure                     pic 99.
           
       01  Schema-Record-Record.
           03  Record-Type                        pic 99.
               88 Is-Schema-Record value 30.
           03  Record-Name                        pic x(35).
           03  Record-Schema-Type                 pic s9(4).
           03  Record-Style                       pic s9(4).
           03  Field-Count                        pic s9(4).
           
       01  Field-Record.
           03  Record-Type                        pic 99.
               88 Is-Field-Record value 40.
           03  Field-Name                         pic x(35).
           03  Field-Type                         pic 9(3).
           03  Field-Position                     pic 9(5).
           03  Field-Length                       pic 9(5).
           03  Decimal                            pic s9(2).
 
