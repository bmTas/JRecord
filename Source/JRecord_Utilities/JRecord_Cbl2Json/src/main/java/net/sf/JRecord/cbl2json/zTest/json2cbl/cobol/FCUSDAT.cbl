       01  CUSTOMER-DATA.
           05 CUSTOMER-ID                    PIC 9(6).
           05 PERSONAL-DATA.
              10 CUSTOMER-NAME               PIC X(20).
              10 CUSTOMER-ADDRESS            PIC X(20).
              10 CUSTOMER-PHONE              PIC X(8).
           05 TRANSACTIONS.
              10 TRANSACTION-NBR             PIC 9(9) COMP.
              10 TRANSACTION OCCURS 0 TO 5
                 DEPENDING ON TRANSACTION-NBR.
                 15 TRANSACTION-DATE         PIC X(8).
                 15 FILLER REDEFINES TRANSACTION-DATE.
                    20 TRANSACTION-DAY       PIC X(2).
                    20 FILLER                PIC X.
                    20 TRANSACTION-MONTH     PIC X(2).
                    20 FILLER                PIC X.
                    20 TRANSACTION-YEAR      PIC X(2).
                 15 TRANSACTION-AMOUNT       PIC S9(13)V99 COMP-3.
                 15 TRANSACTION-COMMENT      PIC X(9).