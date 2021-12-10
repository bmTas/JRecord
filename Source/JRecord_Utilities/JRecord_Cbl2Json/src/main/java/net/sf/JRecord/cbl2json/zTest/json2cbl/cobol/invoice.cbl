       01 CUSTOMER-DATA.
          05 CUSTOMER-NUMBER                 PIC 9(9).
          05 LAST-NAME                       PIC X(20).
          05 FIRST-NAME                      PIC X(20).
          05 INVOICE-COUNT                   PIC 9(7) .
          05 INVOICES OCCURS 0 to 50 TIMES DEPENDING ON INVOICE-COUNT.
             10 INVOICE-NUMBER               PIC X(10).
             10 INVOICE-DATE                 PIC X(10) .
             10 INVOICE-AMOUNT               PIC 9 (4).
             10 INVOICE-DESCRIPTION          PIC X(20).
             10 INV-SUB-CT    PIC 9(4) .
             10 SUB-INVS OCCURS 0 to 10 TIMES DEPENDING ON INV-SUB-CT.
                15 SUB-INVOICES-DATA PIC X(6).
