      *********************************************************
      *    FILE LAYOUT OF THE ASN / RECEIPT EXTRACT           *
      *    NOTE: USES SAME FIELD NAMES AS STD. DB2 COPYBOOKS  *
      *          BUT ALL PACKED FIELDS ARE NOW IN UNSIGNED    *
      *          DISPLAY FORMAT, FOR EASIER TRANSFER/IMPORT   *
      *                                                       *
      *    15/03/00.   350 BYTES.                             *
      *********************************************************
      *
      *CHANGE CONTROL.
      *-----------------------------------------------------------------
      *VERS|  DATE  |DESCRIPTION                            | AUTHOR
      *----+--------+---------------------------------------+-----------
      *V01 |15/03/00|FIRST ISSUE                            | J.BINDER
      *----+--------+---------------------------------------+-----------
      * 1.1|20/03/00|ADDED: STDR-AS-DLVY-TYPE TO AS RECORD  | J.BINDER
      *----+--------+---------------------------------------+-----------
      *V02 |27.04.00|ALL RECORDS NOW CONTAIN ALL KEY COLS.  | J.BINDER
      *----+--------+---------------------------------------+-----------
      *V03 |27.06.00|ADDED SAMPLE CHECK COLS. TO EQV030 AND | J.BINDER
      *    |OR #13  |MADE UNIT-COST 3 DECIMAL PLACES        |
      *----+--------+---------------------------------------+-----------
      *V04 |02.08.00|ADDED ACT-RECV-QTY TO THE "RS" RECORD  | J.BINDER
      *    |OR #130 |HOLDS REC_ALC_QTY ADJUSTED FOR SAMPLE  |
      *    |        |CHECK RESULTS                          |
      *----+--------+---------------------------------------+-----------
      *V05 |22.01.02|ADD PICK-PACK INDICATOR TO RECEIPT     | MRJ
      *    |        |HEADER                                 |
      *----+--------+---------------------------------------+-----------
      *V06 |18.02.03|ADD SHIPPING COMPLETE EXTRACT STATUS   | MRJ
      *----+--------+---------------------------------------+-----------
      *
      *01  STDR-RECORD.
      *     05  STDR-RECORD-TYPE            PIC X(2).

      *         88  STDR-REC-TYPE-FH                  VALUE 'FH'.

      *         88  STDR-REC-TYPE-RH                  VALUE 'RH'.
      *         88  STDR-REC-TYPE-RD                  VALUE 'RD'.
      *         88  STDR-REC-TYPE-RS                  VALUE 'RS'.

      *         88  STDR-REC-TYPE-AS                  VALUE 'AS'.
      *         88  STDR-REC-TYPE-SO                  VALUE 'SO'.
      *         88  STDR-REC-TYPE-SC                  VALUE 'SC'.
      *         88  STDR-REC-TYPE-AP                  VALUE 'AP'.
      *         88  STDR-REC-TYPE-AR                  VALUE 'AR'.

      *         88  STDR-REC-TYPE-FT                  VALUE 'FT'.

      * ========================================================
           03  STDR-DETAIL-RECORD.
             04 XXX.
      * ========================================================
                05  STDR-RECORD-TYPE            PIC X(2).


      * FH     =================================================
      * FH     FILE HEADER
      * FH     =================================================

               05  STDR-FH-DESCRIPTION     PIC X(8).
               05  STDR-FH-BRAND-ID        PIC X(3).
               05  STDR-FH-CREATE-DATE.
                   07  STDR-FH-CRD-DD      PIC 99.
                   07  FILLER              PIC X.
                   07  STDR-FH-CRD-MM      PIC 99.
                   07  FILLER              PIC X.
                   07  STDR-FH-CRD-CC      PIC 99.
                   07  STDR-FH-CRD-YY      PIC 99.
               05  STDR-FH-CREATE-TIME.
                   07  STDR-FH-CRT-HH      PIC 99.
                   07  FILLER              PIC X.
                   07  STDR-FH-CRT-MM      PIC 99.
                   07  FILLER              PIC X.
                   07  STDR-FH-CRT-SS      PIC 99.

               05  STDR-FH-FILLER          PIC X(319).

      * RH     =================================================
      * RH     RECEIPT HEADER
      * RH     =================================================
           03  STDR-RH REDEFINES STDR-DETAIL-RECORD.

               05  STDR-RECORD-TYPE            PIC X(2).
               05  STDR-RH-EQV030.
                   10 BRAND-ID-RH          PIC X(3).
                   10 ORDER-NO-RH          PIC X(12).
                   10 RECEIPT-LOCN-RH      PIC 9(4).
                   10 RECEIPT-NO-RH        PIC 9(9).
                   10 SUP-ID-RH            PIC 9(8).
                   10 RECEIPT-DATE-RH      PIC X(10).
                   10 RECEIPT-TIME-RH      PIC X(8).
                   10 BRAND-XREF-RH        PIC 9(5).
                   10 BRAND-DC-NO-RH       PIC 9(4).
                   10 RCPT-FINAL-STAT-RH   PIC X(1).
                   10 PROCESSED-IND-RH     PIC X(1).
                   10 ASN-RH               PIC X(30).
                   10 ASN-SEQ-NO-RH        PIC 9(9).
                   10 SMPL-CHCK-CTN-RH     PIC 9(7).
                   10 SMPL-CHCK-U-RH       PIC 9(9)V9(2).
                   10 INVOICE-NO-RH        PIC X(20).
                   10 TOT-RCPT-AMT-RH      PIC 9(7)V9(2).
                   10 TOT-RECV-QTY-RH      PIC 9(9).
                   10 TOT-OUTERS-RH        PIC 9(9)V9(2).
                   10 CARRIER-CONNOTE-RH   PIC X(20).
                   10 ERS-STATUS-RH        PIC X(1).
                   10 CHK-RESULT-ADJD-RH   PIC X(1).
                   10 BRAND-STK-UPD-RH     PIC X(1).
                   10 ORGINL-SCM-CNT-RH    PIC 9(7).
V5                 10 PICK-PACK-IND-RH     PIC X.
                   10 C-USR-UPDT-RH        PIC X(8).
                   10 TS-UPDT-RH           PIC X(26).

               05  STDR-RH-NUM-RD          PIC 9(09).
               05  STDR-RH-NUM-AS          PIC 9(09).

V5             05  STDR-RH-FILLER          PIC X(94).

      * RD     =================================================
      * RD     RECEIPT PRODUCT
      * RD     =================================================
           03  STDR-RD REDEFINES STDR-DETAIL-RECORD.

              05  STDR-RECORD-TYPE            PIC X(2).
              05  STDR-RD-EQV031.
                   10 BRAND-ID-RD          PIC X(3).
                   10 ORDER-NO-RD          PIC X(12).
                   10 RECEIPT-LOCN-RD      PIC 9(4).
                   10 RECEIPT-NO-RD        PIC 9(9).
                   10 PROD-NO-RD           PIC 9(14).
                   10 KEYCODE-RD           PIC 9(8).
                   10 SEQ-NO-RD            PIC 9(5).
                   10 PROD-QUALIFIER-RD    PIC X(2).
                   10 RCVD-QTY-RD          PIC 9(9).
                   10 ITMS-PER-CTN-QTYRD   PIC 9(6).
                   10 UNIT-COST-RD         PIC 9(11)V9(3).
                   10 C-USR-UPDT-RD        PIC X(8).
                   10 TS-UPDT-RD           PIC X(26).

               05  STDR-RD-NUM-RS          PIC 9(09).

               05  STDR-RD-FILLER          PIC X(219).

      * RS     =================================================
      * RS     RECEIPT STORE ALLOCATION
      * RS     =================================================
           03  STDR-RS REDEFINES STDR-DETAIL-RECORD.

               05  STDR-RECORD-TYPE            PIC X(2).
               05  STDR-RS-EQV032.
                   10 BRAND-ID-RS          PIC X(3).
                   10 ORDER-NO-RS          PIC X(12).
                   10 RECEIPT-LOCN-RS      PIC 9(4).
                   10 RECEIPT-NO-RS        PIC 9(9).
                   10 PROD-NO-RS           PIC 9(14).
                   10 KEYCODE-RS           PIC 9(8).
                   10 SEQ-NO-RS            PIC 9(5).
                   10 STR-NO-RS            PIC 9(4).
                   10 REC-ALC-QTY-RS       PIC 9(9).
                   10 OUTST-ORD-QTY-RS     PIC 9(9).
                   10 C-USR-UPDT-RS        PIC X(8).
                   10 TS-UPDT-RS           PIC X(26).
      *V04*
               05  ACT-RECV-QTY-RS         PIC 9(9).

      *V04*    05  STDR-RS-FILLER          PIC X(237).
               05  STDR-RS-FILLER          PIC X(228).

      * AS     =================================================
      * AS     ASN HEADER
      * AS     =================================================
           03  STDR-AS REDEFINES STDR-DETAIL-RECORD.

              05  STDR-RECORD-TYPE         PIC X(2).
              05  STDR-AS-EQV004.
                   10 ASN-AS               PIC X(30).
                   10 ASN-SEQ-NO-AS        PIC 9(9).
                   10 SUP-EDI-ADDR-AS      PIC X(15).
                   10 BRAND-ID-AS          PIC X(3).
                   10 PROD-TEST-IND-AS     PIC X(1).
                   10 TRANSACTION-DT-AS    PIC X(10).
                   10 TRANSACTION-TM-AS    PIC X(8).
                   10 TRANS-SET-PURP-AS    PIC X(2).
                   10 ASN-STRUCTURE-AS     PIC X(4).
                   10 SHIP-GROSS-VOL-AS    PIC 9(6)V9(3).
                   10 VOL-MEASURE-CD-AS    PIC X(2).
                   10 SHIP-PACK-CODE-AS    PIC X(5).
                   10 SHIP-LDG-QTY-AS      PIC 9(7).
                   10 SHIP-LDG-WGT-AS      PIC 9(8)V9(3).
                   10 SHIP-WGT-CODE-AS     PIC X(2).
                   10 SHIP-STATUS-AS       PIC X(2).
                   10 CARRIER-CONNOTE-AS   PIC X(20).
                   10 SHIP-SCHED-DT-AS     PIC X(10).
                   10 DLVY-SCHED-DT-AS     PIC X(10).
                   10 M-DC-DLVY-AS         PIC 9(4).
                   10 SUP-ID-AS            PIC 9(8).
                   10 SUP-NAME-AS          PIC X(35).
                   10 A-ALLOC-REG-NET-AS   PIC 9(9)V9(4).
                   10 VALID-IND-AS         PIC X(1).
                   10 REASON-CODE-AS       PIC X(1).
                   10 ASN-STATE-AS         PIC 9(2).
                   10 FULL-DLVY-IND-AS     PIC X(1).
                   10 SHIP-SSCC-AS         PIC X(20).
                   10 EDI-ERR-SEVER-I-AS   PIC X(1).
                   10 ASN-RCPT-DT-AS       PIC X(10).
                   10 MERCH-RCPT-DT-AS     PIC X(10).
                   10 SUP-RATING-AS        PIC X(2).
                   10 INTEG-FLG-AS         PIC X(1).
                   10 CANCEL-REASON-C-AS   PIC X(1).
      ***        10 P-CHECKED-CTNS-AS    PIC S9(3)V USAGE COMP-3.
      ***        10 P-CHECKED-UNITS-AS   PIC S9(3)V USAGE COMP-3.
V6                 10 ASN-ARRV-TM-AS       PIC X(8).
V6                 10 EXTRACT-STATUS-AS    PIC X(1).
                   10 C-USR-UPDT-AS        PIC X(8).
                   10 TS-UPDT-AS           PIC X(26).

               05  STDR-AS-DLVY-TYPE       PIC X(03).
               05  STDR-AS-NUM-SO          PIC 9(09).
               05  STDR-AS-NUM-AR          PIC 9(09).

      ***      05  STDR-AS-FILLER          PIC X(023).
V6             05  STDR-AS-FILLER          PIC X(010).

      * SO     =================================================
      * SO     ASN ORDER
      * SO     =================================================
           03  STDR-SO REDEFINES STDR-DETAIL-RECORD.

               05  STDR-RECORD-TYPE            PIC X(2).
               05  STDR-SO-EQV005.
                   10 ASN-SO               PIC X(30).
                   10 ASN-SEQ-NO-SO        PIC 9(9).
                   10 ORDER-NO-SO          PIC X(12).
                   10 STORE-SO             PIC 9(4).
                   10 ORDER-DATE-SO        PIC X(10).
                   10 ORDER-PACK-CODE-SO   PIC X(5).
                   10 ORDER-LDG-QTY-SO     PIC 9(7).
                   10 ORDER-LDG-WGT-SO     PIC 9(8)V9(3).
                   10 ORDER-WGT-CODE-SO    PIC X(2).
                   10 SHIP-ORD-STATUS-SO   PIC X(2).
                   10 SUP-INV-NO-SO        PIC X(20).
                   10 ERROR-CODE-SO        PIC X(2).
                   10 ORDER-NO-NUM-SO      PIC 9(12).
                   10 ORDER-NO-850-SO      PIC X(12).
                   10 C-USR-UPDT-SO        PIC X(8).
                   10 TS-UPDT-SO           PIC X(26).

               05  STDR-SO-NUM-SC          PIC 9(09).

               05  STDR-SO-FILLER          PIC X(167).

      * SC     =================================================
      * SC     ASN CONTAINER
      * SC     =================================================
           03  STDR-SC REDEFINES STDR-DETAIL-RECORD.

              05  STDR-RECORD-TYPE            PIC X(2).
              05  STDR-SC-EQV006.
                   10 ASN-SC               PIC X(30).
                   10 ASN-SEQ-NO-SC        PIC 9(9).
                   10 ORDER-NO-SC          PIC X(12).
                   10 STORE-SC             PIC 9(4).
                   10 SCM-SC               PIC X(20).
                   10 SCM-SEQ-NO-SC        PIC 9(9).
                   10 CONTAINER-TYPE-SC    PIC X(1).
                   10 PLT-PACK-CODE-SC     PIC X(5).
                   10 PLT-LADING-QTY-SC    PIC 9(7).
                   10 PLT-LADING-WGT-SC    PIC 9(8)V9(3).
                   10 PLT-WGT-MSRE-CD-SC   PIC X(2).
                   10 PARENT-SCM-SC        PIC X(20).
                   10 PARENT-SCM-SEQ-SC    PIC 9(9).
                   10 ERROR-CODE-SC        PIC X(2).
                   10 SCM-RCEIV-QUALFRSC   PIC X(1).
                   10 SCM-SAMPLE-IND-SC    PIC X(1).
                   10 C-USR-UPDT-SC        PIC X(8).
                   10 TS-UPDT-SC           PIC X(26).

               05  STDR-SC-NUM-AP          PIC 9(09).

               05  STDR-SC-FILLER          PIC X(162).

      * AP     =================================================
      * AP     ASN PRODUCT
      * AP     =================================================
           03  STDR-AP REDEFINES STDR-DETAIL-RECORD.

               05  STDR-RECORD-TYPE            PIC X(2).
               05  STDR-AP-EQV007.
                   10 ASN-AP               PIC X(30).
                   10 ASN-SEQ-NO-AP        PIC 9(9).
                   10 ORDER-NO-AP          PIC X(12).
                   10 STORE-AP             PIC 9(4).
                   10 SCM-AP               PIC X(20).
                   10 SCM-SEQ-NO-AP        PIC 9(9).
                   10 PROD-NO-AP           PIC 9(14).
                   10 PROD-SEQ-NO-AP       PIC 9(5).
                   10 PROD-QUALIFIER-AP    PIC X(2).
                   10 QTY-SHIPPED-AP       PIC 9(10)V9(2).
                   10 PROD-MEASURE-CD-AP   PIC X(2).
                   10 USE-BY-DATE-AP       PIC X(10).
                   10 KEYCODE-AP           PIC 9(8).
                   10 ERROR-CODE-AP        PIC X(2).
                   10 DANGR-GOODS-NO-AP    PIC 9(4).
                   10 DANGR-GOODS-CLS-AP   PIC 9(3)V9(1).
                   10 C-USR-UPDT-AP        PIC X(8).
                   10 TS-UPDT-AP           PIC X(26).

               05  STDR-AP-FILLER          PIC X(167).

      * AR     =================================================
      * AR     SCM CONTENTS CHECKING
      * AR     =================================================
           03  STDR-AR REDEFINES STDR-DETAIL-RECORD.

              05  STDR-RECORD-TYPE            PIC X(2).
              05  STDR-AR-EQV044.
                   10 BRAND-ID-AR          PIC X(3).
                   10 ORDER-NO-AR          PIC X(12).
                   10 LOGISTICS-LOC-NOAR   PIC 9(4).
                   10 RECEIPT-NO-AR        PIC 9(9).
                   10 SCM-AR               PIC X(20).
                   10 PROD-NO-AR           PIC 9(14).
                   10 PROD-QUALIFIER-AR    PIC X(2).
                   10 QUALITY-CODE-AR      PIC X(2).
                   10 CHECKED-QTY-AR       PIC 9(10)V9(2).
                   10 SUP-ID-AR            PIC 9(8).
                   10 ASN-AR               PIC X(30).
                   10 BRAND-DC-AR          PIC 9(4).
                   10 VALID-PROD-IND-AR    PIC X(1).
                   10 PROCESSED-IND-AR     PIC X(1).
                   10 CHECKED-BY-AR        PIC X(10).
                   10 CHECKED-DATE-AR      PIC X(10).
                   10 CHECKED-TIME-AR      PIC X(8).
                   10 C-USR-UPDT-AR        PIC X(8).
                   10 TS-UPDT-AR           PIC X(26).

               05  STDR-AR-FILLER          PIC X(164).

      * FT     =================================================
      * FT     FILE TRAILER
      * FT     =================================================
           03  STDR-FT REDEFINES STDR-DETAIL-RECORD.

               05  STDR-RECORD-TYPE            PIC X(2).
               05  STDR-FT-NUM-RECDS       PIC 9(09).

               05  STDR-FT-NUM-FH          PIC 9(09).

               05  STDR-FT-NUM-RH          PIC 9(09).
               05  STDR-FT-NUM-RD          PIC 9(09).
               05  STDR-FT-NUM-RS          PIC 9(09).

               05  STDR-FT-NUM-AS          PIC 9(09).
               05  STDR-FT-NUM-SO          PIC 9(09).
               05  STDR-FT-NUM-SC          PIC 9(09).
               05  STDR-FT-NUM-AP          PIC 9(09).
               05  STDR-FT-NUM-AR          PIC 9(09).

               05  STDR-FT-NUM-FT          PIC 9(09).

               05  STDR-FT-FILLER          PIC X(249).

      * ========================================================