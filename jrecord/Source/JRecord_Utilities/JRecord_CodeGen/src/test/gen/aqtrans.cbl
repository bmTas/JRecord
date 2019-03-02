      *  CREATED - 23.03.91  ** NOTE : DR/CR/IPD/IRC SIZE MUST BE = 700
      *  AMENDED - 16.04.92  ** FIXED LENGTH COBCOPY **
      *            17.11.92 (JMK) - ADD IPD & DR REC- 2ND CONV RTE & AMT
      *            08.04.94 (JMK) - ADD IRC FIELDS
      *            12.09.94 (THK) - ADD CONV RTE & LOCAL AMT TO "IRC"
      *            3/11/95  (THK) - HANDLE FOR FCCA
      *                             NEW BASIC TRANS TYP "SVC" (SAME AS "DR")
      *                             NEW PROD TRANS TYP "WTAX-DR" (SAME AS "WDL")
      *            28/9/98  (JMK) - ADD NEW FIELD : INTF-SYS
      *            3/8/98   (JMK) - AMD FOR NEW FIELD : DEP-SCHEME
      *            8/5/98   (JMK) - EXPAND EXCH RTE FIELDS : 9(4)V9(7)->
      *                             9(7)V9(10)
      *
      *  FILE NAME          : AQSP.TRANS        SYSTEM     : FC SYSTEM
      *                       ANSP.TRANS
      *  RECORD DESCRIPTION : FC FINANCIAL TRANS (AUTHORISED)
      *  ORGANIZATION       : VSAM KSDS - VARIABLE
      *  RECORD SIZE        : 700 - 1527 BYTES  BLOCK SIZE : 4096 BYTES
      *  RECORD KEY         : PRODUCT TYPE     (   2 BYTES  )
      *                       ACCOUNT NO       (  20 BYTES  )
      *                       DEPOSIT NO       (  11 BYTES  )
      *                       TRANS REF        (  13 BYTES  )
      *                       TRANS VER NO     (  02 BYTES  )
      *                       TRANS SEQ        (  02 BYTES  )
      *  ALT RECORD KEY     : TRANS REF        (  13 BYTES  )
      *                       TRANS VER NO     (  02 BYTES  )
      *                       TRANS SEQ        (  02 BYTES  )
      *
      *  IN WORKING STORAGE:
      *     COPY AQTRANS.
      *
      *-----------------------------------------------------------------
       01  TRANS-HEADER.
           05  TRANS-HEADER-KEY                  PIC X(50).
           05  TRANS-DTE-TODAY                   PIC 9(06).
           05  FILLER                            PIC X(1471).


       01  TRANS-RECORD                  REDEFINES TRANS-HEADER.
           05  TRANS-KEY.
               10  TRANS-PROD-TYP                PIC X(02).
               10  TRANS-ACC-NO                  PIC X(20).
               10  TRANS-DEP-NO                  PIC X(11).
               10  TRANS-REF                     PIC X(13).
               10  TRANS-VER-NO                  PIC X(02).
               10  TRANS-SEQ                     PIC 9(02).
           05  TRANS-TRN-VER-NO                  PIC 9(02).
           05  TRANS-PROD-TRNTYP                 PIC X(04).
      **       (AF): DEP, WDL, PW, ROL
      **       (AN): DEP, WDL, REV, RTCQ, MKCQ, WTAX, IWCQ, SPCQ
           05  TRANS-ORIG-PROD-TRNTYP            PIC X(04).
           05  TRANS-BASIC-TRNTYP                PIC X(04).
      **       (AF):  DR, CR, SD, UDP, IPD, IRC
      **       (AN):  DR, CR, SVC,IPD, IRC
           05  TRANS-ORIG-BASIC-TRNTYP           PIC X(04).
      **
           05  TRANS-AUDIT-DETLS.
               10  TRANS-ENTER-ID                PIC X(08).
               10  TRANS-ENTER-BRCH              PIC X(04).
               10  TRANS-ENTER-SECT              PIC X(04).
               10  TRANS-ENTER-DTE               PIC 9(08).
               10  TRANS-ENTER-TIME              PIC 9(06).
               10  TRANS-ENTER-ID-1              PIC X(08).
               10  TRANS-FST-OFF-ID              PIC X(08).
               10  TRANS-SEC-OFF-ID              PIC X(08).
           05  TRANS-DTE-SYS                     PIC 9(08).
           05  TRANS-TIME-SYS                    PIC 9(06).
           05  TRANS-PROC-BRCH                   PIC X(04).
           05  TRANS-PROC-SECT                   PIC X(04).
           05  TRANS-SYSGEN-IND                  PIC X(10).
           05  TRANS-SYSGEN-ID                   PIC X(04).
           05  TRANS-ACTION-INDS.
               10  TRANS-SETL-ENTER-IND          PIC X(01).
           05  TRANS-STATUS-INDS.
               10  TRANS-INITIATED-IND           PIC X(01).
               10  TRANS-SETTLED-IND             PIC X(01).
           05  TRANS-SETL-TRN-REF                PIC X(13).
           05  TRANS-SETL-TRN-VER-NO             PIC 9(02).
           05  TRANS-DEP-DETLS.
               10  TRN-DEP-CCY                   PIC X(03).
               10  TRN-DEP-CCY-DEC               PIC 9(01).
               10  TRN-DEP-ORIG-BAL              PIC S9(15)     COMP-3.
               10  TRN-DEP-CR-INT-RTE            PIC S9(3)V9(6) COMP-3.
               10  TRN-DEP-CR-ORIG-INT-WDL       PIC S9(15)     COMP-3.
               10  TRN-DEP-BAL                   PIC S9(15)     COMP-3.
      **NOTE (AF) : DEP-BAL AT PT OF DATA-ENTRY (IE. BEFORE ACTION STATUS)
      **     (AN) : LEDGER BAL BEFORE DR/CR - FOR TRANS DR/SVC/IPD/IRC
      **            LEDGER BAL AFTER DR/CR  - FOR TRANS CR

               10  TRN-DEP-CR-INT-WDL-BAL        PIC S9(15)     COMP-3.
               10  TRN-DEP-WTAX-AMT              PIC S9(15)     COMP-3.
               10  TRN-DEP-DTE-VAL-END           PIC 9(08).
               10  TRN-DEP-CONTRACT-PRD          PIC X(03).
               10  TRN-DEP-NO-OF-DAYS            PIC S9(05)     COMP-3.
           05  TRANS-PREV-TRN-REF                PIC X(13).
           05  TRANS-PREV-TRN-VER-NO             PIC X(02).
28/9       05  TRANS-INTF-SYS                    PIC X(04).
28/9       05  FILLER                            PIC X(09).
           05  TRANS-DETLS                       PIC X(1260).
           05  TRANS-CREDIT-DETLS     REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(14).
               10  TRN-CR-DTE-TRN                PIC 9(08).
               10  TRN-CR-DTE-VAL                PIC 9(08).
               10  TRN-CR-AMT                    PIC S9(15)      COMP-3.
               10  TRN-CR-REMARK-1               PIC X(60).
               10  TRN-CR-REF-1 REDEFINES TRN-CR-REMARK-1
                                                 PIC X(60).
               10  TRN-CR-REMARK-2               PIC X(60).
               10  TRN-CR-REF-2 REDEFINES TRN-CR-REMARK-2
                                                 PIC X(60).
               10  TRN-CR-SOF-REMARK             PIC X(60).
               10  TRN-CR-REF-3 REDEFINES TRN-CR-SOF-REMARK
                                                 PIC X(60).
               10  TRN-CR-DTE-SETL               PIC 9(08).
               10  TRN-CR-ROL-OLD-DEP-IND        PIC X(01).
               10  TRN-CR-OLD-DEP-NO             PIC X(11).
      **     { APPLICABLE FOR DEMAND DEPOSITS PRODUCTS }
               10  TRN-CR-TXN-CCY                PIC X(03).
               10  TRN-CR-TXN-CCY-DEC            PIC 9(01).
               10  TRN-CR-TXN-AMT                PIC S9(15)      COMP-3.
8/5   *D       10  TRN-CR-TXN-CONV-RTE           PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-CR-TXN-CONV-RTE           PIC S9(7)V9(10) COMP-3.
               10  TRN-CR-SETL-TYP               PIC X(04).
               10  TRN-CR-CHQ-IND                PIC X(1).
               10  FILLER                        PIC X(996).
8/5   *D       10  FILLER                        PIC X(999).
           05  TRANS-DEBIT-DETLS      REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(14).
               10  TRN-DR-DTE-TRN                PIC 9(08).
               10  TRN-DR-DTE-VAL                PIC 9(08).
               10  TRN-DR-BAL                    PIC S9(15)      COMP-3.
      **          (AN) : LEDGER AMT BEFORE SUBTRACT
               10  TRN-DR-AMT                    PIC S9(15)      COMP-3.
8/5   *D       10  TRN-DR-CONV-RTE               PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-DR-CONV-RTE               PIC S9(7)V9(10) COMP-3.
      **           FC CCY -> LOCAL CCY
               10  TRN-DR-LOCAL-EQUIV-AMT        PIC S9(15)      COMP-3.
               10  TRN-DR-REMARK-1               PIC X(60).
               10  TRN-DR-REF-1 REDEFINES TRN-DR-REMARK-1
                                                 PIC X(60).
               10  TRN-DR-REMARK-2               PIC X(60).
               10  TRN-DR-REF-2 REDEFINES TRN-DR-REMARK-2
                                                 PIC X(60).
               10  TRN-DR-AOF-REMARK             PIC X(60).
               10  TRN-DR-REF-3 REDEFINES TRN-DR-AOF-REMARK
                                                 PIC X(60).
               10  TRN-DR-DTE-SETL               PIC 9(08).
               10  TRN-DR-ROL-NEW-DEP-IND        PIC X(01).
               10  TRN-DR-NEW-DEP-NO             PIC X(11).
               10  TRN-DR-PARTIAL-AMT            PIC S9(15)      COMP-3.
               10  TRN-DR-PARTIAL-AMT2           PIC S9(15)      COMP-3.
8/5   *D       10  TRN-DR-CONV-RTE2              PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-DR-CONV-RTE2              PIC S9(7)V9(10) COMP-3.
               10  TRN-DR-LOCAL-EQUIV-AMT2       PIC S9(15)      COMP-3.
               10  TRN-DR-TXN-CCY                PIC X(03).
               10  TRN-DR-TXN-CCY-DEC            PIC 9(01).
               10  TRN-DR-TXN-AMT                PIC S9(15)      COMP-3.
8/5   *D       10  TRN-DR-TXN-CONV-RTE           PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-DR-TXN-CONV-RTE           PIC S9(7)V9(10) COMP-3.
      **           TXN CCY -> A/C CCY
               10  TRN-DR-CHQ-NO                 PIC 9(08).
               10  TRN-DR-CHQ-NO-FROM
                   REDEFINES TRN-DR-CHQ-NO       PIC 9(08).
               10  TRN-DR-CHQ-NO-TO              PIC 9(08).
               10  TRN-DR-SETL-TYP               PIC X(04).
               10  TRN-DR-AVAIL-BAL              PIC S9(15)      COMP-3.
      **         { WORK AVAILABLE BAL BEFORE DR }
8/5            10  FILLER                        PIC X(915).
8/5   *D       10  FILLER                        PIC X(924).

           05  TRANS-START-DEP-DETLS    REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(14).
               10  TRN-SD-DEP-TYP                PIC X(02).
               10  TRN-SD-DTE-LST-TRN            PIC 9(08).
               10  TRN-SD-TIME-LST-TRN           PIC 9(06).
               10  TRN-SD-DTE-TRN-START          PIC 9(08).
               10  TRN-SD-DTE-VAL-START          PIC 9(08).
               10  TRN-SD-DTE-TRN-END            PIC 9(08).
               10  TRN-SD-DTE-VAL-END            PIC 9(08).
               10  TRN-SD-CCY                    PIC X(03).
               10  TRN-SD-CCY-DEC                PIC 9(01).
               10  TRN-SD-ORIG-BAL               PIC S9(15)      COMP-3.
               10  TRN-SD-BAL                    PIC S9(15)      COMP-3.
               10  FILLER                        PIC X(100).
               10  TRN-SD-CONTRACT-PRD           PIC X(03).
               10  TRN-SD-NO-OF-DAYS             PIC S9(05)      COMP-3.
               10  TRN-SD-RTE-QUOTED-BRCH        PIC X(04).
               10  TRN-SD-RTE-QUOTED-BY          PIC X(08).
               10  TRN-SD-CR-INT-RTE             PIC S9(3)V9(6)  COMP-3.
               10  TRN-SD-CR-COST-RTE-1          PIC S9(3)V9(6)  COMP-3.
               10  TRN-SD-CR-MKDN-RTE-1          PIC S9(3)V9(6)  COMP-3.
               10  TRN-SD-CR-COST-RTE-2          PIC S9(3)V9(6)  COMP-3.
               10  TRN-SD-CR-MKDN-RTE-2          PIC S9(3)V9(6)  COMP-3.
               10  TRN-SD-FUND-COST-RTE          PIC S9(3)V9(6)  COMP-3.
               10  FILLER                        PIC X(160).
               10  TRN-SD-CR-INT-ACCRUED         PIC S9(15)      COMP-3.
               10  TRN-SD-CR-ORIG-INT-WDL        PIC S9(15)      COMP-3.
               10  TRN-SD-CR-INT-WDL-BAL         PIC S9(15)      COMP-3.
               10  TRN-SD-WTAX-AMT               PIC S9(15)      COMP-3.
               10  TRN-SD-EMARK-FULL-IND         PIC X(01).
               10  TRN-SD-EMARK-AMT              PIC S9(15)      COMP-3.
               10  TRN-SD-ENTER-ID               PIC X(08).
               10  TRN-SD-ENTER-BRCH             PIC X(04).
               10  TRN-SD-ENTER-SECT             PIC X(04).
               10  TRN-SD-PROC-BRCH              PIC X(04).
               10  TRN-SD-PROC-SECT              PIC X(04).
               10  TRN-SD-PERSON-DLT-WITH        PIC X(30).
               10  TRN-SD-ROL-DAY                PIC 9(02).
               10  TRN-SD-LOCAL-IND              PIC X(01).
               10  TRN-SD-TAG-IND                PIC X(02).
               10  TRN-SD-DTE-TAG                PIC 9(08).
               10  TRN-SD-ADVICE-PRT-STATUS      PIC X(02).
               10  FILLER                        PIC X(150).
               10  TRN-SD-REMARK-1               PIC X(60).
               10  TRN-SD-REMARK-2               PIC X(60).
               10  TRN-SD-USER-FLD-DETLS OCCURS 5.
                   15  TRN-SD-USER-FLD           PIC X(02).
                   15  TRN-SD-USER-FLD-NARR      PIC X(20).
               10  TRN-SD-SETLED-IND             PIC X(01).
               10  TRN-SD-WDL-INITIATED-IND      PIC X(01).
3/8            10  TRN-SD-DEP-SCHEME             PIC X(02).
3/8            10  FILLER                        PIC X(201).
               10  TRN-SD-SOF-REMARK             PIC X(60).
               10  TRN-SD-DTE-SETL               PIC 9(08).
               10  TRN-SD-FX-DETLS  OCCURS 2.
                   15  TRN-SD-FX-REF             PIC X(16).
8/5   *D           15  TRN-SD-FX-CONV-RTE        PIC S9(4)V9(7)  COMP-3.
8/5                15  TRN-SD-FX-CONV-RTE        PIC S9(7)V9(10) COMP-3.
                   15  TRN-SD-PARTIAL-AMT        PIC S9(15)      COMP-3.
               10  TRN-SD-ROL-OLD-DEP-IND        PIC X(01).
               10  TRN-SD-OLD-DEP-NO             PIC X(11).
               10  TRN-SD-CONTRACT-REF           PIC X(08).
               10  TRN-SD-MATDTE-CHG-IND         PIC X(01).
               10  TRN-SD-INTRTE-CHG-IND         PIC X(01).
               10  TRN-SD-WTAX-IND               PIC X(01).
               10  TRN-SD-SETLDTE-CHG-IND        PIC X(01).
8/5            10  FILLER                        PIC X(17).
8/5   *D       10  FILLER                        PIC X(23).

           05  TRANS-INT-PAID-DETLS   REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(14).
               10  TRN-IPD-DTE-TRN               PIC 9(08).
               10  TRN-IPD-DTE-VAL               PIC 9(08).
               10  TRN-IPD-AMT                   PIC S9(15)      COMP-3.
               10  TRN-IPD-CR-INT-WDL-BAL        PIC S9(15)      COMP-3.
               10  TRN-IPD-WTAX-AMT              PIC S9(15)      COMP-3.
8/5   *D       10  TRN-IPD-CONV-RTE              PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-IPD-CONV-RTE              PIC S9(7)V9(10) COMP-3.
               10  TRN-IPD-LOCAL-EQUIV-AMT       PIC S9(15)      COMP-3.
               10  TRN-IPD-REMARK-1              PIC X(60).
               10  TRN-IPD-REF-1 REDEFINES TRN-IPD-REMARK-1
                                                 PIC X(60).
               10  TRN-IPD-REMARK-2              PIC X(60).
               10  TRN-IPD-REF-2 REDEFINES TRN-IPD-REMARK-2
                                                 PIC X(60).
               10  TRN-IPD-AOF-REMARK            PIC X(60).
               10  TRN-IPD-REF-3 REDEFINES TRN-IPD-AOF-REMARK
                                                 PIC X(60).
               10  TRN-IPD-DTE-SETL              PIC 9(08).
               10  TRN-IPD-ROL-NEW-DEP-IND       PIC X(01).
               10  TRN-IPD-NEW-DEP-NO            PIC X(11).
               10  TRN-IPD-PARTIAL-AMT           PIC S9(15)      COMP-3.
               10  TRN-IPD-PARTIAL-AMT2          PIC S9(15)      COMP-3.
8/5   *D       10  TRN-IPD-CONV-RTE2             PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-IPD-CONV-RTE2             PIC S9(7)V9(10) COMP-3.
               10  TRN-IPD-LOCAL-EQUIV-AMT2      PIC S9(15)      COMP-3.
8/5            10  FILLER                        PIC X(956).
8/5   *D       10  FILLER                        PIC X(962).

           05  TRANS-INT-RECV-DETLS   REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(14).
               10  TRN-IRC-DTE-TRN               PIC 9(08).
               10  TRN-IRC-DTE-VAL               PIC 9(08).
               10  TRN-IRC-AMT                   PIC S9(15)      COMP-3.
               10  TRN-IRC-REMARK-1              PIC X(60).
               10  TRN-IRC-REF-1 REDEFINES TRN-IRC-REMARK-1
                                                 PIC X(60).
               10  TRN-IRC-REMARK-2              PIC X(60).
               10  TRN-IRC-REF-2 REDEFINES TRN-IRC-REMARK-2
                                                 PIC X(60).
               10  TRN-IRC-SOF-REMARK            PIC X(60).
               10  TRN-IRC-REF-3 REDEFINES TRN-IRC-SOF-REMARK
                                                 PIC X(60).
               10  TRN-IRC-DTE-SETL              PIC 9(08).
               10  TRN-IRC-PARTIAL-AMT           PIC S9(15)      COMP-3.
               10  TRN-IRC-PARTIAL-AMT2          PIC S9(15)      COMP-3.
8/5   *D       10  TRN-IRC-CONV-RTE              PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-IRC-CONV-RTE              PIC S9(7)V9(10) COMP-3.
8/5   *D       10  TRN-IRC-CONV-RTE2             PIC S9(4)V9(7)  COMP-3.
8/5            10  TRN-IRC-CONV-RTE2             PIC S9(7)V9(10) COMP-3.
               10  TRN-IRC-LOCAL-EQUIV-AMT       PIC S9(15)      COMP-3.
               10  TRN-IRC-LOCAL-EQUIV-AMT2      PIC S9(15)      COMP-3.
8/5            10  FILLER                        PIC X(984).
8/5   *D       10  FILLER                        PIC X(990).

           05  TRANS-UPD-DEP-DETLS    REDEFINES  TRANS-DETLS.
               10  FILLER                        PIC X(46).
               10  TRN-UDP-DTE-TRN-END           PIC 9(08).
               10  TRN-UDP-DTE-VAL-END           PIC 9(08).
               10  FILLER                        PIC X(120).
               10  TRN-UDP-CONTRACT-PRD          PIC X(03).
               10  TRN-UDP-NO-OF-DAYS            PIC S9(05)      COMP-3.
               10  TRN-UDP-RTE-QUOTED-BRCH       PIC X(04).
               10  TRN-UDP-RTE-QUOTED-BY         PIC X(08).
               10  TRN-UDP-CR-INT-RTE            PIC S9(3)V9(6)  COMP-3.
               10  TRN-UDP-CR-COST-RTE-1         PIC S9(3)V9(6)  COMP-3.
               10  TRN-UDP-CR-MKDN-RTE-1         PIC S9(3)V9(6)  COMP-3.
               10  TRN-UDP-CR-COST-RTE-2         PIC S9(3)V9(6)  COMP-3.
               10  TRN-UDP-CR-MKDN-RTE-2         PIC S9(3)V9(6)  COMP-3.
               10  TRN-UDP-FUND-COST-RTE         PIC S9(3)V9(6)  COMP-3.
               10  FILLER                        PIC X(168).
               10  TRN-UDP-CR-ORIG-INT-WDL       PIC S9(15)      COMP-3.
               10  TRN-UDP-CR-INT-WDL-BAL        PIC S9(15)      COMP-3.
               10  TRN-UDP-WTAX-AMT              PIC S9(15)      COMP-3.
               10  FILLER                        PIC X(663).
               10  TRN-UDP-CONTRACT-REF          PIC X(08).
               10  TRN-UDP-DAYS-TO-BORROW        PIC S9(05)      COMP-3.
               10  FILLER                        PIC X(164).
