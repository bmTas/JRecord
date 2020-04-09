
           01  HEADER-RECORD.
               10 HDR-RECORD-TYPE              PIC X(01).
               10 HDR-CREATION-TIMESTAMP.
                  15 HDR-CREATION-YYYYMMDD     PIC 9(08).
                  15 HDR-CREATION-HHMISS       PIC 9(06).
               10 HDR-PRD-START-TIMESTAMP.
                  15 HDR-PRD-START-YYYYMMDD    PIC 9(08).
                  15 HDR-PRD-START-HHMISS      PIC 9(06).
               10 HDR-PRD-END-TIMESTAMP.
                  15 HDR-PRD-END-YYYYMMDD      PIC 9(08).
                  15 HDR-PRD-END-HHMISS        PIC 9(06).
               10 HDR-ASSET-NUMBER             PIC X(09).
               10 HDR-FILE-SEQUENCE            PIC X(06).
               10 HDR-FILE-NAME                PIC X(40).
               10 HDR-SOURCE-CODE              PIC X(04).
               10 HDR-DESTINATION-CODE         PIC X(04).

           01  SUB-HEADER-RECORD.
               10 SHDR-RECORD-TYPE.
                  15 SHDR-RECORD-CODE          PIC X(01).
                  15 SHDR-RECORD-INDEX         PIC X(01).
               10 SHDR-DATA-TEXT               PIC X(104).

           01  DETAIL-DATA-RECORD.
               10 DDR-RECORD-TYPE              PIC X(01).
               10 DDR-DATA-TEXT                PIC X(105).

           01  TRAILER-RECORD.
               10 TLR-RECORD-TYPE              PIC X(01).
               10 TLR-LIFE-CYCLE               PIC X(01).
               10 TLR-RECORD-COUNT             PIC 9(09).
               10 TLR-FILE-NUMBER              PIC 9(04).
               10 TLR-FILE-COUNT               PIC 9(04).
               10 TLR-CONTROL-TOTAL-1          PIC X(15).
               10 TLR-CONTROL-TOTAL-2          PIC X(15).
               10 TLR-CONTROL-TOTAL-3          PIC X(15).
               10 TLR-CONTROL-TOTAL-4          PIC X(15).
               10 TLR-CONTROL-TOTAL-5          PIC X(15).
               10 FILLER                       PIC X(12).

      ***** END COPYBOOK CB1HTRCB
