000100******************************************************************
000200*                                                                *
000300*   DTAR107 - CUSTOMER FILE                                      *
000400*                                                                *
000500*   FORMAT  -  FB   54 CHARS.                                    *
000600*                                                                *
000700*  VER| DATE     | DESCRIPTION                  | AUTHOR         *
000800*  ---|----------|------------------------------|------------    *
000900*  01 | FEB 94   | FIRST ISSUE                  | R. GEALL       *
001000*                                                                *
001100******************************************************************
001200                                                                  
001300     03  DTAR107-STORE-NO               PIC S9(03)    COMP-3.     
001320     03  Filler REDEFINES DTAR107-STORE-NO.                              
001330          05  DTAR107-STORE-NO-REDEF    PIC X(2).                 
001400     03  DTAR107-TRANS-DATE             PIC S9(06)    COMP-3.     
001500     03  DTAR107-CUST-NO                PIC 9(16).                
001600     03  DTAR107-AMOUNT                 PIC S9(07)V99 COMP-3.     
001700     03  DTAR107-OPERATOR-NO            PIC S9(08)    COMP-3.     
001800     03  DTAR107-TERMINAL-NO            PIC S9(03)    COMP-3.    
001900     03  DTAR107-TIME                   PIC S9(04)    COMP-3.    
002000     03  DTAR107-TRANS-NO               PIC S9(04)    COMP-3.    
002100     03  DTAR107-TRANS-TYPE             PIC 9(02).               
002200         88 DTAR107-SALE                   VALUE 1.              
002300         88 DTAR107-REFUND                 VALUE 2.              
002400         88 DTAR107-LAYBY                  VALUE 3.              
002500         88 DTAR107-VOID                   VALUE 4.              
002600     03  DTAR107-TRANS-CODE             PIC 9(02).               
002700         88 DTAR107-SALE-DR                VALUE 10.             
002800         88 DTAR107-REFUND-CR              VALUE 20.             
002900         88 DTAR107-DR-REVERSAL            VALUE 12.             
003000         88 DTAR107-CR-REVERSAL            VALUE 22.             
003100     03  DTAR107-STD-POINTS             PIC S9(06)    COMP-3.    
003200     03  DTAR107-BONUS-POINTS           PIC S9(06)    COMP-3.    
003300     03  DTAR107-NO-OF-TXNS             PIC 9(02).               
003400                                                                 
