000100*                                                                         
000200*   DTAR020 IS THE OUTPUT FROM DTAB020 FROM THE IML                       
000300*   CENTRAL REPORTING SYSTEM                                              
000400*                                                                         
000500*   CREATED BY BRUCE ARTHUR  19/12/90                                     
000600*                                                                         
000700*   RECORD LENGTH IS 27.                                                  
000800*                                                                         
000900        03  DTAR020-KCODE-STORE-KEY.                                      
001000            05 DTAR020-KEYCODE-NO      PIC X(08).                         
001100            05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.               
001200        03  DTAR020-DATE               PIC S9(07)   COMP-3.               
001300        03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.               
001400        03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.               
001500        03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.               

