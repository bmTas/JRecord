       ******************************************************************
       * DTAR1001 - STORE DETAILS EXTRACT FILE                          *
       *                                                                *
       * AUTHOR BRUCE MARTIN   5 SEP 2003                               *
       *                                                                *
       * VERS   DATE   AUTHOR    PURPOSE                                *
       * -------------------------------------------------------------- *
       *  01  05/09/03 B MARTIN  INITIAL VERSION                        *
       *                                                                *
       *                                                                *
       ******************************************************************
                                                                         
        03  DTAR1000-REC.                                                
            10 DTAR1000-STORE-NO       PIC S9(4) COMP.                   
            10 DTAR1000-REGION-NO      PIC S9(4) COMP.                   
            10 DTAR1000-STORE-NAME     PIC X(50).                        
            10 DTAR1000-NEW-STORE      PIC X(1).                         
            10 DTAR1000-ACTIVE-STORE   PIC X(1).                         
            10 DTAR1000-CLOSED-STORE   PIC X(1).                         
            10 DTAR1000-DC-TYPE        PIC X(1).                         
            10 DTAR1000-SRC-TYPE       PIC X(1). 
            10 DTAR1000-HO-TYPE        PIC X(1). 
