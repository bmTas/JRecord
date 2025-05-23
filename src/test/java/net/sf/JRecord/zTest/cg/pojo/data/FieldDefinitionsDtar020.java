package net.sf.JRecord.zTest.cg.pojo.data;
 /*
  *          Field Name's for DTAR020
  * ------------------------------------------------------------------   
  *
  *   This class holds the field names of Cobol-Copybook DTAR020
  *   It will allow you use code completion on <i>Cobol Field Names</i> in your
  *   java Programs 
  *        
  * *------------- Keep this notice in your final code ---------------
  * *   This code was generated by JRecord projects CodeGen program
  * *            on the: 2025/04/24 0:0:0 
  * *     from Copybook: DTAR020
  * *          Template: pojo
  * *             Split: None   
  * * File Organization: FixedWidth   
  * *              Font: cp037
  * *   
  * *    CodeGen Author: Bruce Martin
  * *-----------------------------------------------------------------
  *
  *
  *   Please supply any program fixes/enhancements/documentation
  *   back to the JRecord project (https://sourceforge.net/projects/jrecord/)
  *   so other people can benefit !!!
  * 
  *
  *          Bruce Martin (JRecord / CodeGen Author) 
  *
  *
  * ------------------------------------------------------------------
  * v01  CodeGen        2025/04/24  Initial version
  *     (Bruce Martin)
  */

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;

public class FieldDefinitionsDtar020 {

    public static  RecordDtar020 getFieldsDtar020(LayoutDetail schema) {
        return new RecordDtar020(schema);
    }

    public static class RecordDtar020 {
	   /**
	    * Cobol Field DTAR020-KEYCODE-NO      Pic X(08)      (Position/Length 1/8)
	    */
       public final IFieldDetail keycodeNo;
	   /**
	    * Cobol Field DTAR020-STORE-NO      Pic S9(03)     computational-3 (Position/Length 9/2)
	    */
       public final IFieldDetail storeNo;
	   /**
	    * Cobol Field DTAR020-DATE      Pic S9(07)     computational-3 (Position/Length 11/4)
	    */
       public final IFieldDetail date;
	   /**
	    * Cobol Field DTAR020-DEPT-NO      Pic S9(03)     computational-3 (Position/Length 15/2)
	    */
       public final IFieldDetail deptNo;
	   /**
	    * Cobol Field DTAR020-QTY-SOLD      Pic S9(9)     computational-3 (Position/Length 17/5)
	    */
       public final IFieldDetail qtySold;
	   /**
	    * Cobol Field DTAR020-SALE-PRICE      Pic S9(9)V99     computational-3 (Position/Length 22/6)
	    */
       public final IFieldDetail salePrice;
	
	    public RecordDtar020(LayoutDetail schema) {
            keycodeNo = schema.getFieldFromName("DTAR020-KEYCODE-NO");
            storeNo = schema.getFieldFromName("DTAR020-STORE-NO");
            date = schema.getFieldFromName("DTAR020-DATE");
            deptNo = schema.getFieldFromName("DTAR020-DEPT-NO");
            qtySold = schema.getFieldFromName("DTAR020-QTY-SOLD");
            salePrice = schema.getFieldFromName("DTAR020-SALE-PRICE");
	
	    }	
    }


}

