package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;

public class RecordDtar020 {

    public static final String RECORD_NAME = "DTAR020";
    
    public final IFieldDetail fldKeycodeNo;
    public final IFieldDetail fldStoreNo;
    public final IFieldDetail fldDate;
    public final IFieldDetail fldDeptNo;
    public final IFieldDetail fldQtySold;
    public final IFieldDetail fldSalePrice;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordDtar020() {
       FieldDetail[] flds = new FieldDetail[6];
       int i = 0;
       
       font = "CP037";
       fldKeycodeNo = createField(flds, i++, "KEYCODE-NO", Type.ftChar, 0, 1, 8);
       fldStoreNo = createField(flds, i++, "STORE-NO", Type.ftPackedDecimal, 0, 9, 2);
       fldDate = createField(flds, i++, "DATE", Type.ftPackedDecimal, 0, 11, 4);
       fldDeptNo = createField(flds, i++, "DEPT-NO", Type.ftPackedDecimal, 0, 15, 2);
       fldQtySold = createField(flds, i++, "QTY-SOLD", Type.ftPackedDecimal, 0, 17, 5);
       fldSalePrice = createField(flds, i++, "SALE-PRICE", Type.ftPackedDecimal, 2, 22, 6);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtBinaryRecord, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordDtar020(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldKeycodeNo = l.getFieldFromName("KEYCODE-NO");        
        fldStoreNo = l.getFieldFromName("STORE-NO");        
        fldDate = l.getFieldFromName("DATE");        
        fldDeptNo = l.getFieldFromName("DEPT-NO");        
        fldQtySold = l.getFieldFromName("QTY-SOLD");        
        fldSalePrice = l.getFieldFromName("SALE-PRICE");        
    }
    
    public void updateRecordSelection(LayoutDetail l) {
    }
    
    private FieldDetail createField(
                FieldDetail[] flds, int idx,
                String name,
	        int type,
	        int decimal,
	        int pos,
		int len) {
        flds[idx] = FieldDetail.newFixedWidthField(name, type, pos, len, decimal, font);
        return flds[idx];
    }
}
