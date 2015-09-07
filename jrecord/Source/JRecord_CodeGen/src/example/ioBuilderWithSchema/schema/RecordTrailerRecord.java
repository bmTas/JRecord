package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;

public class RecordTrailerRecord {

    public static final String RECORD_NAME = "Trailer-Record";
    
    public final IFieldDetail fldRecordType;
    public final IFieldDetail fldRecordCount;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordTrailerRecord() {
       FieldDetail[] flds = new FieldDetail[2];
       int i = 0;
       
       font = "";
       fldRecordType = createField(flds, i++, "Record-Type", Type.ftChar, 0, 1, 1);
       fldRecordCount = createField(flds, i++, "Record-Count", Type.ftNumZeroPaddedPositive, 0, 2, 9);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtBinaryRecord, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordTrailerRecord(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldRecordType = l.getFieldFromName("Record-Type");        
        fldRecordCount = l.getFieldFromName("Record-Count");        
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
