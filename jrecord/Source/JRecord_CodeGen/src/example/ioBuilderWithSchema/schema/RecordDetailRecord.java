package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Types.Type;

public class RecordDetailRecord {

    public static final String RECORD_NAME = "Detail-Record";
    
    public final IFieldDetail fldRecordType;
    public final IFieldDetail fldField1;
    public final IFieldDetail fldField2;
    public final IFieldDetail fldField3;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordDetailRecord() {
       FieldDetail[] flds = new FieldDetail[4];
       int i = 0;
       
       font = "";
       fldRecordType = createField(flds, i++, "Record-Type", Type.ftChar, 0, 1, 1);
       fldField1 = createField(flds, i++, "Field-1", Type.ftChar, 0, 2, 10);
       fldField2 = createField(flds, i++, "Field-2", Type.ftChar, 0, 12, 20);
       fldField3 = createField(flds, i++, "Field-3", Type.ftChar, 0, 32, 10);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtBinaryRecord, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordDetailRecord(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldRecordType = l.getFieldFromName("Record-Type");        
        fldField1 = l.getFieldFromName("Field-1");        
        fldField2 = l.getFieldFromName("Field-2");        
        fldField3 = l.getFieldFromName("Field-3");        
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
