package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.LayoutGetFieldByName;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Types.Type;

public class RecordAmsPoDownloadAllocation {

    public static final String RECORD_NAME = "ams PO Download: Allocation";
    
    public final IFieldDetail fldRecordType;
    public final IFieldDetail fldDcNumber1;
    public final IFieldDetail fldPackQuantity1;
    public final IFieldDetail fldDcNumber2;
    public final IFieldDetail fldPackQuantity2;
    public final IFieldDetail fldDcNumber4;
    public final IFieldDetail fldPackQuantity4;
    public final IFieldDetail fldDcNumber5;
    public final IFieldDetail fldPackQuantity5;
    public final IFieldDetail fldDcNumber6;
    public final IFieldDetail fldPackQuantity6;
    public final IFieldDetail fldDcNumber7;
    public final IFieldDetail fldPackQuantity7;
    public final IFieldDetail fldDcNumber8;
    public final IFieldDetail fldPackQuantity8;
    public final IFieldDetail fldDcNumber9;
    public final IFieldDetail fldPackQuantity9;
    public final IFieldDetail fldDcNumber10;
    public final IFieldDetail fldPackQuantity10;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordAmsPoDownloadAllocation() {
       FieldDetail[] flds = new FieldDetail[19];
       int i = 0;
       
       font = "";
       fldRecordType = createField(flds, i++, "Record Type", Type.ftChar, 0, 1, 2);
       fldDcNumber1 = createField(flds, i++, "DC Number 1", Type.ftNumZeroPadded, 0, 3, 4);
       fldPackQuantity1 = createField(flds, i++, "Pack Quantity 1", Type.ftNumZeroPadded, 0, 7, 8);
       fldDcNumber2 = createField(flds, i++, "DC Number 2", Type.ftNumZeroPadded, 0, 15, 4);
       fldPackQuantity2 = createField(flds, i++, "Pack Quantity 2", Type.ftNumZeroPadded, 0, 19, 8);
       fldDcNumber4 = createField(flds, i++, "DC Number 4", Type.ftChar, 0, 39, 4);
       fldPackQuantity4 = createField(flds, i++, "Pack Quantity 4", Type.ftNumZeroPadded, 0, 43, 8);
       fldDcNumber5 = createField(flds, i++, "DC Number 5", Type.ftNumZeroPadded, 0, 51, 4);
       fldPackQuantity5 = createField(flds, i++, "Pack Quantity 5", Type.ftNumZeroPadded, 0, 55, 8);
       fldDcNumber6 = createField(flds, i++, "DC Number 6", Type.ftNumZeroPadded, 0, 63, 4);
       fldPackQuantity6 = createField(flds, i++, "Pack Quantity 6", Type.ftNumZeroPadded, 0, 67, 8);
       fldDcNumber7 = createField(flds, i++, "DC Number 7", Type.ftNumZeroPadded, 0, 75, 4);
       fldPackQuantity7 = createField(flds, i++, "Pack Quantity 7", Type.ftNumZeroPadded, 0, 79, 8);
       fldDcNumber8 = createField(flds, i++, "DC Number 8", Type.ftNumZeroPadded, 0, 87, 4);
       fldPackQuantity8 = createField(flds, i++, "Pack Quantity 8", Type.ftNumZeroPadded, 0, 91, 8);
       fldDcNumber9 = createField(flds, i++, "DC Number 9", Type.ftNumZeroPadded, 0, 99, 4);
       fldPackQuantity9 = createField(flds, i++, "Pack Quantity 9", Type.ftNumZeroPadded, 0, 103, 8);
       fldDcNumber10 = createField(flds, i++, "DC Number 10", Type.ftNumZeroPadded, 0, 111, 4);
       fldPackQuantity10 = createField(flds, i++, "Pack Quantity 10", Type.ftNumZeroPadded, 0, 115, 8);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtRecordLayout, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordAmsPoDownloadAllocation(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldRecordType = l.getFieldFromName("Record Type");        
        fldDcNumber1 = l.getFieldFromName("DC Number 1");        
        fldPackQuantity1 = l.getFieldFromName("Pack Quantity 1");        
        fldDcNumber2 = l.getFieldFromName("DC Number 2");        
        fldPackQuantity2 = l.getFieldFromName("Pack Quantity 2");        
        fldDcNumber4 = l.getFieldFromName("DC Number 4");        
        fldPackQuantity4 = l.getFieldFromName("Pack Quantity 4");        
        fldDcNumber5 = l.getFieldFromName("DC Number 5");        
        fldPackQuantity5 = l.getFieldFromName("Pack Quantity 5");        
        fldDcNumber6 = l.getFieldFromName("DC Number 6");        
        fldPackQuantity6 = l.getFieldFromName("Pack Quantity 6");        
        fldDcNumber7 = l.getFieldFromName("DC Number 7");        
        fldPackQuantity7 = l.getFieldFromName("Pack Quantity 7");        
        fldDcNumber8 = l.getFieldFromName("DC Number 8");        
        fldPackQuantity8 = l.getFieldFromName("Pack Quantity 8");        
        fldDcNumber9 = l.getFieldFromName("DC Number 9");        
        fldPackQuantity9 = l.getFieldFromName("Pack Quantity 9");        
        fldDcNumber10 = l.getFieldFromName("DC Number 10");        
        fldPackQuantity10 = l.getFieldFromName("Pack Quantity 10");        
    }
    
    public void updateRecordSelection(LayoutDetail l) {
       record.updateRecordSelection(
                  new ExternalFieldSelection("Record Type", "S1", "=")
,
                 new LayoutGetFieldByName(l, record));
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
