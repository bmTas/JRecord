package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.LayoutGetFieldByName;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Types.Type;

public class RecordAmsPoDownloadDetail {

    public static final String RECORD_NAME = "ams PO Download: Detail";
    
    public final IFieldDetail fldRecordType;
    public final IFieldDetail fldPackQty;
    public final IFieldDetail fldPackCost;
    public final IFieldDetail fldApn;
    public final IFieldDetail fldFiller;
    public final IFieldDetail fldProduct;
    public final IFieldDetail fldPmgDtlTechKey;
    public final IFieldDetail fldCasePackId;
    public final IFieldDetail fldProductName;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordAmsPoDownloadDetail() {
       FieldDetail[] flds = new FieldDetail[9];
       int i = 0;
       
       font = "";
       fldRecordType = createField(flds, i++, "Record Type", Type.ftChar, 0, 1, 2);
       fldPackQty = createField(flds, i++, "Pack Qty", Type.ftAssumedDecimal, 4, 3, 9);
       fldPackCost = createField(flds, i++, "Pack Cost", Type.ftAssumedDecimal, 4, 12, 13);
       fldApn = createField(flds, i++, "APN", Type.ftNumZeroPadded, 0, 25, 13);
       fldFiller = createField(flds, i++, "Filler", Type.ftChar, 0, 38, 1);
       fldProduct = createField(flds, i++, "Product", Type.ftNumZeroPadded, 0, 39, 8);
       fldPmgDtlTechKey = createField(flds, i++, "pmg dtl tech key", Type.ftChar, 0, 72, 15);
       fldCasePackId = createField(flds, i++, "Case Pack id", Type.ftChar, 0, 87, 15);
       fldProductName = createField(flds, i++, "Product Name", Type.ftChar, 0, 101, 50);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtRecordLayout, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordAmsPoDownloadDetail(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldRecordType = l.getFieldFromName("Record Type");        
        fldPackQty = l.getFieldFromName("Pack Qty");        
        fldPackCost = l.getFieldFromName("Pack Cost");        
        fldApn = l.getFieldFromName("APN");        
        fldFiller = l.getFieldFromName("Filler");        
        fldProduct = l.getFieldFromName("Product");        
        fldPmgDtlTechKey = l.getFieldFromName("pmg dtl tech key");        
        fldCasePackId = l.getFieldFromName("Case Pack id");        
        fldProductName = l.getFieldFromName("Product Name");        
    }
    
    public void updateRecordSelection(LayoutDetail l) {
       record.updateRecordSelection(
                  new ExternalFieldSelection("Record Type", "D1", "=")
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
