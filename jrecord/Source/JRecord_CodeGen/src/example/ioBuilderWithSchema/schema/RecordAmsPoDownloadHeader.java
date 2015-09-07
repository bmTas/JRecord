package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.LayoutGetFieldByName;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Types.Type;

public class RecordAmsPoDownloadHeader {

    public static final String RECORD_NAME = "ams PO Download: Header";
    
    public final IFieldDetail fldRecordType;
    public final IFieldDetail fldSequenceNumber;
    public final IFieldDetail fldVendor;
    public final IFieldDetail fldPo;
    public final IFieldDetail fldEntryDate;
    public final IFieldDetail fldFiller;
    public final IFieldDetail fldBeg01Code;
    public final IFieldDetail fldBeg02Code;
    public final IFieldDetail fldDepartment;
    public final IFieldDetail fldExpectedRecieptDate;
    public final IFieldDetail fldCancelByDate;
    public final IFieldDetail fldEdiType;
    public final IFieldDetail fldAddDate;
    public final IFieldDetail fldFiller1;
    public final IFieldDetail fldDepartmentName;
    public final IFieldDetail fldPrcoessType;
    public final IFieldDetail fldOrderType;
 
    public final RecordDetail record;
    
    private final String font;
    
    public RecordAmsPoDownloadHeader() {
       FieldDetail[] flds = new FieldDetail[17];
       int i = 0;
       
       font = "";
       fldRecordType = createField(flds, i++, "Record Type", Type.ftChar, 0, 1, 2);
       fldSequenceNumber = createField(flds, i++, "Sequence Number", Type.ftAssumedDecimal, 3, 3, 5);
       fldVendor = createField(flds, i++, "Vendor", Type.ftNumZeroPadded, 0, 8, 10);
       fldPo = createField(flds, i++, "PO", Type.ftAssumedDecimal, 0, 18, 12);
       fldEntryDate = createField(flds, i++, "Entry Date", Type.ftChar, 0, 30, 6);
       fldFiller = createField(flds, i++, "Filler", Type.ftChar, 0, 36, 8);
       fldBeg01Code = createField(flds, i++, "beg01 code", Type.ftChar, 0, 44, 2);
       fldBeg02Code = createField(flds, i++, "beg02 code", Type.ftChar, 0, 46, 2);
       fldDepartment = createField(flds, i++, "Department", Type.ftChar, 0, 48, 4);
       fldExpectedRecieptDate = createField(flds, i++, "Expected Reciept Date", Type.ftChar, 0, 52, 6);
       fldCancelByDate = createField(flds, i++, "Cancel by date", Type.ftChar, 0, 58, 6);
       fldEdiType = createField(flds, i++, "EDI Type", Type.ftChar, 0, 68, 1);
       fldAddDate = createField(flds, i++, "Add Date", Type.ftChar, 0, 69, 6);
       fldFiller1 = createField(flds, i++, "Filler", Type.ftChar, 0, 75, 1);
       fldDepartmentName = createField(flds, i++, "Department Name", Type.ftChar, 0, 76, 10);
       fldPrcoessType = createField(flds, i++, "Prcoess Type", Type.ftChar, 0, 86, 1);
       fldOrderType = createField(flds, i++, "Order Type", Type.ftChar, 0, 87, 2);

      String t = "	"; 	
      if (t == "\t") {
          t = "<tab>";
      }
      record = new RecordDetail(RECORD_NAME,
	            Constants.rtRecordLayout, t, "",
	            font, flds, 0);
      record.setParentRecordIndex( -1);
	    
    }
    
    public RecordAmsPoDownloadHeader(LayoutDetail l, RecordDetail r) {
    
        font = l.getFontName();
        record = r;
        
        fldRecordType = l.getFieldFromName("Record Type");        
        fldSequenceNumber = l.getFieldFromName("Sequence Number");        
        fldVendor = l.getFieldFromName("Vendor");        
        fldPo = l.getFieldFromName("PO");        
        fldEntryDate = l.getFieldFromName("Entry Date");        
        fldFiller = l.getFieldFromName("Filler");        
        fldBeg01Code = l.getFieldFromName("beg01 code");        
        fldBeg02Code = l.getFieldFromName("beg02 code");        
        fldDepartment = l.getFieldFromName("Department");        
        fldExpectedRecieptDate = l.getFieldFromName("Expected Reciept Date");        
        fldCancelByDate = l.getFieldFromName("Cancel by date");        
        fldEdiType = l.getFieldFromName("EDI Type");        
        fldAddDate = l.getFieldFromName("Add Date");        
        fldFiller1 = l.getFieldFromName("Filler");        
        fldDepartmentName = l.getFieldFromName("Department Name");        
        fldPrcoessType = l.getFieldFromName("Prcoess Type");        
        fldOrderType = l.getFieldFromName("Order Type");        
    }
    
    public void updateRecordSelection(LayoutDetail l) {
       record.updateRecordSelection(
                  new ExternalFieldSelection("Record Type", "H1", "=")
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
