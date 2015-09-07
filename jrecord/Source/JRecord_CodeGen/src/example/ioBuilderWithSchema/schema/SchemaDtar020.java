package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

public class SchemaDtar020 {

    public final RecordDtar020 recordDtar020;
    
    public final LayoutDetail schema;
    
    public SchemaDtar020() {
    
 	recordDtar020 = new RecordDtar020();
        
        RecordDetail[] recs = {
                recordDtar020.record,
        };

        String recordSepString = ""; 
        byte[] recordSep = CommonBits.getEolBytes( null, recordSepString, "CP037");
        schema = new LayoutDetail(
                      "DTAR020", recs, "", 
                      Constants.rtBinaryRecord, recordSep, recordSepString, 
                      "CP037", null, Constants.IO_FIXED_LENGTH_RECORDS);
        
        recordDtar020.updateRecordSelection(schema);
    }
    
    public SchemaDtar020(LayoutDetail schema) {
        this.recordDtar020 = new RecordDtar020(schema, schema.getRecord("DTAR020"));

        this.schema = schema;
    }
}

