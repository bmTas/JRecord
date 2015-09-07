package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

public class SchemaMultirecordtest {

    public final RecordHeaderRecord recordHeaderRecord;
    public final RecordDetailRecord recordDetailRecord;
    public final RecordTrailerRecord recordTrailerRecord;
    
    public final LayoutDetail schema;
    
    public SchemaMultirecordtest() {
    
 	recordHeaderRecord = new RecordHeaderRecord();
 	recordDetailRecord = new RecordDetailRecord();
 	recordTrailerRecord = new RecordTrailerRecord();
        
        RecordDetail[] recs = {
                recordHeaderRecord.record,
                recordDetailRecord.record,
                recordTrailerRecord.record,
        };

        String recordSepString = ""; 
        byte[] recordSep = CommonBits.getEolBytes( null, recordSepString, "");
        schema = new LayoutDetail(
                      "MultiRecordTest", recs, "", 
                      Constants.rtGroupOfBinaryRecords, recordSep, recordSepString, 
                      "", null, Constants.IO_FIXED_LENGTH_RECORDS);
        
        recordHeaderRecord.updateRecordSelection(schema);
        recordDetailRecord.updateRecordSelection(schema);
        recordTrailerRecord.updateRecordSelection(schema);
    }
    
    public SchemaMultirecordtest(LayoutDetail schema) {
        this.recordHeaderRecord = new RecordHeaderRecord(schema, schema.getRecord("Header-Record"));
        this.recordDetailRecord = new RecordDetailRecord(schema, schema.getRecord("Detail-Record"));
        this.recordTrailerRecord = new RecordTrailerRecord(schema, schema.getRecord("Trailer-Record"));

        this.schema = schema;
    }
}

