package example.ioBuilderWithSchema.schema;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

public class SchemaAmsPoDownload {

    public final RecordAmsPoDownloadDetail recordAmsPoDownloadDetail;
    public final RecordAmsPoDownloadHeader recordAmsPoDownloadHeader;
    public final RecordAmsPoDownloadAllocation recordAmsPoDownloadAllocation;
    
    public final LayoutDetail schema;
    
    public SchemaAmsPoDownload() {
    
 	recordAmsPoDownloadDetail = new RecordAmsPoDownloadDetail();
 	recordAmsPoDownloadHeader = new RecordAmsPoDownloadHeader();
 	recordAmsPoDownloadAllocation = new RecordAmsPoDownloadAllocation();
        
        RecordDetail[] recs = {
                recordAmsPoDownloadDetail.record,
                recordAmsPoDownloadHeader.record,
                recordAmsPoDownloadAllocation.record,
        };

        String recordSepString = ""; 
        byte[] recordSep = CommonBits.getEolBytes( null, recordSepString, "");
        schema = new LayoutDetail(
                      "ams PO Download", recs, "", 
                      Constants.rtGroupOfRecords, recordSep, recordSepString, 
                      "", null, Constants.IO_BIN_TEXT);
        
        recordAmsPoDownloadDetail.updateRecordSelection(schema);
        recordAmsPoDownloadHeader.updateRecordSelection(schema);
        recordAmsPoDownloadAllocation.updateRecordSelection(schema);
    }
    
    public SchemaAmsPoDownload(LayoutDetail schema) {
        this.recordAmsPoDownloadDetail = new RecordAmsPoDownloadDetail(schema, schema.getRecord("ams PO Download: Detail"));
        this.recordAmsPoDownloadHeader = new RecordAmsPoDownloadHeader(schema, schema.getRecord("ams PO Download: Header"));
        this.recordAmsPoDownloadAllocation = new RecordAmsPoDownloadAllocation(schema, schema.getRecord("ams PO Download: Allocation"));

        this.schema = schema;
    }
}

