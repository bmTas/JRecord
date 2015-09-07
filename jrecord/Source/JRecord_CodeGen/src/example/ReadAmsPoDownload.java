package example;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadDetail;
import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadHeader;
import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadAllocation;
import example.ioBuilderWithSchema.schema.SchemaAmsPoDownload;
	

/**
 * Read Cobol file using a Cobol Copybook (AmsPoDownload).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class ReadAmsPoDownload {

    private String salesFile      = "C:\\Users\\Bruce01\\.RecordEditor\\HSQLDB\\SampleFiles\\Ams_PODownload_20041231.txt";

    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/gen/ams_PO_Download.Xml"; 
 
    /**
     * Example of LineReader  classes
     */
    public ReadAmsPoDownload() {
        super();

        AbstractLine saleRecord;
        int lineNum = 0;

        try {
            IIOBuilder iob = JRecordInterface1.SCHEMA_XML
                                       .newIOBuilder(copybookName);
            SchemaAmsPoDownload schemaAmsPoDownload
                    = new SchemaAmsPoDownload(iob.getLayout()); 
            RecordAmsPoDownloadDetail rAmsPoDownloadDetail = schemaAmsPoDownload.recordAmsPoDownloadDetail;
            RecordAmsPoDownloadHeader rAmsPoDownloadHeader = schemaAmsPoDownload.recordAmsPoDownloadHeader;
            RecordAmsPoDownloadAllocation rAmsPoDownloadAllocation = schemaAmsPoDownload.recordAmsPoDownloadAllocation;
            AbstractLineReader reader = iob.newReader(salesFile);
 
            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;
                if ("H1".equals(saleRecord.getFieldValue(rAmsPoDownloadDetail.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rAmsPoDownloadDetail.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldPackQty).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldPackCost).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldApn).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldFiller).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldProduct).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldPmgDtlTechKey).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldCasePackId).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadDetail.fldProductName).asString()
                   );
                }
                if ("D1".equals(saleRecord.getFieldValue(rAmsPoDownloadDetail.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rAmsPoDownloadHeader.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldSequenceNumber).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldVendor).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldPo).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldEntryDate).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldFiller).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldBeg01Code).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldBeg02Code).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldDepartment).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldExpectedRecieptDate).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadHeader.fldCancelByDate).asString()
                   );
                }
                if ("S1".equals(saleRecord.getFieldValue(rAmsPoDownloadDetail.fldRecordType).asString())) {
                   System.out.println(
                              saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldRecordType).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber1).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity1).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber2).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity2).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber4).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity4).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber5).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity5).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber6).asString()
                      + " " + saleRecord.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity6).asString()
                   );
                }
            }

            reader.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e);
            System.out.println();

            e.printStackTrace();
        }
    }
    
    public static void main(String[] args) {
        new ReadAmsPoDownload();
    }
}

