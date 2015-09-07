package example;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadDetail;
import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadHeader;
import example.ioBuilderWithSchema.schema.RecordAmsPoDownloadAllocation;
import example.ioBuilderWithSchema.schema.SchemaAmsPoDownload;


/**
 * Write Cobol file using a Cobol Copybook (AmsPoDownload).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class WriteAmsPoDownload {

    private String testDir        = "G:/temp/";
    private String salesFileOut   = testDir + "DTAR020out.bin";
    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/gen/ams_PO_Download.Xml"; 

    private RecordAmsPoDownloadDetail rAmsPoDownloadDetail;
    private RecordAmsPoDownloadHeader rAmsPoDownloadHeader;
    private RecordAmsPoDownloadAllocation rAmsPoDownloadAllocation;
    
    /**
     * Example of LineReader  classes
     */
    public WriteAmsPoDownload() {
        super();

        try {
            IIOBuilder iob = JRecordInterface1.SCHEMA_XML
                                       .newIOBuilder(copybookName);
            SchemaAmsPoDownload schemaAmsPoDownload
                    = new SchemaAmsPoDownload(iob.getLayout()); 
            rAmsPoDownloadDetail = schemaAmsPoDownload.recordAmsPoDownloadDetail;
            rAmsPoDownloadHeader = schemaAmsPoDownload.recordAmsPoDownloadHeader;
            rAmsPoDownloadAllocation = schemaAmsPoDownload.recordAmsPoDownloadAllocation;
            AbstractLineWriter writer = iob.newWriter(salesFileOut);
 
            writer.write(createAmsPoDownloadDetail(iob, 111));
            writer.write(createAmsPoDownloadHeader(iob, 211));
            writer.write(createAmsPoDownloadAllocation(iob, 311));
            writer.write(createAmsPoDownloadDetail(iob, 122));
            writer.write(createAmsPoDownloadHeader(iob, 222));
            writer.write(createAmsPoDownloadAllocation(iob, 322));

            writer.close();
        } catch (Exception e) {
             System.out.println();

            e.printStackTrace();
        }
    }

    private AbstractLine createAmsPoDownloadDetail(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rAmsPoDownloadDetail.fldRecordType).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldPackQty).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldPackCost).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldApn).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldFiller).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldProduct).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldPmgDtlTechKey).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldCasePackId).set(data);
        l.getFieldValue(rAmsPoDownloadDetail.fldProductName).set(data);
    
        return l;
    }

    private AbstractLine createAmsPoDownloadHeader(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rAmsPoDownloadHeader.fldRecordType).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldSequenceNumber).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldVendor).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldPo).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldEntryDate).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldFiller).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldBeg01Code).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldBeg02Code).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldDepartment).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldExpectedRecieptDate).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldCancelByDate).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldEdiType).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldAddDate).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldFiller1).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldDepartmentName).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldPrcoessType).set(data);
        l.getFieldValue(rAmsPoDownloadHeader.fldOrderType).set(data);
    
        return l;
    }

    private AbstractLine createAmsPoDownloadAllocation(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue(rAmsPoDownloadAllocation.fldRecordType).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber1).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity1).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber2).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity2).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber4).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity4).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber5).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity5).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber6).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity6).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber7).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity7).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber8).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity8).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber9).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity9).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldDcNumber10).set(data);
        l.getFieldValue(rAmsPoDownloadAllocation.fldPackQuantity10).set(data);
    
        return l;
    }

    public static void main(String[] args) {
        new WriteAmsPoDownload();
    }
}

