package example_ioBldr;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

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
    private String salesFileOut   = testDir + "AmsPoDownloadOut.bin";
    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/ioBuilder/gen/ams_PO_Download.Xml"; 
 
    /**
     * Example of LineReader  classes
     */
    public WriteAmsPoDownload() {
        super();

        try {
            IIOBuilder iob = JRecordInterface1.SCHEMA_XML
                                       .newIOBuilder(copybookName);
            AbstractLineWriter writer = iob.newWriter(salesFileOut);
 
            writer.write(createAmsPoDownloadDetail(iob, 11));
            writer.write(createAmsPoDownloadHeader(iob, 22));
            writer.write(createAmsPoDownloadAllocation(iob, 33));

            writer.close();
        } catch (Exception e) {
             System.out.println();

            e.printStackTrace();
        }
    }

    private AbstractLine createAmsPoDownloadDetail(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue("Record Type").set(data);
        l.getFieldValue("Pack Qty").set(data);
        l.getFieldValue("Pack Cost").set(data);
        l.getFieldValue("APN").set(data);
        l.getFieldValue("Filler").set(data);
        l.getFieldValue("Product").set(data);
        l.getFieldValue("pmg dtl tech key").set(data);
        l.getFieldValue("Case Pack id").set(data);
        l.getFieldValue("Product Name").set(data);
    
        return l;
    }
    private AbstractLine createAmsPoDownloadHeader(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue("Record Type").set(data);
        l.getFieldValue("Sequence Number").set(data);
        l.getFieldValue("Vendor").set(data);
        l.getFieldValue("PO").set(data);
        l.getFieldValue("Entry Date").set(data);
        l.getFieldValue("Filler").set(data);
        l.getFieldValue("beg01 code").set(data);
        l.getFieldValue("beg02 code").set(data);
        l.getFieldValue("Department").set(data);
        l.getFieldValue("Expected Reciept Date").set(data);
        l.getFieldValue("Cancel by date").set(data);
        l.getFieldValue("EDI Type").set(data);
        l.getFieldValue("Add Date").set(data);
        l.getFieldValue("Filler").set(data);
        l.getFieldValue("Department Name").set(data);
        l.getFieldValue("Prcoess Type").set(data);
        l.getFieldValue("Order Type").set(data);
    
        return l;
    }
    private AbstractLine createAmsPoDownloadAllocation(IIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue("Record Type").set(data);
        l.getFieldValue("DC Number 1").set(data);
        l.getFieldValue("Pack Quantity 1").set(data);
        l.getFieldValue("DC Number 2").set(data);
        l.getFieldValue("Pack Quantity 2").set(data);
        l.getFieldValue("DC Number 4").set(data);
        l.getFieldValue("Pack Quantity 4").set(data);
        l.getFieldValue("DC Number 5").set(data);
        l.getFieldValue("Pack Quantity 5").set(data);
        l.getFieldValue("DC Number 6").set(data);
        l.getFieldValue("Pack Quantity 6").set(data);
        l.getFieldValue("DC Number 7").set(data);
        l.getFieldValue("Pack Quantity 7").set(data);
        l.getFieldValue("DC Number 8").set(data);
        l.getFieldValue("Pack Quantity 8").set(data);
        l.getFieldValue("DC Number 9").set(data);
        l.getFieldValue("Pack Quantity 9").set(data);
        l.getFieldValue("DC Number 10").set(data);
        l.getFieldValue("Pack Quantity 10").set(data);
    
        return l;
    }
    
    public static void main(String[] args) {
        new WriteAmsPoDownload();
    }
}

