package example_ioBldr;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.IIOBuilder;


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

    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/ioBuilder/gen/ams_PO_Download.Xml"; 
 
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
            AbstractLineReader reader = iob.newReader(salesFile);
 
            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;
                if ("H1".equals(saleRecord.getFieldValue("Record Type").asString())) {
                   System.out.println(
                              saleRecord.getFieldValue("Record Type").asString()
                      + " " + saleRecord.getFieldValue("Pack Qty").asString()
                      + " " + saleRecord.getFieldValue("Pack Cost").asString()
                      + " " + saleRecord.getFieldValue("APN").asString()
                      + " " + saleRecord.getFieldValue("Filler").asString()
                      + " " + saleRecord.getFieldValue("Product").asString()
                      + " " + saleRecord.getFieldValue("pmg dtl tech key").asString()
                      + " " + saleRecord.getFieldValue("Case Pack id").asString()
                      + " " + saleRecord.getFieldValue("Product Name").asString()
                   );
                }
                if ("D1".equals(saleRecord.getFieldValue("Record Type").asString())) {
                   System.out.println(
                              saleRecord.getFieldValue("Record Type").asString()
                      + " " + saleRecord.getFieldValue("Sequence Number").asString()
                      + " " + saleRecord.getFieldValue("Vendor").asString()
                      + " " + saleRecord.getFieldValue("PO").asString()
                      + " " + saleRecord.getFieldValue("Entry Date").asString()
                      + " " + saleRecord.getFieldValue("Filler").asString()
                      + " " + saleRecord.getFieldValue("beg01 code").asString()
                      + " " + saleRecord.getFieldValue("beg02 code").asString()
                      + " " + saleRecord.getFieldValue("Department").asString()
                      + " " + saleRecord.getFieldValue("Expected Reciept Date").asString()
                   );
                }
                if ("S1".equals(saleRecord.getFieldValue("Record Type").asString())) {
                   System.out.println(
                              saleRecord.getFieldValue("Record Type").asString()
                      + " " + saleRecord.getFieldValue("DC Number 1").asString()
                      + " " + saleRecord.getFieldValue("Pack Quantity 1").asString()
                      + " " + saleRecord.getFieldValue("DC Number 2").asString()
                      + " " + saleRecord.getFieldValue("Pack Quantity 2").asString()
                      + " " + saleRecord.getFieldValue("DC Number 4").asString()
                      + " " + saleRecord.getFieldValue("Pack Quantity 4").asString()
                      + " " + saleRecord.getFieldValue("DC Number 5").asString()
                      + " " + saleRecord.getFieldValue("Pack Quantity 5").asString()
                      + " " + saleRecord.getFieldValue("DC Number 6").asString()
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

