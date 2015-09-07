package example_ioBldr;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 * Read Cobol file using a Cobol Copybook (Dtar020).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class ReadDtar020 {

    private String testDir        = "G:/temp/";
    private String salesFile      = testDir + "DTAR020.bin";

    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/ioBuilder/gen/DTAR020.cbl"; 
 
    /**
     * Example of LineReader  classes
     */
    public ReadDtar020() {
        super();

        AbstractLine saleRecord;
        int lineNum = 0;

        try {
            ICobolIOBuilder iob = JRecordInterface1.COBOL
                                       .newIOBuilder(copybookName)
                                           .setFont("CP037")
                                           .setFileOrganization(Constants.IO_FIXED_LENGTH)
                                           .setSplitCopybook(CopybookLoader.SPLIT_NONE)
                                           .setDropCopybookNameFromFields(true);  
            AbstractLineReader reader = iob.newReader(salesFile);
 
            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;
                if (true) {
                   System.out.println(
                              saleRecord.getFieldValue("KEYCODE-NO").asString()
                      + " " + saleRecord.getFieldValue("STORE-NO").asString()
                      + " " + saleRecord.getFieldValue("DATE").asString()
                      + " " + saleRecord.getFieldValue("DEPT-NO").asString()
                      + " " + saleRecord.getFieldValue("QTY-SOLD").asString()
                      + " " + saleRecord.getFieldValue("SALE-PRICE").asString()
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
        new ReadDtar020();
    }
}

