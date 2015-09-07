package example_ioBldr;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

/**
 * Write Cobol file using a Cobol Copybook (Dtar020).
 *
 * This Generated program is intended as an example of using JRecord
 * rather than a useful program (that compiles - it wont).
 * You should regard it as a starting point and modify 
 * it according to needs
 */
public final class WriteDtar020 {

    private String testDir        = "G:/temp/";
    private String salesFileOut   = testDir + "DTAR020out.bin";
    private String copybookName   = "/F:/Work/EclipseWorkspaces/std_workspace/JRecord_CodeGen/bin/test/ioBuilder/gen/DTAR020.cbl"; 
 
    /**
     * Example of LineReader  classes
     */
    public WriteDtar020() {
        super();

        try {
            ICobolIOBuilder iob = JRecordInterface1.COBOL
                                       .newIOBuilder(copybookName)
                                           .setFont("CP037")
                                           .setFileOrganization(Constants.IO_FIXED_LENGTH)
                                           .setSplitCopybook(CopybookLoader.SPLIT_NONE)
                                           .setDropCopybookNameFromFields(true);  
             AbstractLineWriter writer = iob.newWriter(salesFileOut);
 
            writer.write(createDtar020(iob, 1));

            writer.close();
        } catch (Exception e) {
             System.out.println();

            e.printStackTrace();
        }
    }

    private AbstractLine createDtar020(ICobolIOBuilder iob, int data) throws RecordException, IOException {
        AbstractLine l = iob.newLine();    
        
        l.getFieldValue("KEYCODE-NO").set(data);
        l.getFieldValue("STORE-NO").set(data);
        l.getFieldValue("DATE").set(data);
        l.getFieldValue("DEPT-NO").set(data);
        l.getFieldValue("QTY-SOLD").set(data);
        l.getFieldValue("SALE-PRICE").set(data);
    
        return l;
    }
    
    public static void main(String[] args) {
        new WriteDtar020();
    }
}

