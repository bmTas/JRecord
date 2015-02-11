package net.sf.JRecord.zExamples.cobol.readWrite;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 * Read / Write Mainframe Cobol file using a Cobol Copybook
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIO6 {

    private static final double GST_CONVERSION = 1.1;

    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = installDir + "DTAR020out.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIO6() {
        super();

        int lineNum = 0;
        double gstExclusive;
        AbstractLine saleRecord;

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
                   fileStructure, ICopybookDialects.FMT_MAINFRAME,
                    CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS, 
                    copybookName, salesFile
            );

            AbstractLineWriter writer  = ioProvider.getLineWriter(reader.getLayout(), salesFileOut); 

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                System.out.print(saleRecord.getFieldValue("KEYCODE-NO").asString()
                        + " " + saleRecord.getFieldValue("QTY-SOLD").asString()
                        + " " + saleRecord.getFieldValue("SALE-PRICE").asString());

                gstExclusive = saleRecord.getFieldValue("SALE-PRICE").asDouble() / GST_CONVERSION;
                saleRecord.getFieldValue("SALE-PRICE").set(gstExclusive);
                writer.write(saleRecord);

                System.out.println(" " + saleRecord.getFieldValue("SALE-PRICE").asString());
            }

            reader.close();
            writer.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
    	new XmplLineIO6();
    }
}
