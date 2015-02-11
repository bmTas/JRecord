package net.sf.JRecord.zExamples.cobol.iobuilder;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Read / Write Mainframe Cobol file using a 
 * Cobol Copybook - <b>CobolIOBuilder</b> version.
 * 
 * <p><b>Note:</b> The input and output file formats are exactly the same. 
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplReadWrite02 {

    private static final double GST_CONVERSION = 1.1;

    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = installDir + "DTAR023out.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";
    private String outCopybookame = this.getClass().getResource("DTAR023.cbl").getFile();

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplReadWrite02() {
        super();

        int lineNum = 0;
        double gstExclusive, price, gst;
        AbstractLine saleRecord;

        try {
        	ICobolIOBuilder iobIn  = CobolIoProvider.getInstance()
        								.newIOBuilder(copybookName)
        									.setFont("cp037")       
        									.setFileOrganization(Constants.IO_FIXED_LENGTH_RECORDS);  
        	ICobolIOBuilder iobOut  = CobolIoProvider.getInstance()
										.newIOBuilder(outCopybookame)
											.setFont("cp037")       
											.setFileOrganization(Constants.IO_FIXED_LENGTH_RECORDS);  
            AbstractLineReader reader = iobIn.newReader(salesFile);
            AbstractLineWriter writer = iobOut.newWriter(salesFileOut);
            AbstractLine outLine = iobOut.newLine();

            while ((saleRecord = reader.read()) != null) {
                long dateCCYYMMDD = 20000000 + saleRecord.getFieldValue("DTAR020-DATE").asLong();
                price = saleRecord.getFieldValue("DTAR020-SALE-PRICE").asDouble();
				gstExclusive = price / GST_CONVERSION;
				gst = price - gstExclusive;
				
                outLine.getFieldValue("DTAR023-KEYCODE-NO").set(saleRecord.getFieldValue("DTAR020-KEYCODE-NO").asString());
                outLine.getFieldValue("DTAR023-STORE-NO").set(saleRecord.getFieldValue("DTAR020-STORE-NO").asString());
 				outLine.getFieldValue("DTAR023-DATE").set(dateCCYYMMDD);
                outLine.getFieldValue("DTAR023-QTY-SOLD").set(saleRecord.getFieldValue("DTAR020-QTY-SOLD").asString());
                outLine.getFieldValue("DTAR023-SALE-PRICE").set(saleRecord.getFieldValue("DTAR020-SALE-PRICE").asString());
                outLine.getFieldValue("DTAR023-GST").set(gst);
 
                writer.write(outLine);
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
    	new XmplReadWrite02();
    }
}
