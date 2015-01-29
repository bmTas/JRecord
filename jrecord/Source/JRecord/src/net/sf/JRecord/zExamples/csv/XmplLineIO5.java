package net.sf.JRecord.zExamples.csv;


import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.TextLineReader;
import net.sf.JRecord.IO.TextLineWriter;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Example of Reading /writing CSV files with names on the first line
 * @author Bruce Martin
 *
 */
public final class XmplLineIO5 {

	    private static final double GST_CONVERSION = 1.1;

	    private String salesFile           = this.getClass().getResource("DTAR020.csv").getFile();
	    private String salesFileOut        = TstConstants.TEMP_DIRECTORY + "DTAR020out1.csv";

	    /**
	     * Example of LineReader / LineWrite classes
	     */
	    private XmplLineIO5() {
	        super();

	        int lineNum = 0;
	        double gstExclusive;
	        AbstractLine saleRecord;

	        try {
	        	// For a Csv File you can let the Reader crate the schema for you
	            TextLineReader reader  = new TextLineReader(null, true);

	            TextLineWriter writer  = new TextLineWriter(true);
	            
	    		CommonBits.setUseCsvLine(true);

	            reader.setDefaultDelim("\t"); // This must be done before the reader is opened
	            reader.setDefaultQuote("\""); // This must be done before the reader is opened
	            reader.open(salesFile);       // Open with a null layout and let the reader create the schema
	            writer.open(salesFileOut);

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
	    	new XmplLineIO5();
	    }
}
