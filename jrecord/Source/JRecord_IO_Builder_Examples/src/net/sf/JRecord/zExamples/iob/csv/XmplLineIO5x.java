package net.sf.JRecord.zExamples.iob.csv;


import java.math.BigDecimal;
import java.math.RoundingMode;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Example of Reading /writing CSV files with names on the first line
 * @author Bruce Martin
 *
 */
public final class XmplLineIO5x {

	    private static final BigDecimal GST_CONVERSION = new BigDecimal("1.1");

	    private String inputSchemaFile     = this.getClass().getResource("XML_TabDelimCsvSchema.Xml").getFile();
	    private String outputSchemaFile    = this.getClass().getResource("XML_SaleCsvSchema.Xml").getFile();

	    private String salesFile           = this.getClass().getResource("DTAR020.csv").getFile();
	    private String salesFileOut        = TstConstants.TEMP_DIRECTORY + "DTAR020out1.csv";

	
	    /**
	     * Example of LineReader / LineWrite classes
	     */
	    private XmplLineIO5x() {
	        super();

	        int lineNum = 0;
	        BigDecimal gstExclusive, price, gst;
	        AbstractLine saleRecord;
			CommonBits.setUseCsvLine(true);
	    	
	        try {
	        	IIOBuilder inBldr  = JRecordInterface1.SCHEMA_XML.newIOBuilder(inputSchemaFile);
	        	IIOBuilder outBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(outputSchemaFile);      	

				AbstractLine outCsvRecord = outBldr.newLine();
				
				AbstractLineReader reader = inBldr.newReader(salesFile);
				AbstractLineWriter writer = outBldr.newWriter(salesFileOut);
				
	            while ((saleRecord = reader.read()) != null) {
	                lineNum += 1;

	                outCsvRecord.getFieldValue("Sku")  .set(saleRecord.getFieldValue("KEYCODE-NO").asString());
	                outCsvRecord.getFieldValue("Store").set(saleRecord.getFieldValue("Store-NO").asString());
	                outCsvRecord.getFieldValue("Date") .set(saleRecord.getFieldValue("Date").asString());
	                outCsvRecord.getFieldValue("Dept") .set(saleRecord.getFieldValue("Dept-NO").asString());
	                outCsvRecord.getFieldValue("Qty")  .set(saleRecord.getFieldValue("Qty-Sold").asString());
	                outCsvRecord.getFieldValue("GST")  .set(saleRecord.getFieldValue("KEYCODE-NO").asString());

	                price =  saleRecord.getFieldValue("SALE-PRICE").asBigDecimal();
	                gstExclusive = price.divide(GST_CONVERSION, 2, RoundingMode.HALF_DOWN);
	                gst = price.subtract(gstExclusive);
	                
	                outCsvRecord.getFieldValue("Price").set(gstExclusive);
	                outCsvRecord.getFieldValue("GST")  .set(gst);
	        
	                writer.write(outCsvRecord);
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
	    	new XmplLineIO5x();
	    }
}
