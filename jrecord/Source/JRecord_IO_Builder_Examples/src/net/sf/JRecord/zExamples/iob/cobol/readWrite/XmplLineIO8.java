package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Example of writing a File using a RecordEditor - XML copybook 
 * definition
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIO8 {


	    /**
	     * Example of
	     * 1) Loading an XML copybook (RecordEditor-Xml) - External Record
	     * 3) LineWrite classes
	     */
	    private XmplLineIO8() {
	        super();
	
		    String installDir     = TstConstants.SAMPLE_DIRECTORY;
		    String salesFileOut   = installDir + "DTAR020out8.bin";
		    String copybookName   = TstConstants.RE_XML_DIRECTORY + "DTAR020.Xml";
	        int lineNum = 0;
	        
	        IIOBuilder ioBldr = JRecordInterface1.SCHEMA_XML .newIOBuilder(copybookName);


	        try {
	            AbstractLine saleRecord = ioBldr.newLine();
	            AbstractLineWriter writer  = ioBldr.newWriter(salesFileOut);

	            saleRecord.getFieldValue("KEYCODE-NO").set(1331);
	            saleRecord.getFieldValue("STORE-NO").set(1);
	            saleRecord.getFieldValue("DATE").set(80921);
	            saleRecord.getFieldValue("DEPT-NO").set(100);
	            saleRecord.getFieldValue("QTY-SOLD").set(7);
	            saleRecord.getFieldValue("SALE-PRICE").set(7.00);
	            writer.write(saleRecord);
	            
	            saleRecord.getFieldValue("STORE-NO").set(11);
	            writer.write(saleRecord);
	            
	            saleRecord.getFieldValue("STORE-NO").set(121);
	            writer.write(saleRecord);

	            System.out.println(" " + saleRecord.getFieldValue("SALE-PRICE").asString());
	            writer.close();
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIO8();
	    }
}
