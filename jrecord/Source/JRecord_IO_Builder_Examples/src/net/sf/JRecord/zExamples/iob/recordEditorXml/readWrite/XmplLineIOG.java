package net.sf.JRecord.zExamples.iob.recordEditorXml.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Reading / writing files using a RecordEditor-XML copybook
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIOG {


	    /**
	     * Example of<ul>
	     *   <li> Loading an XML copybook (RecordEditor-Xml)
	     *   <li> LineReader / LineWrite classes
	     *   <li> Using <b>isNumeric()</b> method on the fieldValue
	     * </ul>
	     */
	    private XmplLineIOG() {
	        super();

		    double GST_CONVERSION = 1.1;
	
		    String installDir     = TstConstants.SAMPLE_DIRECTORY;
		    String salesFile      = installDir + "DTAR020.bin";
		    String salesFileOut   = installDir + "DTAR020outG.bin";
		    String copybookName   = TstConstants.RE_XML_DIRECTORY
	    					+ "DTAR020.Xml";
	        int lineNum = 0;
	        AbstractFieldValue fldValue;

	        AbstractLine salesRecord;
	        
	        System.out.println("Output File: " + salesFileOut);
	        

	        try {
	        	IIOBuilder ioBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(copybookName);
	            
	            AbstractLineReader reader  = ioBldr.newReader(salesFile);
	            AbstractLineWriter writer  = ioBldr.newWriter(salesFileOut);

	            while ((salesRecord = reader.read()) != null) {
	                AbstractFieldValue keycode = salesRecord.getFieldValue("KEYCODE-NO");
	                AbstractFieldValue qtySold = salesRecord.getFieldValue("QTY-SOLD");
	                AbstractFieldValue salePrice = salesRecord.getFieldValue("SALE-PRICE");
	                lineNum += 1;

	                System.out.print(keycode.asString()
	                        + " " + qtySold.asString()
	                        + " " + salePrice.asString());

	                fldValue = salesRecord.getFieldValue("KEYCODE-NO");
	                if (fldValue.isNumeric()) {
	                	fldValue.set(0);
	                } else {
	                	fldValue.set("");
	                }
	                fldValue = salesRecord.getFieldValue("DATE");
	                if (fldValue.isNumeric()) {
	                	fldValue.set(0);
	                } else {
	                	fldValue.set("");
	                }
	                
	                salePrice.set(salePrice.asDouble() / GST_CONVERSION);
	                writer.write(salesRecord);

	                System.out.println(" " + salePrice.asString());
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
	    	new XmplLineIOG();
	    }
}
