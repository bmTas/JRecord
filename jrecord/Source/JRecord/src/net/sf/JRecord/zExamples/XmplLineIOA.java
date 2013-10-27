package net.sf.JRecord.zExamples;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoaderFactory;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Reading / writing files using a RecordEditor-XML copybook
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIOA {


	    /**
	     * Example of
	     * 1) Loading an XML copybook (RecordEditor-Xml)
	     * 2) LineReader / LineWrite classes
	     */
	    private XmplLineIOA() {
	        super();

		    double GST_CONVERSION = 1.1;
	
		    String installDir     = TstConstants.SAMPLE_DIRECTORY;
		    String salesFile      = installDir + "DTAR020.bin";
		    String salesFileOut   = installDir + "DTAR020out.bin";
		    String copybookName   = TstConstants.RE_XML_DIRECTORY
	    					+ "DTAR020.Xml";
	        int lineNum = 0;

	        AbstractLine salesRecord;

	        try {
	            LayoutDetail layout = CopybookLoaderFactory.getInstance().getLayoutRecordEditXml(copybookName, null);
	            
	            /* with XML copybooks, get the file structure from layout */
	            int fileStructure = layout.getFileStructure();
	            
	            AbstractLineReader reader  = LineIOProvider.getInstance().getLineReader(fileStructure);
	            AbstractLineWriter writer  = LineIOProvider.getInstance().getLineWriter(fileStructure);
	            
	            reader.open(salesFile, layout);
	            writer.open(salesFileOut);

	            while ((salesRecord = reader.read()) != null) {
	                AbstractFieldValue keycode = salesRecord.getFieldValue("KEYCODE-NO");
	                AbstractFieldValue qtySold = salesRecord.getFieldValue("QTY-SOLD");
	                AbstractFieldValue salePrice = salesRecord.getFieldValue("SALE-PRICE");
	                lineNum += 1;

	                System.out.print(keycode.asString()
	                        + " " + qtySold.asString()
	                        + " " + salePrice.asString());

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
	    	new XmplLineIOA();
	    }
}
