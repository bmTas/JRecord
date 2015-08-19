package net.sf.JRecord.zExamples.iob.xml.multiRecord;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.IIOBuilder;



/**
 * <b>Purpose:</b> This program demonstrates adding Record-Selection-Criteria to a Cobol-Copybook.
 * <br>Also it is an example of processing Multiple record types in a file
 * 
 * <p>The key to doing this is to update the <b>ExternalRecord</b>. The <b>ExternalRecord</b> is essentially a 
 * <b>"Builder"</b> class for the <b>LayoutDetail</b> (record-schema) class. My background is not Java so I 
 * do not always no the correct OO terminology.
 * 
 * <pre>
 * This testing can also be done in Java code, See:
 * *  AmsReceipt01 - processing with just Java code no Record-Selection-Criteria. This will use less 
 *                   resources than using the  Record-Selection-Criteria   
 * *  AmsReceipt03 - Combines Record-Selection-Details from a Xml file with Field Details from 
 *                   a Cobol Copybook
 *</pre>                   
 *                   
 * @author Bruce Martin
 *
 */
public class AmsPOcont01 {

	
	private static final String copybookFileName = "amsPODownload.Xml";
	
	//private LayoutDetail schema;
	private IIOBuilder IOBldr;
	
    public static void main(String[] args) throws Exception {
    	new AmsPOcont01();
    }
    
    
    private AmsPOcont01() throws Exception {
    	loadRecordDefinition();
    	
    	readFile();
    }
    
    
    /**
     * Load RecordLayout (schema) from the Xml-File-Schema
     * @throws Exception
     */
    private void loadRecordDefinition() throws Exception{
    	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	//copyName = Conversion.replace(copyName, "/bin/", "/src/").toString();
 
    	IOBldr = JRecordInterface1.SCHEMA_XML
    				.newIOBuilder(copyName)
    					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
    					;

//		
//		This is not needed for an Xml-Schema as it already has the Selection criteria in it:
//    	
//		externalRecord.getRecord("ams PO Download: Detail")
//						.setRecordSelection(new ExternalFieldSelection("Record Type", "D1"));
//		externalRecord.getRecord("ams PO Download: Header")
//						.setRecordSelection(new ExternalFieldSelection("Record Type", "H1"));
//		externalRecord.getRecord("ams PO Download: Allocation")
//						.setRecordSelection(new ExternalFieldSelection("Record Type", "S1"));
    }
    
    
    private void readFile() throws IOException, RecordException {
    	
    	String fname= this.getClass().getResource("ContAms_PODownload_20041231.txt").getFile();
    	AbstractLineReader r = IOBldr.newReader(fname);
    	AbstractLine l;
    	String id;
    	
    	while ((l = r.read()) != null) {
    		id = l.getFieldValue("Record Type").asString();
    		if ("H1".equals(id)) {
    			System.out.println("Header: " 
    					+  l.getFieldValue("Vendor").asString()
    					+  l.getFieldValue("PO").asString()
    					+  l.getFieldValue("Entry Date").asString()
    					+  l.getFieldValue("Department").asString()
    			);
    		} else if ("D1".equals(id)) {
       			System.out.println("  Detail: " 
    					+       l.getFieldValue("APN").asString()
    					+ " " + l.getFieldValue("Product").asString()
    					+ " " + l.getFieldValue("Pack Qty").asString()
    					+ " " + l.getFieldValue("Pack Cost").asString()
       			);
    		} else if ("S1".equals(id)) {
       			System.out.println("  Detail: " 
    					+       l.getFieldValue("DC Number 1").asString()
    					+ " " + l.getFieldValue("Pack Quantity 1").asString()
    					+ " " + l.getFieldValue("DC Number 2").asString()
    					+ " " + l.getFieldValue("Pack Quantity 2").asString()
       			);
    		} else {
    			System.out.print("\t. " + id);
    		}
    	}
    	r.close();
    }
    
    
    /**
     * This method adds a Record-Selection-Test to a Record, three times.
     * The 3 tests will be will be combined with a boolean AND operator   
     *  
     * @param extlayoutCBL Group or parent record
     * @param recordName name of the record to be update
     * @param fieldName Field to Test
     * @param value value to be tested
     */
    @SuppressWarnings("unused")
	private static void addFieldTest3times(ExternalRecord extlayoutCBL, String recordName, String fieldName, String value) {
    	int idx = findRecordIndex(extlayoutCBL, recordName);
    	
    	if (idx < 0) {
    		System.out.println("Record " + recordName + " was not found");
    	} else {
    		// This add a test to the record three times.
    		// The 3 tests will be joined with a logical AND
    		// Obviously this would only make sense if the tests where different.
    		// I did not have any sensible examples handy
    		extlayoutCBL.getRecord(idx).addTstField(fieldName, value);
    		extlayoutCBL.getRecord(idx).addTstField(fieldName, value);
       		extlayoutCBL.getRecord(idx).addTstField(fieldName, value);
    	}
    }

    private static int findRecordIndex(ExternalRecord extlayoutCBL, String recordName) {
    	int ret = -1;
    	
    	for (int i = 0; i < extlayoutCBL.getNumberOfRecords(); i++) {
    		if (recordName.equalsIgnoreCase(extlayoutCBL.getRecord(i).getRecordName())) {
    			ret = i;
    			break;
    		}
    	}
    	
    	return ret;
    }
 }
