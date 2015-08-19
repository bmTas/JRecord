package net.sf.JRecord.zExamples.iob.cobol.multiRecord;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;



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
public class AmsReceipt01 {

	
	private static final String copybookFileName = "AmsReceipt.cbl";
	
	//private LayoutDetail schema;
	private ICobolIOBuilder ioBldr;
	
    public static void main(String[] args) throws Exception {
    	new AmsReceipt01();
    }
    
    
    private AmsReceipt01() throws Exception {
    	loadRecordDefinition();
    	
    	readFile();
    }
    
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private void loadRecordDefinition() throws Exception{
    	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
 
    	ioBldr = JRecordInterface1.COBOL
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
    					.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE)
    					;
    }
    
    
    private void readFile() throws IOException, RecordException {
    	
    	AbstractLineReader r = ioBldr.newReader("G:\\Users\\BruceTst01\\RecordEditor_HSQL\\SampleFiles\\Ams_Receipt_05AUG08190103.txt");
    	AbstractLine l;
    	String id;
    	
    	while ((l = r.read()) != null) {
    		id = l.getFieldValue("STDR-RECORD-TYPE").asString();
    		if ("FH".equals(id)) {
    			System.out.println("fh: " 
    					+         l.getFieldValue("STDR-FH-CRD-DD").asString()
    					+ " / " + l.getFieldValue("STDR-FH-CRD-MM").asString()
    					+ " / " + l.getFieldValue("STDR-FH-CRD-CC").asString()
    					+         l.getFieldValue("STDR-FH-CRD-YY").asString()
    			);
    		} else if ("RH".equals(id)) {
       			System.out.println("rh: " 
    					+       l.getFieldValue("BRAND-ID-RH").asString()
    					+ " " + l.getFieldValue("ORDER-NO-RH").asString()
    					+ " " + l.getFieldValue("RECEIPT-LOCN-RH").asString()
    					+ " " + l.getFieldValue("RECEIPT-NO-RH").asString()
   			);
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
