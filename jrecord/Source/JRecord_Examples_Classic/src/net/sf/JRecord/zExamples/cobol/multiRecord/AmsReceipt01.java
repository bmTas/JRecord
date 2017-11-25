/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */
      
package net.sf.JRecord.zExamples.cobol.multiRecord;

import java.io.IOException;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cb2xml.def.Cb2xmlConstants;


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
	
	private LayoutDetail schema;
	
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
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    			copyName, CopybookLoader.SPLIT_REDEFINE, 0,
				/* Font name */"", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_FUJITSU, 0, new TextLog());
    	  
    	/*
    	   If you want to write this out as a Xml-Schema:
    	 
	    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
	    	RecordEditorXmlWriter writer
	    			= (RecordEditorXmlWriter) writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);
	
	    	writer.writeCopyBook(new FileOutputStream(TstConstants.TEMP_DIRECTORY + "XML_CompanyRecord.Xml"), extlayoutCBL, null);
    	*/
    	
    	schema = extlayoutCBL.asLayoutDetail();
    }
    
    
    private void readFile() throws IOException, RecordException {
    	AbstractLineReader r = LineIOProvider.getInstance().getLineReader(schema);
    	AbstractLine l;
    	String id;
    	
    	r.open("G:\\Users\\BruceTst01\\RecordEditor_HSQL\\SampleFiles\\Ams_Receipt_05AUG08190103.txt", schema);
    	
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
