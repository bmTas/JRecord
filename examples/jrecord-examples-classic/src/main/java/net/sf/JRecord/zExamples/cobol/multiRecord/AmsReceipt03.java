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
import net.sf.JRecord.External.RecordEditorXmlLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cb2xml.def.Cb2xmlConstants;


/**
 * <b>Purpose:</b> This program demonstrates merging Record-Selection-Criteria stored in a Xml-Schema
 * with field definitions from a Cobol-Copybook.
 * <br>Also it is an example of processing Multiple record types in a file
 * 
 * <p>The key to doing this is to update the <b>ExternalRecord</b>. The <b>ExternalRecord</b> is essentially a 
 * <b>"Builder"</b> class for the <b>LayoutDetail</b> (record-schema) class. My background is not Java so I 
 * do not always no the correct OO terminology.
 * 
 * <p>This testing can also be done in Java code, See:<ul>
 * <li> AmsReceipt01 - processing with just Java code no Record-Selection-Criteria. This will use less 
 *                   resources than using the  Record-Selection-Criteria   
 * <li>  AmsReceipt02 - Adds Record-Selection-Details to the schema in the Java code
 * </ul>
 * 
 * 
 * In normal boolean expression, the operator goes between the operands:
 * <pre>
 *         (     f1 = v1
 *           and f2 = v2
 *           and f3 = v3)
 *     or        f4 = v4
 *     or  (     f5 = v5
 *            or f6 = v6)        
 *  
 *  In JRecord (both java and Xml), the boolean expression would be expressed as:
 * 
 *   or(
 *       and( f1 = v1,
 *            f2 = v2,
 *            f3 = v3),
 *            f4 = v4,
 *       and( f5 = v5,
 *            f6 = v6)        
 *
 *  Where the boolean operator is applied to all its operands.
 *  In the Xml File it would be written as
 * 
 *   <or>
 *       <and>
 *           <TSTFIELD NAME="f1" VALUE="v1"/>
 *           <TSTFIELD NAME="f2" VALUE="v2"/>
 *           <TSTFIELD NAME="f3" VALUE="v3"/>
 *       </and>
 *           <TSTFIELD NAME="f4" VALUE="v4"/>
 *       <and>
 *           <TSTFIELD NAME="f5" VALUE="v5"/>
 *           <TSTFIELD NAME="f6" VALUE="v6"/>    
 *       </and>
 *   </or>
 * 
 *   <b>Note:</b> there is also an <b>OPERATOR</b> tag that can be use (>, <, >= etc)
 * 
 * </pre>
 *                   
 * @author Bruce Martin
 *
 */
public class AmsReceipt03 {

	private int fhIdx, rhIdx;
	
	private LayoutDetail schema;
	
    public static void main(String[] args) throws Exception {
    	new AmsReceipt03();
    }
    
    
    private AmsReceipt03() throws Exception {
    	loadRecordDefinition("AmsReceipt.cbl");
    	
    	readFile();
    }
    
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private void loadRecordDefinition(String copybookFileName) throws Exception{
    	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	RecordEditorXmlLoader loaderXML = new RecordEditorXmlLoader();
    	TextLog log = new TextLog();
		ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    			copyName, CopybookLoader.SPLIT_REDEFINE, 0,
				/* Font name */"", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_FUJITSU, 0, log);
    	ExternalRecord extlayoutXML = loaderXML.loadCopyBook(
    			copyName + ".Xml", CopybookLoader.SPLIT_NONE, 0,
				/* Font name */"", Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_FUJITSU, 0, log);
    	
    	extlayoutCBL.setFileStructure(extlayoutXML.getFileStructure());
    	extlayoutCBL.setFontName(extlayoutXML.getFontName());
    	
    	for (int i = extlayoutCBL.getNumberOfRecords() - 1; i >= 0; i--) {
    		ExternalRecord record = extlayoutCBL.getRecord(i);
			int idx = findRecordIndex(extlayoutXML, record.getRecordName());
    		if (idx >= 0) {
    			record.setRecordSelection(extlayoutXML.getRecord(i).getRecordSelection());

    			record.setParentRecord(extlayoutXML.getParentRecord());	
    		}
    	}
    	  
    	
    	fhIdx = findRecordIndex(extlayoutCBL, "STDR-DETAIL-RECORD"); 
    	rhIdx = findRecordIndex(extlayoutCBL, "STDR-RH");
    	
    	schema = extlayoutCBL.asLayoutDetail();
    }
    
    
    private void readFile() throws IOException, RecordException {
    	AbstractLineReader r = LineIOProvider.getInstance().getLineReader(schema);
    	AbstractLine l;
    	
    	r.open("G:\\Users\\BruceTst01\\RecordEditor_HSQL\\SampleFiles\\Ams_Receipt_05AUG08190103.txt", schema);
    	
    	while ((l = r.read()) != null) {
    		if (l.getPreferredLayoutIdx() == fhIdx) {
    			System.out.println("fh: " 
    					+         l.getFieldValue("STDR-FH-CRD-DD").asString()
    					+ " / " + l.getFieldValue("STDR-FH-CRD-MM").asString()
    					+ " / " + l.getFieldValue("STDR-FH-CRD-CC").asString()
    					+         l.getFieldValue("STDR-FH-CRD-YY").asString()
   			);
    		} else if (l.getPreferredLayoutIdx() == rhIdx) {
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
