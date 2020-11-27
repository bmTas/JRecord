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
      
package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.CopybookWriter;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Purpose: This program demonstrates converting a Cobol Copybook (with duplicate field names) to a
 *     Xml copybook with Duplicate fields Renamed.
 *
 * @author Bruce Martin
 *
 */
public class ConvertCbl2RecordEditorXml02 {

	
	private static String cobolCopybook
	= "      01 COMPANY-RECORD.\n"
	+ "         05 COMPANY-NAME     PIC X(30).\n"
	+ "         05 EMPLOYEE-LIST.\n"
	+ "            10 PRESIDENT.\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "            10 VICE-PRESIDENT.\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "            10 OTHERS.\n"
	+ "               15 TITLE      PIC X(10).\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "         05 PARTNER-LIST.\n"
	+ "            10 PRESIDENT.\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "            10 VICE-PRESIDENT.\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "            10 OTHERS.\n"
	+ "               15 TITLE      PIC X(10).\n"
	+ "               15 LAST-NAME  PIC X(15).\n"
	+ "               15 FIRST-NAME PIC X(8).\n"
	+ "         05 ADDRESS          PIC X(15).\n"
	+ "         05 CITY             PIC X(15).\n"
	+ "         05 STATE            PIC XX.\n"
	+ "         05 ZIP              PIC 9(5).\n"
	+ "         05 PROFIT           PIC s9(9)V99.\n";
	
	private static byte[] copyBookBytes = cobolCopybook.getBytes();
	
	
	/**
	 * This Table holds the translation Table, each 
	 * entry consists of:<ul>
	 *     <li>A list of <i>From-Group-Names</i> and the <i>Field-name</i>. Group and Field names must
	 *     be in the <b>correct</b> sequence.
	 *     <li>A list with one entry <b>To Field name</b>
	 * </ul> 
	 */
	private static String[][][] NAME_TRANSLATIONS = {
		{{"EMPLOYEE-LIST", "PRESIDENT",  "LAST-NAME"}, {"President-last-name"}},
		{{"EMPLOYEE-LIST", "PRESIDENT",  "FIRST-NAME"}, {"President-first-name"}},
		{{"EMPLOYEE-LIST", "VICE-PRESIDENT",  "LAST-NAME"}, {"Vice-President-last-name"}},
		{{"EMPLOYEE-LIST", "VICE-PRESIDENT",  "FIRST-NAME"}, {"Vice-President-first-name"}},
		{{"PARTNER-LIST", "PRESIDENT",  "LAST-NAME"}, {"Presidents-Partner-last-name"}},
		{{"PARTNER-LIST", "PRESIDENT",  "FIRST-NAME"}, {"Presidents-Partner-first-name"}},
	};

    public static void main(String[] args) throws Exception {
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    	    new ByteArrayInputStream(copyBookBytes),
    	    Conversion.getCopyBookId("COMPANY-RECORD.cbl"),
    	    CopybookLoader.SPLIT_NONE, 0, "", ICopybookDialects.FMT_FUJITSU, 0, new TextLog());
    	
    	transalateFieldNamesInRecord(extlayoutCBL);
    	
    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
    	CopybookWriter writer
    			= writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);

    	writer.writeCopyBook(new FileOutputStream(TstConstants.TEMP_DIRECTORY + "XML_CompanyRecord.Xml"), extlayoutCBL, null);
    }
    
    
    /**
     * This method converts Duplicate fields in the record (in NAME_TRANSLATIONS table)
     * with there new name.
     * @param rec
     */
    private static void transalateFieldNamesInRecord(ExternalRecord rec) {
    	if (rec.getNumberOfRecords() == 0) {
    		transalateFieldNamesInOneRecord(rec);
    	} else {
    		for (int i = 0; i < rec.getNumberOfRecords(); i++) {
    			transalateFieldNamesInRecord(rec.getRecord(i));
    		}
    	}
    }
    
    
    /**
     * This method converts Duplicate fields (in NAME_TRANSLATIONS table)
     * with there new name.
     * to there new Names.
     * @param rec record to update
     */
    private static void transalateFieldNamesInOneRecord(ExternalRecord rec) {
    	ExternalField recordField;
    	String group;
    	for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
    		recordField = rec.getRecordField(i);
    		group = recordField.getGroup();
    		for (String[][] translateItem : NAME_TRANSLATIONS) {
    			int pos = 0;
    			boolean match = true;
    			String[] FromFieldNames = translateItem[0];
    			String toFieldName = translateItem[1][0];
    			
    			/**
    			 * It may be better to match using cobol name i.e.
    			 * 
    			 * if (FromFieldNames[FromFieldNames.length - 1].equalsIgnoreCase(recordField.getCobolName())) {
    			 */
				if (FromFieldNames[FromFieldNames.length - 1].equalsIgnoreCase(recordField.getName())) {
    				for (int j = 0; match && j < FromFieldNames.length - 1; j++) {
    					pos = group.indexOf(FromFieldNames[j], pos);
    					match = pos >= 0;
    				}
    				
    				if (match) {
    					recordField.setName(toFieldName);
    				}
    			}
    		}
    	}
    }
}
