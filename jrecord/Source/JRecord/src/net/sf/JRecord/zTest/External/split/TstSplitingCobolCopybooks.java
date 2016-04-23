/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zTest.External.split;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import junit.framework.TestCase;

public class TstSplitingCobolCopybooks extends TestCase {

	private RecordDtls[] ExpectedRecords = {
			new RecordDtls("HEADER-RECORD", 
					  new FieldDtls("HDR-RECORD-TYPE", 1, 1, 0), new FieldDtls("HDR-CREATION-YYYYMMDD", 2, 8, 25), new FieldDtls("HDR-CREATION-HHMISS", 10, 6, 25)
					, new FieldDtls("HDR-PRD-START-YYYYMMDD", 16, 8, 25), new FieldDtls("HDR-PRD-START-HHMISS", 24, 6, 25), new FieldDtls("HDR-PRD-END-YYYYMMDD", 30, 8, 25)
					, new FieldDtls("HDR-PRD-END-HHMISS", 38, 6, 25), new FieldDtls("HDR-ASSET-NUMBER", 44, 9, 0), new FieldDtls("HDR-FILE-SEQUENCE", 53, 6, 0)
					, new FieldDtls("HDR-FILE-NAME", 59, 40, 0), new FieldDtls("HDR-SOURCE-CODE", 99, 4, 0), new FieldDtls("HDR-DESTINATION-CODE", 103, 4, 0)
			), 
			new RecordDtls("SUB-HEADER-RECORD", 
					  new FieldDtls("SHDR-RECORD-CODE", 1, 1, 0), new FieldDtls("SHDR-RECORD-INDEX", 2, 1, 0), new FieldDtls("SHDR-DATA-TEXT", 3, 104, 0)
			), 
			new RecordDtls("DETAIL-DATA-RECORD", 
					  new FieldDtls("DDR-RECORD-TYPE", 1, 1, 0), new FieldDtls("DDR-DATA-TEXT", 2, 105, 0)
			), 
			new RecordDtls("TRAILER-RECORD", 
					  new FieldDtls("TLR-RECORD-TYPE", 1, 1, 0), new FieldDtls("TLR-LIFE-CYCLE", 2, 1, 0), new FieldDtls("TLR-RECORD-COUNT", 3, 9, 25)
					, new FieldDtls("TLR-FILE-NUMBER", 12, 4, 25), new FieldDtls("TLR-FILE-COUNT", 16, 4, 25), new FieldDtls("TLR-CONTROL-TOTAL-1", 20, 15, 0)
					, new FieldDtls("TLR-CONTROL-TOTAL-2", 35, 15, 0), new FieldDtls("TLR-CONTROL-TOTAL-3", 50, 15, 0), new FieldDtls("TLR-CONTROL-TOTAL-4", 65, 15, 0)
					, new FieldDtls("TLR-CONTROL-TOTAL-5", 80, 15, 0), new FieldDtls("FILLER", 95, 12, 0)
			)
	};
	
	
	public void testRepeating()  throws RecordException {
		tstImport("example.cbl", CopybookLoader.SPLIT_HIGHEST_REPEATING);
		tstImport("example1.cbl", CopybookLoader.SPLIT_HIGHEST_REPEATING);
		tstImport("example2.cbl", CopybookLoader.SPLIT_HIGHEST_REPEATING);
		tstImport("example3.cbl", CopybookLoader.SPLIT_HIGHEST_REPEATING);
	}
	
	public void test01()  throws RecordException {
		tstImport("example3.cbl", CopybookLoader.SPLIT_01_LEVEL);
	}
	
	public void testRedef()  throws RecordException {
		tstImport("updExample.cbl", CopybookLoader.SPLIT_REDEFINE);
	}
	
	private void tstImport(String copybook, int splitOption) throws RecordException {
	   	String copyName = this.getClass().getResource(copybook).getFile();

    	CobolCopybookLoader loaderXML = new CobolCopybookLoader();

    	ExternalRecord extlayoutCBL = loaderXML.loadCopyBook(
    			copyName , splitOption, 0,
				/* Font name */"", ICopybookDialects.FMT_MAINFRAME, 0, new TextLog()); 
    	
    	LayoutDetail schema = extlayoutCBL.asLayoutDetail();
    	
    	assertEquals(ExpectedRecords.length, schema.getRecordCount());
    	
    	for (int i = 0; i < schema.getRecordCount(); i++) {
    		RecordDetail record = schema.getRecord(i);

    		assertTrue( record.getRecordName().indexOf(ExpectedRecords[i].recordName) >= 0);

			for (int j = 0; j < record.getFieldCount(); j++) {
				FieldDetail field = record.getField(j);
				FieldDtls expectedField = ExpectedRecords[i].fields[j];
				String id = record.getRecordName() + " " + i + ", " + j;

				assertEquals(id, expectedField.name, field.getName());			
				assertEquals(id, expectedField.pos,  field.getPos());			
				assertEquals(id, expectedField.len,  field.getLen());			
				assertEquals(id, expectedField.type, field.getType());			
			}
    	}
	}
	
	private class RecordDtls {
		final String recordName;
		final FieldDtls[] fields;
		
		public RecordDtls(String recordName, FieldDtls... fields) {
			super();
			this.recordName = recordName;
			this.fields = fields;
		}
		
		
	}
	
	private class FieldDtls {
		final String name;
		final int pos, len, type;
		
		public FieldDtls(String name, int pos, int len, int type) {
			super();
			this.name = name;
			this.pos = pos;
			this.len = len;
			this.type = type;
		}
		
		
	}
}
