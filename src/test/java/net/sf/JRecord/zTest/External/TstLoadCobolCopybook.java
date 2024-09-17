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

package net.sf.JRecord.zTest.External;

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.JUnit3Test;

public class TstLoadCobolCopybook extends JUnit3Test  {

	private String copybookFileName = "CopybookWithHex88.cbl";
	
	private String[] fldNames = {
			"CB-SORT-CODE",
			"STATISTICAL-IND",
			"HDR-PORTION-LENGTH",
			"POL-BODY-LENGTH",
			"XBOLG-HEX",
			"OVERFLOW-LENGTH",
			"1st-byte",
			"2nd-byte",
			"ARRANGEMENT-TYPE",
			"1-field",
			"ARRANGEMENT-NUMB",
			"PAY-POINT-NUMB",
			"XRCCT",
			"RECORD-COUNT",
			"CSO",
			"STATUS-CD",	
	};
	
	private int[][] fldAttrs = {
			{0, 1, 1},
			{0, 2, 1},
			{0, 3, 2},
			{0, 5, 2},
			{36, 5, 2},
			{0, 7, 2},
			{25, 7, 1},
			{25, 8, 1},
			{0, 9, 1},
			{25, 9, 1},
			{33, 10, 3},
			{33, 13, 2},
			{33, 15, 1},
			{0, 16, 1},
			{0, 17, 1},
			{0, 18, 1},
	};
	
	@Test public void testLoadCopybook() throws RecordException, IOException {
		
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    			copyName, CopybookLoader.SPLIT_NONE, 0,
				/* Font name */"", ICopybookDialects.FMT_MAINFRAME, 0, new TextLog());
    	
    	
    	assertEquals(fldNames.length, extlayoutCBL.getNumberOfRecordFields());
    	for (int i = 0; i < extlayoutCBL.getNumberOfRecordFields(); i++) {
    		ExternalField fld = extlayoutCBL.getRecordField(i);
			String id = "FldNo=" + i + ", name=" + fldNames[i];
			assertEqualsV3o(id, fldNames[i], fld.getName());
			assertEqualsV3i(id, fldAttrs[i][0], fld.getType());
			assertEqualsV3i(id, fldAttrs[i][1], fld.getPos());
			assertEqualsV3i(id, fldAttrs[i][2], fld.getLen());
    	}

        Assertions.assertTrue(CobolCopybookLoader.isAvailable());

	}
}
