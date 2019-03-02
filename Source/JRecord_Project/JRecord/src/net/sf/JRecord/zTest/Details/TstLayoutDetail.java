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

package net.sf.JRecord.zTest.Details;

import java.io.ByteArrayInputStream;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cb2xml.def.Cb2xmlConstants;
import junit.framework.TestCase;

public class TstLayoutDetail extends TestCase {
	
	public static String[] cobolCopybook = {
		"        01  Header-Record.\n",
		"            05  Record-Type              pic x.\n",
		"                88 Header-Record  value 'H'.\n",
		"                88 Detail-Record  value 'D'.\n",
		"                88 Detail-Record  value 'T'.\n",
		"            05  Filler                   pic x.\n",
		"            05  Run-Number               pic 9(5).\n",
		"            05  Filler                   pic x.\n",
		"            05  Run-Date                 pic 9(8).\n",
		"            05  Filler                   pic x(34).\n",
		
		"        01  Detail-Record.\n",
		"            05  Record-Type              pic x.\n",
		"            05  field1                   pic x(3).\n",
		"            05  field2                   pic x(12).\n",
		"            05  field3                   pic x(10).\n",
		"            05  Filler                   pic x(14).\n",

		"        01  Trailer-Record.\n",
		"            05  Record-Type              pic x.\n",
		"            05  record-count             pic 9(8).\n",
		"            05  Filler                   pic x(31).\n",	};

	public void testGetRecordName() throws Exception {
		ExternalRecord rec = getExternalLayout();
		LayoutDetail l = rec.asLayoutDetail();
		
		tst(rec, "Header-Record");
		tst(rec, "Detail-Record");
		tst(rec, "Trailer-Record");
		
		tst(l, "Header-Record");
		tst(l, "Detail-Record");
		tst(l, "Trailer-Record");
	}
	
	private void tst(ExternalRecord rec, String recordName) {
		assertEquals(recordName, rec.getRecord(recordName).getRecordName());
	}
	
	private void tst(LayoutDetail rec, String recordName) {
		assertEquals(recordName, rec.getRecord(recordName).getRecordName());
	}

	private ExternalRecord getExternalLayout() throws Exception {
		ByteArrayInputStream bs = array2stream(cobolCopybook);
		
		return  (new CobolCopybookLoader())
					.loadCopyBook(
							bs, "RedefTest", CopybookLoader.SPLIT_01_LEVEL, 0, "",
							Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_INTEL, 0, new TextLog());
	}
	
	
	private ByteArrayInputStream array2stream(String[] array) {
		StringBuilder b = new StringBuilder();
		
		for (int i = 0; i < array.length; i++) {
			b.append(array[i]);
		}
		
		System.out.println(b.toString());
		return new ByteArrayInputStream(b.toString().getBytes());
	}

}
