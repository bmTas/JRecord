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

import junit.framework.TestCase;
import net.sf.JRecord.External.DbCsvCopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;

public class TstDbCsvExtractCopybook extends TestCase {

	private final String directory = "/home/bm/Work/RecordEditor/CsvCopybooks/";
	private String copyBookFile = directory + "E_POL.Csv";
	public void testLoadCopyBook() throws Exception {
		DbCsvCopybookLoader cnv = new DbCsvCopybookLoader();
		ExternalRecord rec;
		ExternalField fld;
		

		rec = cnv.loadCopyBook(copyBookFile, 0, 0, "", 0, 0, null);
		
		System.out.println("Number of fields " + rec.getNumberOfRecordFields());
		
		for (int i = 0; i < rec.getNumberOfRecordFields(); i++) {
			fld = rec.getRecordField(i);
			System.out.println("  " + fld.getName() + " "
					+ fld.getPos() + " " + fld.getLen() + " > "
					+ fld.getType());
		}

	}

}
