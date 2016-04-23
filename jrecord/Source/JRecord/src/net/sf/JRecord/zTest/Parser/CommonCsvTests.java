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

package net.sf.JRecord.zTest.Parser;

import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.CsvParser.ICsvLineParser;

public class CommonCsvTests {

	public static void tstGetFieldList(String id, String[] lines, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		System.out.println(id);
		for (String s : lines) {
			List<String> flds = parser.getFieldList(s, csvDefinition);
			if (flds.size() != fieldCount) {
				flds = parser.getFieldList(s, csvDefinition);
				TestCase.assertEquals(id + " check counts", fieldCount, flds.size());
			}
			for (int i = 0; i < flds.size(); i++) {
				TestCase.assertEquals(id + " Check: " + s + ", fieldNo=" + i, 
						parser.getField(i, s, csvDefinition), flds.get(i) );
			}
		}
	}
	

	public static void tstSetFieldList(String id, String[] lines, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		tstSetFieldList(id, lines, lines.length, parser, csvDefinition, fieldCount);
	}
	

	public static void tstSetFieldList(String id, String[] lines, int num, ICsvLineParser parser, ICsvDefinition csvDefinition, int fieldCount) {
		System.out.println(id);
		for (int i = 0; i < num; i++) {
			String s = lines[i];
			List<String> flds = parser.getFieldList(s, csvDefinition);
			TestCase.assertEquals(id + " check counts", fieldCount, flds.size());
			TestCase.assertEquals(id + " check line: ", s, parser.formatFieldList(flds, csvDefinition, null));
		}
	}

}
