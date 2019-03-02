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

import junit.framework.TestCase;
import net.sf.JRecord.CsvParser.BasicCsvLineParser;
import net.sf.JRecord.CsvParser.CsvDefinition;

public class TstBasicParser extends TestCase {

	private static final CsvDefinition CSV_DEF_COMMA = new CsvDefinition(",", "");

	private static final CsvDefinition CSV_DEF_BAR_SINGLE_QUOTE = new CsvDefinition("|", "'");

	private static final CsvDefinition CSV_DEF_COMMA_SINGLE_QUOTE = new CsvDefinition(",", "'");

	private static final CsvDefinition LINES0_CSV_DEF = CSV_DEF_COMMA;

	private static final CsvDefinition LINES1_CSV_DEF = CSV_DEF_COMMA_SINGLE_QUOTE;

	private static final CsvDefinition LINES2_CSV_DEF = CSV_DEF_BAR_SINGLE_QUOTE;

	private static final CsvDefinition LINES3_CSV_DEF = new CsvDefinition(",", "`~");

	private String[] lines0 = {"field1, field2, field3",
			 "f11,f21,f31",
			 "f12,f22,f32",
			 "f13,f23,f33",
			 "f14,f24,f34",
			 "f15,f25,f35",
			 "f16,f26,f36",
	};

	private String[] lines1 = {"field1, field2, field3",
							 "f11,'pt1, pt2, pt3',f31",
							 "f12,'pt1, pt2, pt3'',f32",
							 "f13,'pt1, 'pt2' , pt3',f33",
							 "f14,''pt1, pt2, pt3'',f34",
							 "f15,''pt1, pt2, pt3',f35",
							 "f16,''pt1, 'pt2' , pt3',f36",
	};
	private String[] lines2 = {"field1| field2| field3",
			 "f11|'pt1| pt2| pt3'|f31",
			 "f12|'pt1| pt2| pt3''|f32",
			 "f13|'pt1| 'pt2' | pt3'|f33",
			 "f14|''pt1| pt2| pt3''|f34",
			 "f15|''pt1| pt2| pt3'|f35",
			 "f16|''pt1| 'pt2' | pt3'|f36",
	};

	private String[] lines3 = {"field1, field2, field3",
			 "f11,`~pt1, pt2, pt3`~,f31",
			 "f12,`~pt1, pt2, pt3`~`~,f32",
			 "f13,`~pt1, `~pt2`~ , pt3`~,f33",
			 "f14,`~`~pt1, pt2, pt3`~`~,f34",
			 "f15,`~`~pt1, pt2, pt3`~,f35",
			 "f16,`~`~pt1, `~pt2`~ , pt3`~`~,f36",
	};

	public void testGetFieldCount() {
		BasicCsvLineParser p = BasicCsvLineParser.getInstance();
		int c;

		for (int i = 0; i < lines1.length; i++) {
			c = p.getFieldCount(lines1[i], CSV_DEF_COMMA_SINGLE_QUOTE);
			assertEquals("Error in " + i + "a, count was " + c, 3, c);
			c = p.getFieldCount(lines2[i], CSV_DEF_BAR_SINGLE_QUOTE);
			assertEquals("Error in " + i + "b, count was " + c, 3, c);
			c = p.getFieldCount(lines0[i], CSV_DEF_COMMA);
			assertEquals("Error in " + i + "c, count was " + c, 3, c);
		}
	}

	public void testGetField() {
		BasicCsvLineParser p = BasicCsvLineParser.getInstance();
		String s;

		for (int i = 1; i < lines1.length; i++) {
			s = p.getField(0, lines1[i], CSV_DEF_COMMA_SINGLE_QUOTE);
			assertEquals("Error in " + i + "a, string was " + s, "f1" + i, s);
			s = p.getField(0, lines2[i], CSV_DEF_BAR_SINGLE_QUOTE);
			assertEquals("Error in " + i + "b, string was " + s, "f1" + i, s);
			s = p.getField(0, lines0[i], CSV_DEF_COMMA);
			assertEquals("Error in " + i + "c, string was " + s, "f1" + i, s);

			s = p.getField(2, lines1[i], CSV_DEF_COMMA_SINGLE_QUOTE);
			assertEquals("Error in " + i + "d, string was " + s, "f3" + i, s);
			s = p.getField(2, lines2[i], CSV_DEF_BAR_SINGLE_QUOTE);
			assertEquals("Error in " + i + "e, string was " + s, "f3" + i, s);
			s = p.getField(2, lines0[i], CSV_DEF_COMMA);
			assertEquals("Error in " + i + "f, string was " + s, "f3" + i, s);

		}

		s = p.getField(1, lines1[1], CSV_DEF_COMMA_SINGLE_QUOTE);
		assertEquals("Error in 11a, string was " + s, "pt1, pt2, pt3", s);
		s = p.getField(1, lines2[1], CSV_DEF_BAR_SINGLE_QUOTE);
		assertEquals("Error in 11a, string was " + s, "pt1| pt2| pt3", s);


		s = p.getField(1, lines1[2], CSV_DEF_COMMA_SINGLE_QUOTE);
		assertEquals("Error in 12a, string was " + s, "pt1, pt2, pt3'", s);
		s = p.getField(1, lines2[2], CSV_DEF_BAR_SINGLE_QUOTE);
		assertEquals("Error in 12a, string was " + s, "pt1| pt2| pt3'", s);

		s = p.getField(1, lines1[3], CSV_DEF_COMMA_SINGLE_QUOTE);
		assertEquals("Error in 13a, string was " + s, "pt1, 'pt2' , pt3", s);
		s = p.getField(1, lines2[3], CSV_DEF_BAR_SINGLE_QUOTE);
		assertEquals("Error in 13a, string was " + s, "pt1| 'pt2' | pt3", s);
	}

	public void testSetField() {
		BasicCsvLineParser p = BasicCsvLineParser.getInstance();
		int j;
		String s;

		for (int i = 0; i < lines1.length; i++) {
			for (j = 0; j < 3; j++) {
				s = p.setField(j, 0, lines1[i], CSV_DEF_COMMA_SINGLE_QUOTE,
						p.getField(j, lines1[i], CSV_DEF_COMMA_SINGLE_QUOTE));
				System.out.println(i + "," + j + ">" + s);
				System.out.println("   >" + lines1[i]);
				assertEquals("Error in " + i +":" + j + "a got " + s, lines1[i], s);

				s = p.setField(j,  0, lines2[i], CSV_DEF_BAR_SINGLE_QUOTE,
						p.getField(j, lines2[i], CSV_DEF_BAR_SINGLE_QUOTE));
				assertEquals("Error in " + i +":" + j + "b got " + s, lines2[i], s);

				s = p.setField(j,  0, lines0[i], CSV_DEF_COMMA_SINGLE_QUOTE,
						p.getField(j, lines0[i], CSV_DEF_COMMA));
				assertEquals("Error in " + i +":" + j + "c got " + s, lines0[i], s);
}
		}
	}
	
	public void testGetFieldList() {
		BasicCsvLineParser p = BasicCsvLineParser.getInstance();
		CommonCsvTests.tstGetFieldList("0: ", lines0, p, LINES0_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("1: ", lines1, p, LINES1_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("2: ", lines2, p, LINES2_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("3: ", lines3, p, LINES3_CSV_DEF, 3);
	}
	
	public void testSetFieldList() {
		BasicCsvLineParser p = BasicCsvLineParser.getInstance();
		CommonCsvTests.tstGetFieldList("0: ", lines0, p, LINES0_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("1: ", lines1, p, LINES1_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("2: ", lines2, p, LINES2_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldList("3: ", lines3, p, LINES3_CSV_DEF, 3);
	}

	


//	public void testSplit() {
//		fail("Not yet implemented");
//	}

}
