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
import net.sf.JRecord.CsvParser.BasicCsvByteLineParserExtended;
import net.sf.JRecord.CsvParser.BasicCsvLineParserExtended;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.ICsvByteLineParser;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.Types.Type;

public class TstBasicByteParserExtended extends TestCase {

	private static final CsvDefinition LINES0_CSV_DEF = new CsvDefinition("," , "");
	private static final CsvDefinition LINES1_CSV_DEF = new CsvDefinition("," , "'");
	private static final CsvDefinition LINES2_CSV_DEF = new CsvDefinition("|" , "'");
	private static final CsvDefinition LINES3_CSV_DEF = new CsvDefinition("," , "`~");

	private String[] lines0 = {"field1, field2, field3",
			 "f11,f21,f31",
			 "f12,f22,f32",
			 "f13,f23,f33",
			 "f14,f24,f34",
			 "f15,f25,f35",
			 "f16,f26,f36",
			 "f17,f26,f37",
	};

	private String[] lines1 = {"field1, field2, field3",
							 "f11,'pt1, pt2, pt3',f31",
							 "f12,'pt1, pt2, pt3''',f32",
							 "f13,'pt1, ''pt2'' , pt3',f33",
							 "f14,'pt1, ''pt2'' , ''pt3''',f34",
							 "f15,'pt1, ''pt2'', ''pt3'', pt4',f35",
							 "f16,'''f2a'','' f2b''',f36",
							 "'f17','pt1, ''pt2'' , ''pt3'', pt4','f37'",
	};
	private String[] lines2 = {"field1| field2| field3",
			 "f11|'pt1| pt2| pt3'|f31",
			 "f12|'pt1| pt2| pt3'''|f32",
			 "f13|'pt1| ''pt2'' | pt3'|f33",
			 "f14|'pt1| ''pt2'' | ''pt3'''|f34",
			 "f15|'pt1| ''pt2''| ''pt3''| pt4'|f35",
			 "f16|'''f2a''|'' f2b'''|f36",
			 "'f17'|'pt1| ''pt2'' | ''pt3''| pt4'|'f37'",
	};

	private String[] lines3 = {"field1, field2, field3",
			 "f11,`~pt1, pt2, pt3`~,f31",
			 "f12,`~pt1, pt2, pt3`~`~`~,f32",
			 "f13,`~pt1, `~`~pt2`~`~ , pt3`~,f33",
			 "f14,`~pt1, `~`~pt2`~`~ , `~`~pt3`~`~`~,f34",
			 "f15,`~pt1, `~`~pt2`~`~, `~`~pt3`~`~, pt4`~,f35",
			 "f16,`~`~`~f2a`~`~,`~`~ f2b`~`~`~,f36",
			 "`~f17`~,`~pt1, `~`~pt2`~`~ , `~`~pt3`~`~, pt4`~,`~f37`~",
	};

	public void testGetField1a() {
		ICsvByteLineParser p = new BasicCsvByteLineParserExtended(false);
		tstGetField1(p);
	}

	public void testGetField1b() {
		ICsvByteLineParser p = new BasicCsvLineParserExtended(false);
		tstGetField1(p);
	}

	private void tstGetField1(ICsvByteLineParser p) {
		String s;

		for (int i = 1; i < lines1.length; i++) {
			s = p.getField(0, lines1[i].getBytes(), new CsvDefinition("," , "'"));
			assertEquals("Error in " + i + "a, string was " + s, "f1" + i, s);
			s = p.getField(0, lines2[i].getBytes(), new CsvDefinition("|" , "'"));
			assertEquals("Error in " + i + "b, string was " + s, "f1" + i, s);
			s = p.getField(0, lines0[i].getBytes(), new CsvDefinition("," , ""));
			assertEquals("Error in " + i + "c, string was " + s, "f1" + i, s);

			s = p.getField(2, lines1[i].getBytes(), new CsvDefinition("," , "'"));
			assertEquals("Error in " + i + "d, string was " + s, "f3" + i, s);
			s = p.getField(2, lines2[i].getBytes(), new CsvDefinition("|" , "'"));
			assertEquals("Error in " + i + "e, string was " + s, "f3" + i, s);
			s = p.getField(2, lines0[i].getBytes(), new CsvDefinition("," , ""));
			assertEquals("Error in " + i + "f, string was " + s, "f3" + i, s);

		}
	}

	public void testGetField2a() {
		ICsvByteLineParser p = new BasicCsvByteLineParserExtended(false);
		tstGetField2(p);
	}

	public void testGetField2b() {
		ICsvByteLineParser p = new BasicCsvLineParserExtended(false);
		tstGetField2(p);
	}

	private void tstGetField2(ICsvByteLineParser p) {
		String s;
 
		s = p.getField(1, lines1[1].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 11a, string was " + s, "pt1, pt2, pt3", s);
		s = p.getField(1, lines2[1].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 11b, string was " + s, "pt1| pt2| pt3", s);


		s = p.getField(1, lines1[2].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 12a, string was " + s, "pt1, pt2, pt3'", s);
		s = p.getField(1, lines2[2].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 12b, string was " + s, "pt1| pt2| pt3'", s);

		s = p.getField(1, lines1[3].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 13a, string was " + s, "pt1, 'pt2' , pt3", s);
		s = p.getField(1, lines2[3].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 13b, string was " + s, "pt1| 'pt2' | pt3", s);

		s = p.getField(1, lines1[4].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 14a, string was " + s, "pt1, 'pt2' , 'pt3'", s);
		s = p.getField(1, lines2[4].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 14b, string was " + s, "pt1| 'pt2' | 'pt3'", s);

		s = p.getField(1, lines1[5].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 15a, string was " + s, "pt1, 'pt2', 'pt3', pt4", s);
		s = p.getField(1, lines2[5].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 15b, string was " + s, "pt1| 'pt2'| 'pt3'| pt4", s);

		s = p.getField(1, lines1[6].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 16a, string was " + s, "'f2a',' f2b'", s);
		s = p.getField(1, lines2[6].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 16b, string was " + s, "'f2a'|' f2b'", s);

		s = p.getField(1, lines1[7].getBytes(), new CsvDefinition("," , "'"));
		assertEquals("Error in 17a, string was " + s, "pt1, 'pt2' , 'pt3', pt4", s);
		s = p.getField(1, lines2[7].getBytes(), new CsvDefinition("|" , "'"));
		assertEquals("Error in 17b, string was " + s, "pt1| 'pt2' | 'pt3'| pt4", s);
	}

	public void testSetFieldA() {
		ICsvByteLineParser p = new BasicCsvByteLineParserExtended(false);
		tstSetField(p);
	}

	public void testSetFieldB() {
		ICsvByteLineParser p = new BasicCsvLineParserExtended(false);
		tstSetField(p);
	}

	private void tstSetField(ICsvByteLineParser p) {
		int j;
		byte[] s;
		//StringBuffer comment = new StringBuffer("12345");

		//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

		for (int i = 0; i < lines1.length - 1; i++) {
			for (j = 0; j < 3; j++) {
				s = p.setFieldByteLine(j, Type.NT_TEXT, lines1[i].getBytes(), new CsvDefinition(",", "'"),
						p.getField(j, lines1[i].getBytes(), new CsvDefinition(",", "'")));
				assertEquals("Error in " + i +":" + j + "a got " + s, lines1[i], new String(s));

				s = p.setFieldByteLine(j, Type.NT_TEXT, lines2[i].getBytes(), new CsvDefinition("|", "'"),
						p.getField(j, lines2[i].getBytes(), new CsvDefinition("|", "'")));
				assertEquals("Error in " + i +":" + j + "b got " + s, lines2[i], new String(s));

				s = p.setFieldByteLine(j, Type.NT_TEXT, lines0[i].getBytes(), new CsvDefinition(",", "'"),
						p.getField(j, lines0[i].getBytes(), new CsvDefinition(",", "")));
				assertEquals("Error in " + i +":" + j + "c got " + s, lines0[i], new String(s));
}
		}
	}


	public void testSetField2a() {
		ICsvByteLineParser p = new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true);
		tstSetField2(p);
	}
	

	public void testSetField2b() {
		ICsvByteLineParser p = new BasicCsvLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true);
		tstSetField2(p);
	}



	private void tstSetField2(ICsvByteLineParser p) {
		int j;
		byte[] s;
		//StringBuffer comment = new StringBuffer("12345");

		//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

		for (int i = 0; i < lines1.length - 1; i++) {
			for (j = 0; j < 3; j++) {
				s = p.setFieldByteLine(j, Type.NT_NUMBER, 
						lines1[i].getBytes(), new CsvDefinition(",", "'"),
						p.getField(j, lines1[i].getBytes(), new CsvDefinition(",", "'")));
				assertEquals("Error in " + i +":" + j + "a got " + new String(s), lines1[i], new String(s));

				s = p.setFieldByteLine(j, Type.NT_NUMBER, lines2[i].getBytes(), new CsvDefinition("|", "'"),
						p.getField(j, lines2[i].getBytes(), new CsvDefinition("|", "'")));
				assertEquals("Error in " + i +":" + j + "b got " + new String(s), lines2[i], new String(s));

				s = p.setFieldByteLine(j, Type.NT_NUMBER, lines0[i].getBytes(), new CsvDefinition(",", "'"),
						p.getField(j, lines0[i].getBytes(), new CsvDefinition(",", "")));
				assertEquals("Error in " + i +":" + j + "c got " + new String(s), lines0[i], new String(s));
			}
		}
	}


	public void testSetField3a() {
			BasicCsvByteLineParserExtended p = new BasicCsvByteLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true);
			tstSetField3(p);
	}

	public void testSetField3b() {
			BasicCsvLineParserExtended p = new BasicCsvLineParserExtended(false, ICsvDefinition.NORMAL_SPLIT, false, true);
			tstSetField3(p);
	}

	private void tstSetField3(ICsvByteLineParser p) {
		int j;
		String before, after;
		CsvDefinition csvDefCommaSingleQuote = new CsvDefinition(",", "'");
		CsvDefinition csvDefBarSingleQuote = new CsvDefinition("|", "'");
		byte[] s;
		//StringBuffer comment = new StringBuffer("12345");

		//System.out.println("@@~~ " + comment.substring(comment.length() - 1));

		for (int i = 0; i < lines1.length - 1; i++) {
			for (j = 0; j < 3; j++) {
				before = p.getField(j, lines1[i].getBytes(), csvDefCommaSingleQuote);
				s = p.setFieldByteLine(j, Type.NT_TEXT, lines1[i].getBytes(), csvDefCommaSingleQuote, before);
				System.out.println(lines1[i]);
				System.out.println(new String(s));

				if (before.indexOf(",") == 0) {
					after = p.getField(j, s, new CsvDefinition(",", ""));
					System.out.println(i + ", " + j + " > " + before + " => " + after + " <");

					assertEquals("Error in " + i +":" + j + "aa got " + after, after, "'" + before + "'");
				}
				after = p.getField(j, s, csvDefCommaSingleQuote);
				assertEquals("Error in " + i +":" + j + "ab got " + after, before, after);

				before = p.getField(j, lines2[i].getBytes(), csvDefBarSingleQuote);
				s = p.setFieldByteLine(j, Type.NT_TEXT, lines2[i].getBytes(), csvDefBarSingleQuote, before);

				if (before.indexOf("|") == 0) {
					after = p.getField(j, s, new CsvDefinition("|", ""));
					System.out.println(i + ", " + j + " > " + before + " => " + after);

					assertEquals("Error in " + i +":" + j + "ba got " + after, after, "'" + before + "'");
				}
				after = p.getField(j, s, csvDefBarSingleQuote);
				System.out.println(i + ", " + j + " > " + before + " => " + after);
				assertEquals("Error in " + i +":" + j + "bb got " + after, before, after);

				before = p.getField(j, lines0[i].getBytes(), new CsvDefinition(",", ""));
				s = p.setFieldByteLine(j, Type.NT_TEXT, lines0[i].getBytes(), csvDefCommaSingleQuote, before);
				after = p.getField(j, s,  new CsvDefinition(",", ""));
				System.out.println(i + ", " + j + " > " + before + " => " + after);

				assertEquals("Error in " + i +":" + j + " ca got " + after, "'" + before + "'", after);
				after = p.getField(j, s, csvDefCommaSingleQuote);
				assertEquals("Error in " + i +":" + j + "cb got " + after, before, after);
				System.out.println();
			}
		}
	} 

	
	public void testGetFieldList() {
		BasicCsvByteLineParserExtended p = new BasicCsvByteLineParserExtended(false);
		CommonCsvTests.tstGetFieldListByte("0: ", lines0, p, LINES0_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldListByte("1: ", lines1, p, LINES1_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldListByte("2: ", lines2, p, LINES2_CSV_DEF, 3);
		CommonCsvTests.tstGetFieldListByte("3: ", lines3, p, LINES3_CSV_DEF, 3);
	}

	
	public void testSetFieldList() {
		BasicCsvByteLineParserExtended p = new BasicCsvByteLineParserExtended(false);
		CommonCsvTests.tstSetFieldListByte("0: ", lines0, p, LINES0_CSV_DEF, 3);
		CommonCsvTests.tstSetFieldListByte("1: ", lines1, lines1.length - 1, p, LINES1_CSV_DEF, 3);
		CommonCsvTests.tstSetFieldListByte("2: ", lines2, lines2.length - 1, p, LINES2_CSV_DEF, 3);
		CommonCsvTests.tstSetFieldListByte("3: ", lines3, lines3.length - 1, p, LINES3_CSV_DEF, 3);
	}

//	public void testSplit() {
//		fail("Not yet implemented");
//	}

}
