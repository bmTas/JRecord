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

package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


public class TstMultiCopybookCommon {

	private static final int DETAIL_COUNT = 15;
	public static final String TRAILER_ID = "T";
	public static final String DETAIL_ID = "D";
	public static final String REC_A_ID  = "A";
	public static final String REC_B_ID  = "B";
	public static final String HEADER_ID = "H";
	public static final String RECORD_TYPE = "Record-Type";
	
	
	private static final String[] RECORD_NAMES = { "RecordA", "Header-Record", "Detail-Record", "Trailer-Record", "RecordB",};
	private static final String[] RECORD_NAMES2 = { "Detail-Record-A", "Header-Record", "Detail-Record", "Trailer-Record", "Detail-Record-B",};

	private final static FieldDetail[][] EXPECTED_FIELDS = {
		{
			bldType("Record-Type", 1, 1, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-1a", 2, 11, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-2a", 13, 12, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-3a", 25, 11, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-4a", 36, 44, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
		}, {
			bldType("Record-Type", 1, 1, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Creation-Date", 2, 8, 0, Type.ftNumZeroPaddedPositive, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Version", 10, 5, 2, Type.ftAssumedDecimalPositive, Conversion.DEFAULT_ASCII_CHARSET),
		}, {
			bldType("Record-Type", 1, 1, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-1", 2, 10, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-2", 12, 20, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-3", 32, 10, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
		}, {
			bldType("Record-Type", 1, 1, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Record-Count", 2, 9, 0, 25, Conversion.DEFAULT_ASCII_CHARSET),
		}, {
			bldType("Record-Type", 1, 1, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-1b", 2, 22, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-2b", 24, 33, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-3b", 57, 11, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
			bldType("Field-4b", 68, 11, 0, Type.ftChar, Conversion.DEFAULT_ASCII_CHARSET),
		}
	};

	private final boolean lowerCase, normalNames;
	private final ICobolIOBuilder ioBldr;
	
	public TstMultiCopybookCommon(boolean lowerCase, ICobolIOBuilder ioBldr) {
		this(lowerCase, ioBldr, true);
	}
	public TstMultiCopybookCommon(boolean lowerCase, ICobolIOBuilder ioBldr, boolean normalNames) {
		super();
		this.lowerCase = lowerCase;
		this.ioBldr = ioBldr;
		this.normalNames = normalNames;
	}
	
	public void tstFields() throws Exception {
		LayoutDetail l = ioBldr.getLayout();
		
		TestCase.assertEquals(5, l.getRecordCount());
		TestCase.assertEquals(Constants.IO_CONTINOUS_NO_LINE_MARKER, l.getFileStructure());
		TestCase.assertEquals(Conversion.DEFAULT_ASCII_CHARSET, l.getFontName());
		
		for (int i = 0; i < l.getRecordCount(); i++) {
			RecordDetail r = l.getRecord(i);
			
//			System.out.println("\t}, {");
			for (int j = 0; j < r.getFieldCount(); j++) {
				String t2 = i + ", " + j;

	    		FieldDetail fld = r.getField(j);
	    		FieldDetail ef =  EXPECTED_FIELDS[i][j];
				TestCase.assertEquals(t2, ef.getName(), fld.getName());
				TestCase.assertEquals(t2, ef.getPos(), fld.getPos());
				TestCase.assertEquals(t2, ef.getLen(), fld.getLen());
				TestCase.assertEquals(t2, ef.getDecimal(), fld.getDecimal());
				TestCase.assertEquals(t2, ef.getType(), fld.getType());
				TestCase.assertEquals(t2, ef.getFontName(), fld.getFontName());

//				System.out.println("\t\tbldType(\"" + field.getName() 
//					+ "\", " + field.getPos()
//					+ ", "   + field.getLen()
//					+ ", "   + field.getDecimal()
//					+ ", "   + field.getType()
//					+ ", \"" + l.getFontName() 
//					+ "\"),"
//				);

			}
			
			TestCase.assertEquals("rec: " + i, EXPECTED_FIELDS[i].length, r.getFieldCount());
			
			if (normalNames) {
				TestCase.assertEquals("rec: " + i, RECORD_NAMES[i], r.getRecordName());
			} else {
				TestCase.assertEquals("rec: " + i, RECORD_NAMES2[i], r.getRecordName());
			}
//			System.out.print(" \"" + r.getRecordName() + "\",");
		}
		System.out.println();
	}



	public void tstRead() throws Exception {

		AbstractLine line;
		List<Line> list = bldLines();
		String s = toString(list);
			
		if (lowerCase) {
			s = s.toLowerCase();
		}
		AbstractLineReader r = ioBldr.newReader(new ByteArrayInputStream(s.getBytes()));
		
		line  = r.read();
		
		iAssertEquals(HEADER_ID, line.getFieldValue(RECORD_TYPE).asString());
		iAssertEquals("20150711", line.getFieldValue("Creation-Date").asString());
		iAssertEquals("1.00", line.getFieldValue("Version").asString());
		iAssertEquals(list.get(0).getFullLine(), line.getFullLine());
		
		for (int i = 1; i < DETAIL_COUNT; i++) {
			line = r.read();
			System.out.println(i +" >" + line.getFullLine() + "<");
			int idx = (i-1) * 3 + 1;
			
			
			iAssertEquals(list.get(idx).getFullLine(), line.getFullLine());
			
			iAssertEquals(DETAIL_ID, line.getFieldValue(RECORD_TYPE).asString());
			iAssertEquals("Fld_1_" + i, line.getFieldValue("Field-1").asString());
			iAssertEquals("Fld_2_" + i, line.getFieldValue("Field-2").asString());
			iAssertEquals("Fld_3_" + i, line.getFieldValue("Field-3").asString());

			line = r.read();
			System.out.println(i +" >" + line.getFullLine() + "<");
			
			iAssertEquals(list.get(idx + 1).getFullLine(), line.getFullLine());
			iAssertEquals(REC_A_ID, line.getFieldValue(RECORD_TYPE).asString());
			iAssertEquals("Fld_1a_" + i, line.getFieldValue("Field-1A").asString());
			iAssertEquals("Fld_2a_" + i, line.getFieldValue("Field-2A").asString());
			iAssertEquals("Fld_3a_" + i, line.getFieldValue("Field-3A").asString());			
			iAssertEquals("Fld_4a_z" + i, line.getFieldValue("Field-4A").asString());			
			
			line = r.read();
			System.out.println(i +" >" + line.getFullLine() + "<");
			
			iAssertEquals(list.get(idx + 2).getFullLine(), line.getFullLine());
			iAssertEquals(REC_B_ID, line.getFieldValue(RECORD_TYPE).asString());
			iAssertEquals("Fld_1b_" + i, line.getFieldValue("Field-1B").asString());
			iAssertEquals("Fld_2b_" + i, line.getFieldValue("Field-2B").asString());
			iAssertEquals("Fld_3b_" + i, line.getFieldValue("Field-3B").asString());			
			iAssertEquals("Fld_4b_z" + i, line.getFieldValue("Field-4B").asString());			
		}

		line = r.read();
		iAssertEquals(list.get(list.size() - 1).getFullLine(), line.getFullLine());
		iAssertEquals(TRAILER_ID, line.getFieldValue(RECORD_TYPE).asString());
		TestCase.assertEquals(list.size(), line.getFieldValue("Record-Count").asInt());
		TestCase.assertTrue(r.read() == null);

		r.close();
	}
	
	
	private void iAssertEquals(String expected, String actual) {
		if (lowerCase) {
			TestCase.assertEquals(expected.toLowerCase(), actual);
		} else {
			TestCase.assertEquals(expected, actual);
		}
	}
	
	private String toString(List<Line> lines) {
		StringBuilder b = new StringBuilder();
		
		for (Line l : lines) {
			b.append(l.getFullLine());
		}
		return b.toString();
	}
	
	
	private List<Line> bldLines() throws Exception {
		LayoutDetail l =  ioBldr.getLayout();
		ArrayList<Line> lines = new ArrayList<Line>();
		Line line = new Line(l);
		Line lineA = new Line(l);
		Line lineB = new Line(l);
		
		line.getFieldValue(RECORD_TYPE).set(HEADER_ID);
		line.getFieldValue("Creation-Date").set("20150711");
		line.getFieldValue("Version").set(1);
		lines.add(line);
		
		for (int i = 1; i < DETAIL_COUNT; i++) {
			line = new Line(l);
			lineA = new Line(l);
			lineB = new Line(l);
			line.getFieldValue(RECORD_TYPE).set(DETAIL_ID);
			lineA.getFieldValue(RECORD_TYPE).set(REC_A_ID);
			lineB.getFieldValue(RECORD_TYPE).set(REC_B_ID);
			
			for (int j = 1; j < 4; j++) {
				line.getFieldValue("Field-" + j).set("Fld_" + j + "_" + i);
				lineA.getFieldValue("Field-" + j + "A").set("Fld_" + j + "a_" + i);
				lineB.getFieldValue("Field-" + j + "B").set("Fld_" + j + "b_" + i);
			}
			lineA.getFieldValue("Field-" + 4 + "A").set("Fld_" + 4 + "a_z" + i);
			lineB.getFieldValue("Field-" + 4 + "B").set("Fld_" + 4 + "b_z" + i);

			System.out.println(lineA.getFullLine());
			System.out.println(lineB.getFullLine());

			lines.add(line);
			lines.add(lineA);
			lines.add(lineB);

		}
		
		line = new Line(l);
		
		line.getFieldValue(RECORD_TYPE).set(TRAILER_ID);
		line.getFieldValue("Record-Count").set(lines.size() + 1);
		lines.add(line);
		return lines;
				
    /* ------------------------------------------------------------
    
       01 Header-Record.
          05 Record-Type                            Pic X.
             88 Header-Record         value 'H'.
         05 Creation-Date                           Pic 9(8).
         05 Version                                 pic 9(3)V99.
         
       01 Detail-Record.
          05 Rec-Type                               Pic X.
             88 Detail-Record         value 'D'.
          05 Field-1                                Pic X(10).
          05 Field-2                                Pic X(20).
          05 Field-3                                Pic X(10).
              
       01 Trailer-Record.
          05 Rec-Type                               Pic X.
             88 Trailer-Record        value 'T'.
          05 Record-Count                           Pic 9(9).  

    		
       ------------------------------------------------------------ */

	}


	private static FieldDetail bldType(String name, int pos, int len, int decimal, int type, String font) {
		FieldDetail fd = new FieldDetail(name, "", type, decimal, font, 0, Conversion.DEFAULT_ASCII_CHARSET);
		fd.setPosLen(pos, len);
		
		return fd;
	}
}
