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

package net.sf.JRecord.zTest.Details.csv;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.CsvLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Types.Type;
import junit.framework.TestCase;

public class TstCsvLines extends TestCase {

	private String[] STRINGS = {
			"12345", ",2345", "1,345", "12,45", "123,5", "1234,", "'2345", 
			"1'345", "12'45", "123'5", "1234'", ",'345", ",2'45", ",23'5", 
			",234'", ",2345", "1,'45", "1,3'5", "1,34'", "'2,45", "12,'5", 
			"12,4'", "'2,45", "'234,", "1'34,", "12'4,", "1234,", "'2,45", 
			"'23,5", "'234,", ",'345", "1'345", "1'3,5", "1'34,", ",234'", 
			"1,34'", "12,4'", "123,'", "1234'", "''345", "'2'45", "'23'5",
			"'234'", "1''45", "1'3'5", "1'34'", "12''5", "12'4'", "123''",
			",,345", ",2,45", ",23,5", ",234,", "1,,45", "1,3,5", "1,34,", 
			"12,,5", "12,4,", "123,,",
	};
	
	public void testBasic1() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.BASIC_CSV_PARSER);
		IUpdateFld u = new UpdateByIndex();
		
		doTst(l, 0, u);
		doTst(l, 2, u);
	}
	
	public void testStandard1() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.STANDARD_CSV_PARSER);
		IUpdateFld u = new UpdateByIndex();
		
		doTst(l, 0, u);
		doTst(l, 2, u);
	}

	
	public void testQuoteChar1() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.DB_CSV_PARSER);
		IUpdateFld u = new UpdateByIndex();
		
		doTst(l, 0, u);
		//doTst(l, 2); - fails for reverse assignment
	}

	
	public void testBasic2() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.BASIC_CSV_PARSER);
		IUpdateFld u = new UpdateByField();
		
		doTst(l, 0, u);
		doTst(l, 2, u);
	}
	
	public void testStandard2() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.STANDARD_CSV_PARSER);
		IUpdateFld u = new UpdateByField();
		
		doTst(l, 0, u);
		doTst(l, 2, u);
	}

	
	public void testQuoteChar2() throws RecordException {
		LayoutDetail l = bldLayout1(CsvParserManagerChar.DB_CSV_PARSER);
		IUpdateFld u = new UpdateByField();
		
		doTst(l, 0, u);
		//doTst(l, 2); - fails for reverse assignment
	}


	private void doTst(LayoutDetail l, int m, IUpdateFld u) throws RecordException {
		AbstractLine[] lines = {null, null, null};	
		String[] vals = {"", "", ""};

		for (int i = 0; i < STRINGS.length; i++) {
			define(lines, l);
			
			vals[0] = STRINGS[i];
			u.setCheckFld("Check 1: " + STRINGS[i], lines, -m, STRINGS[i], vals, m);
			for (int j = 0; j < STRINGS.length; j++) {
				vals[1] = STRINGS[j];
				
				define(lines, l);
				u.setFld("Check 1a: " + STRINGS[i], lines, -m, STRINGS[i]);
				u.setCheckFld("Check 2: " + i + ", " + j + ": " + STRINGS[i] + " ! " + STRINGS[j], lines, 1-m, STRINGS[j], vals, m);
				for (int k = 0; k < STRINGS.length; k++) {
					vals[2] = STRINGS[k];
					
					u.setFld("Check 1b: " + STRINGS[i], lines, -m, STRINGS[i]);
					u.setFld("Check 2b: " + i + ", " + j + ": " + STRINGS[i] + " ! " + STRINGS[j], lines, 1-m, STRINGS[j]);
					u.setCheckFld("Check 3: " + STRINGS[i] + " ! " + STRINGS[j] + " ! " + STRINGS[k], lines, 2-m, STRINGS[k], vals, m);
				}
				vals[2] = "";
			}
			vals[1] = "";
		}
		
	}
	
	
	private void define(AbstractLine[] lines, LayoutDetail l) {
		lines[0] = new Line(l);
		lines[1] = new CharLine(l, "");
		lines[2] = new CsvLine(l);
	}

	
	private static LayoutDetail bldLayout1(int recStyle) {
		ExternalRecord r = ExternalRecord.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", ",", "'")
								.addCsvField("fld1", Type.ftChar, 0)
								.addCsvField("fld2", Type.ftChar, 0)
								.addCsvField("fld3", Type.ftChar, 0)
							.asExternalRecord()
								.setRecordStyle(recStyle);
		try {
			return r.asLayoutDetail();
		} catch (RecordException e) {
			throw new RuntimeException(e);
		}
	}
	
	private static interface IUpdateFld {
		void setFld(String id, AbstractLine[] lines, int flNum, String value)  throws RecordException;
		void setCheckFld(String id, AbstractLine[] lines, int flNum, String value, String[] values, int m) throws RecordException;
	}
	
	private static class UpdateByIndex implements IUpdateFld {
		public void setCheckFld(String id, AbstractLine[] lines, int flNum, String value, String[] values, int m) throws RecordException {
			setFld(id, lines, flNum, value);
			String text = lines[0].getFullLine();
			assertEquals(id, text, lines[1].getFullLine());
			assertEquals(id, text, lines[2].getFullLine());
			
			lines[0].setData(text);
			lines[1].setData(text);
			lines[2].setData(text);

			assertEquals(id, text, lines[0].getFullLine());
			assertEquals(id, text, lines[1].getFullLine());
			assertEquals(id, text, lines[2].getFullLine());

//			System.out.println(lines[0].getFullLine());
			for (AbstractLine line : lines) {
				for (int i = 0; i < values.length; i++) {
					assertEquals(id + " " + line.getClass().getName() + " " + i, 
							values[i], line.getFieldValue(0, Math.abs(i - m)).asString());
				}
			}
		}
		
		public void setFld(String id, AbstractLine[] lines, int flNum, String value) throws RecordException {
			for (AbstractLine line : lines) {
				line.getFieldValue(0, Math.abs(flNum)).set(value);
				assertEquals(id, value, line.getFieldValue(0, Math.abs(flNum)).asString());
			}
		}
	}
	
	
	private static class UpdateByField implements IUpdateFld {
		public void setCheckFld(String id, AbstractLine[] lines, int flNum, String value, String[] values, int m) throws RecordException {
			setFld(id, lines, flNum, value);
			String text = lines[0].getFullLine();
			LayoutDetail l = lines[0].getLayout();
			assertEquals(id, text, lines[1].getFullLine());
			assertEquals(id, text, lines[2].getFullLine());
			
			lines[0].setData(text);
			lines[1].setData(text);
			lines[2].setData(text);

			assertEquals(id, text, lines[0].getFullLine());
			assertEquals(id, text, lines[1].getFullLine());
			assertEquals(id, text, lines[2].getFullLine());

//			System.out.println(lines[0].getFullLine());
			for (AbstractLine line : lines) {
				for (int i = 0; i < values.length; i++) {
					assertEquals(id + " " + line.getClass().getName() + " " + i, 
							values[i], line.getFieldValue(l.getRecord(0).getField(Math.abs(i - m))).asString());
				}
			}
		}
		
		public void setFld(String id, AbstractLine[] lines, int flNum, String value) throws RecordException {
			LayoutDetail l = lines[0].getLayout();
			for (AbstractLine line : lines) {
				line.getFieldValue(0, Math.abs(flNum)).set(value);
				assertEquals(id, value, line.getFieldValue(l.getRecord(0).getField(Math.abs(flNum))).asString());
			}
		}
	}

	
}
