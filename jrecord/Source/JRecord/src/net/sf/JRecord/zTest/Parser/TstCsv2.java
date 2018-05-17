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

import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.ByteArray;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CharLine;
import net.sf.JRecord.Details.CsvLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

public class TstCsv2 extends TestCase {

	private static String[] CSV_LINES = {
		"Year,Make,Model,Description,Price",
		"1997,Ford,E350,\"ac, abs, moon\",3000.00",
		"1999,Chevy,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00",
		"1996,Jeep,Grand Cherokee,\"MUST SELL!\nair, moon roof, loaded\",4799.00",
		"1999,Chevy,\"Venture \"\"Extended Edition, Very Large\"\"\",,5000.00",
		",,\"Venture \"\"Extended Edition\"\"\",\"\",4900.00",
	};


	private static String[] CSV_LINES_ALT = {
		null,
		null,
		"1999,Chevy,\"Venture \"\"Extended Edition\"\"\",,4900.00",
		null,
		null,
		",,\"Venture \"\"Extended Edition\"\"\",,4900.00",
	};

	private static String[][] CSV_LINE_FIELD = {
		{"Year", "Make", "Model", "Description", "Price"},
		{"1997", "Ford", "E350", "ac, abs, moon", "3000.00"},
		{"1999", "Chevy", "Venture \"Extended Edition\"", "", "4900.00"},
		{"1996", "Jeep", "Grand Cherokee", "MUST SELL!\nair, moon roof, loaded", "4799.00"},
		{"1999", "Chevy", "Venture \"Extended Edition, Very Large\"", "", "5000.00"},
		{"", "", "Venture \"Extended Edition\"", "", "4900.00"},
	};
	
	private static String[] CSV_LINES2 = {
		"\"field 1\",\"field 2\",\"field 3\"",
		"\"val-row1\",\"<h1>heading-row1</h1>\n<p>text-row1</p>\",\"lastvalue-1\"",
		"\"val-row2\",\"<h1>heading-row2</h1>\n<p>text-row2</p>\",\"lastvalue-2\"",
	};
	
	private static String[] CSV_LINES2_ALT = {
		"field 1,field 2,field 3",
		"val-row1,\"<h1>heading-row1</h1>\n<p>text-row1</p>\",lastvalue-1",
		"val-row2,\"<h1>heading-row2</h1>\n<p>text-row2</p>\",lastvalue-2",
	};

	
	private static String[][] CSV_LINE_FIELD2 = {
		{"field 1","field 2","field 3"},
		{"val-row1","<h1>heading-row1</h1>\n<p>text-row1</p>","lastvalue-1"},
		{"val-row2","<h1>heading-row2</h1>\n<p>text-row2</p>","lastvalue-2"},
	};
	
	



	public void testCsvParser11() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine("Standard Line a", new Line(getCsvLayout()));
		tstLine("Standard Line b", new Line(getCsvLayoutByte()));
		tstLineBin("Standard Line c", new Line(getCsvLayoutByte()));
		tstLineBin("Standard Line d", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin2("Standard Line e", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin3("Standard Line e", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
	}
	
	
	public void testCsvParser12() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine("Char Line", new CharLine(getCsvLayout(), ""));
		tstLine("Char Line", new CharLine(getCsvLayoutByte(), ""));
	}
	
	public void testCsvParser13() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine("Csv Line", new CsvLine(getCsvLayout()));
		tstLine("Csv Line", new CsvLine(getCsvLayoutByte()));
		tstLineBin("Csv Line", new CsvLine(getCsvLayoutByte()));
		tstLineBin("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin2("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin3("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
	}
	
	
	public void testCsvRead11() throws RecordException, IOException {
		CommonBits.setUseCsvLine(false);
		tstReadArray("CsvRead1: ", CSV_LINES, CSV_LINE_FIELD);
	}
	


	public void testCsvParser11a() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine("Standard Line", new Line(getCsvLayout()));
		tstLine("Standard Line", new Line(getCsvLayoutByte()));
		tstLineBin("Standard Line", new Line(getCsvLayoutByte()));
		tstLineBin("Standard Line", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin2("Standard Line", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin3("Standard Line", new Line(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
	}
	
	
	public void testCsvParser12a() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine("Char Line", new CharLine(getCsvLayout(), ""));
	}
	
	public void testCsvParser13a() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine("Csv Line", new CsvLine(getCsvLayout()));
	}
	
	
	public void testCsvParser13b() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine("Csv Line", new CsvLine(getCsvLayoutByte()));
		tstLineBin("Csv Line", new CsvLine(getCsvLayoutByte()));
		tstLineBin("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin2("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
		tstLineBin3("Standard Line", new CsvLine(getCsvLayout(Constants.IO_BIN_TEXT, "x'01'", "x'02'")));
	}
	
	
	public void testCsvRead11a() throws RecordException, IOException {
		CommonBits.setUseCsvLine(true);

		
		tstReadArray("CsvRead1: ", getCsvLayout(Constants.IO_CSV), 0, CSV_LINES, CSV_LINES_ALT, CSV_LINE_FIELD);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", ",", "\"").asLayoutDetail();
		tstReadArray("CsvRead1: ", schema, 1, CSV_LINES, CSV_LINES_ALT, CSV_LINE_FIELD);
	}


	public void testCsvRead11b() throws RecordException, IOException {
		CommonBits.setUseCsvLine(true);

		
		tstReadArray("CsvRead1: ", getCsvLayoutByte(Constants.IO_CSV), 0, CSV_LINES, CSV_LINES, CSV_LINE_FIELD);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "",
				CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"')).asLayoutDetail();
		//tstReadArray("CsvRead1: ", schema, 1, CSV_LINES, CSV_LINES, CSV_LINE_FIELD);
		tstReadArray("CsvRead1: ", schema, 1, CSV_LINES, CSV_LINES_ALT, CSV_LINE_FIELD);
	}

	/*---------------------------------------------------------------------------------*/
	
	
	public void testCsvParser21() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Standard Line 2", new Line(getCsvLayout2()));
	}
	
	
	public void testCsvParser22() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Char Line", new CharLine(getCsvLayout2(), ""));
	}
	
	public void testCsvParser23() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Csv Line", new CsvLine(getCsvLayout2()));
	}
	
	
	public void testCsvParser21a() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Standard Line 2", new Line(getCsvLayout2()));
	}
	
	
	public void testCsvParser22a() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Char Line", new CharLine(getCsvLayout2(), ""));
	}
	
	public void testCsvParser23a() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Csv Line", new CsvLine(getCsvLayout2()));
	}
	
	
	public void testCsvParser21bin() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Standard Line 2", new Line(getCsvLayout2byte()));
	}
	
	
	public void testCsvParser22bin() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Char Line", new CharLine(getCsvLayout2byte(), ""));
	}
	
	public void testCsvParser23bin() throws RecordException {
		CommonBits.setUseCsvLine(true);
		tstLine2("Csv Line", new CsvLine(getCsvLayout2byte()));
	}
	
	
	public void testCsvParser21aBin() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Standard Line 2", new Line(getCsvLayout2byte()));
	}
	
	
	public void testCsvParser22aBin() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Char Line", new CharLine(getCsvLayout2byte(), ""));
	}
	
	public void testCsvParser23aBin() throws RecordException {
		CommonBits.setUseCsvLine(false);
		tstLine2("Csv Line", new CsvLine(getCsvLayout2byte()));
	}

	public void testCsvRead21() throws RecordException, IOException {
		CommonBits.setUseCsvLine(false);
		String id = "CsvRead1: ";
		tstReadArray(id, getCsvLayout(Constants.IO_CSV), 0, CSV_LINES2, CSV_LINE_FIELD2);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", ",", "\"").asLayoutDetail();
		tstReadArray(id, schema, 1, CSV_LINES2, CSV_LINE_FIELD2);

	}


	public void testCsvRead21a() throws RecordException, IOException {
		CommonBits.setUseCsvLine(true);
		String id = "CsvRead1: ";
		tstReadArray(id, getCsvLayout(Constants.IO_CSV), 0, CSV_LINES2, CSV_LINES2_ALT, CSV_LINE_FIELD2);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", ",", "\"").asLayoutDetail();
		tstReadArray(id, schema, 1, CSV_LINES2, CSV_LINES2_ALT, CSV_LINE_FIELD2);
	}


	public void testCsvRead21Bin() throws RecordException, IOException {
		CommonBits.setUseCsvLine(false);
		String id = "CsvRead1: ";
		tstReadArray(id, getCsvLayoutByte(Constants.IO_CSV), 0, CSV_LINES2, CSV_LINE_FIELD2);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", 
				CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"')).asLayoutDetail();
		tstReadArray(id, schema, 1, CSV_LINES2, CSV_LINE_FIELD2);
	}


	public void testCsvRead21aBin() throws RecordException, IOException {
		CommonBits.setUseCsvLine(true);
		String id = "CsvRead1: ";
		tstReadArray(id, getCsvLayout(Constants.IO_CSV), 0, CSV_LINES2, CSV_LINES2_ALT, CSV_LINE_FIELD2);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", 
				CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"')).asLayoutDetail();
		//tstReadArray(id, schema, 1, CSV_LINES2, CSV_LINES2, CSV_LINE_FIELD2);
		tstReadArray(id, schema, 1, CSV_LINES2, CSV_LINES2_ALT, CSV_LINE_FIELD2);
	}


	private void tstReadArray(String id, String[] lines, String[][] expected) throws RecordException, IOException {
		//LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV, "", ",", "\"").asLayoutDetail();
		tstReadArray(id, getCsvLayout(Constants.IO_CSV), 0, lines, lines, expected);
		
		LayoutDetail schema = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "", ",", "\"").asLayoutDetail();
		tstReadArray(id, schema, 1, lines, expected);
		
		LayoutDetail schema1 = ExternalRecord.newCsvRecord("", Constants.IO_CSV_NAME_1ST_LINE, "",
				CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"')).asLayoutDetail();
		tstReadArray(id, schema1, 1, lines, expected);
	}

	
	private void tstReadArray(String id, LayoutDetail schema, int firstLine, String[] lines, String[][] expected) throws RecordException, IOException {
		tstReadArray(id, schema, firstLine, lines, lines, expected);
	}

	
	private void tstReadArray(String id, LayoutDetail schema, int firstLine, String[] lines, String[] lines1, String[][] expected) throws RecordException, IOException {
		AbstractLineReader lineReader = LineIOProvider.getInstance().getLineReader(schema);
		AbstractLine line;
		int i = firstLine;
		
		lineReader.open(TestCommonCode.arrayToStream(lines), schema);
		
		while ((line = lineReader.read()) != null) {
			if (lines1[i] == null) {
				assertEquals(id + i, lines[i], line.getFullLine());
			} else {
				assertEquals(id + "~" + i, lines1[i], line.getFullLine());
			}
			
			for (int j = 0; j < expected[i].length; j++) {
				assertEquals(id + i + ", j", expected[i][j], line.getFieldValue(0, j).asString());
			}
			
			i += 1;
		}
	}

	private static LayoutDetail getCsvLayout() throws RecordException {
		return getCsvLayout(Constants.IO_BIN_TEXT);
	}
	
	private static LayoutDetail getCsvLayout(int filestructure) throws RecordException {
		return getCsvLayout(filestructure, ",", "\"");
	}
	

	private static LayoutDetail getCsvLayoutByte() throws RecordException {
		return getCsvLayoutByte(Constants.IO_BIN_TEXT);
	}
	
	private static LayoutDetail getCsvLayoutByte(int filestructure) throws RecordException {
		return getCsvLayout(filestructure, CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"'));
	}
	
	private static LayoutDetail getCsvLayout(int filestructure, String delim, String quote) throws RecordException {
		//ParserManager.setUseNewCsvParsers(true);
		return ExternalRecord.newCsvRecord("", filestructure, "", delim, quote)
					.addCsvField("Year", Type.ftChar, 0)
					.addCsvField("Make", Type.ftChar, 0)
					.addCsvField("Model", Type.ftChar, 0)
					.addCsvField("Description", Type.ftChar, 0)
					.addCsvField("Price", Type.ftChar, 0)					
				.asLayoutDetail();
	}
	

	private static LayoutDetail getCsvLayout2() throws RecordException {
		return getCsvLayout2(Constants.IO_BIN_TEXT);
	}
	
	private static LayoutDetail getCsvLayout2(int filestructure) throws RecordException {
		return getCsvLayout2(filestructure, ",", "\"");
	}
	
	private static LayoutDetail getCsvLayout2byte() throws RecordException {
		return getCsvLayout2byte(Constants.IO_BIN_TEXT);
	}
	
	private static LayoutDetail getCsvLayout2byte(int filestructure) throws RecordException {
		return getCsvLayout2(filestructure, CommonCsvTests.toHex(','), CommonCsvTests.toHex('\"'));
	}

	private static LayoutDetail getCsvLayout2(int filestructure, String delim, String quote ) throws RecordException {
		//ParserManager.setUseNewCsvParsers(true);
		ExternalRecord rec =  ExternalRecord.newCsvRecord("", filestructure, "", ",", "\"")
										.addCsvField("Field 1", Type.ftChar, 0)
										.addCsvField("Field 2", Type.ftChar, 0)
										.addCsvField("Field 3", Type.ftChar, 0)	
									.asExternalRecord();
		
		rec.setRecordStyle(CsvParserManagerChar.BASIC_TXT_INQUOTE);

		return rec.asLayoutDetail();
	}

	
	private void tstLine(String id, AbstractLine l) throws RecordException {
		tstLine(id, l, CSV_LINES, CSV_LINES_ALT, CSV_LINE_FIELD);
	}

	
	private void tstLine2(String id, AbstractLine l) throws RecordException {
		tstLine(id, l, CSV_LINES2, CSV_LINES2, CSV_LINE_FIELD2);
	}
	
	private void tstLine(String id, AbstractLine l, String[] data, String[] data2, String[][] expected) throws RecordException {
		
		for (int i = 0; i < data.length; i++) {
			l.setData(data[i]);
			for (int j = 0; j < expected[0].length; j++) {				
				AbstractFieldValue fieldValue = l.getFieldValue(0, j);
				if (! expected[i][j].equals( fieldValue.toString())) {
					assertEquals(id + " " + i + ", " + j, expected[i][j], fieldValue.toString());
				}
			}
			
			l.setData("");
			for (int j = 0; j < expected[0].length; j++) {
				l.getFieldValue(0, j).set(expected[i][j]);
			}			
			tstLine(id, data, data2, i, l.getFullLine());
			
			l.setData("");
			for (int j = expected[0].length - 1; j>= 0; j--) {
				l.getFieldValue(0, j).set(expected[i][j]);
			}			
			tstLine(id, data, data2, i, l.getFullLine());	
		}
	}
	
	
	private void tstLineBin(String id, AbstractLine l) throws RecordException {
		tstLine(id, l, convertArray(l, CSV_LINES), convertArray(l, CSV_LINES_ALT), CSV_LINE_FIELD);
	}
	
	private byte[][] convertArray(AbstractLine l, String[] data) {
		LayoutDetail layout = l.getLayout();
		String f = Conversion.DEFAULT_ASCII_CHARSET;
		byte delim = layout.getDelimiterDetails().asByte();
		byte quote = layout.getQuoteDetails().asByte();
		ByteArray ba = new ByteArray(50);
		byte[][] ret = new byte[data.length][];
		int count = 0;
		
		for (String s : data) {
			byte[] b = Conversion.getBytes(s, f);
			
			ba.clear();
			if (s == null) {
				ret[count++] = null;
			} else {
				for (int i = 0; i < s.length(); i++) {
					switch (s.charAt(i)) {
					case ',' : ba.add(delim);				break;
					case '\"': ba.add(quote);				break;
					default:
						ba.add(b[i]);
					}
				}
				ret[count++] = ba.toByteArray();
			}
		}
		return ret;
	}
	
	
	private void tstLineBin2(String id, AbstractLine l) throws RecordException {
		String[][] fields = new String[CSV_LINE_FIELD.length][];
		for (int i =0; i < fields.length; i++) {
			fields[i] = CSV_LINE_FIELD[i].clone();
			for (int j =0; j < fields[i].length; j++) {
				if (fields[i][j].indexOf('\n') >= 0) {
					fields[i][j] = Conversion.replace(fields[i][j], "\n", " ").toString();
				}
			}
		}
		tstLine(id, l, createLines(l, fields), new byte[fields.length][], fields);
	}
	
	private void tstLineBin3(String id, AbstractLine l) throws RecordException {
		String[][] fields = CSV_LINE_FIELD;
		tstLine(id, l, createLines(l, fields), new byte[fields.length][], fields);
	}

	private byte[][] createLines(AbstractLine l, String[][] fields) {
		LayoutDetail layout = l.getLayout();
		String f = Conversion.DEFAULT_ASCII_CHARSET;
		byte delim = layout.getDelimiterDetails().asByte();
		byte quote = layout.getQuoteDetails().asByte();
		ByteArray ba = new ByteArray(50);
		byte[][] ret = new byte[fields.length][];
		int count = 0;
		
		for (String[] fieldsInLine : fields) {
			ba.clear();
			ba.add(Conversion.getBytes(fieldsInLine[0], f));
			for (int i = 1; i < fieldsInLine.length; i++) {
				ba.add(delim);
				if (fieldsInLine[i].indexOf('\n') > 0 || fieldsInLine[i].indexOf('\r') > 0) {
					ba.add(quote).add(Conversion.getBytes(fieldsInLine[i], f)).add(quote);
				} else {
					ba.add(Conversion.getBytes(fieldsInLine[i], f));
				}
			}
			
			ret[count++] = ba.toByteArray();
		}
		return ret;
	}

	private void tstLine(String id, AbstractLine l, byte[][] data, byte[][] data2, String[][] expected) throws RecordException {
		
		for (int i = 0; i < data.length; i++) {
			boolean doTest = true;
			l.setData(data[i]);
			for (int j = 0; j < expected[0].length; j++) {				
				AbstractFieldValue fieldValue = l.getFieldValue(0, j);
				String val = fieldValue.toString();
				if (expected[i][j].equals( val)) {
					
				} else if(val.indexOf((char) 01) >= 0 || val.indexOf((char) 02) >= 0 ) {
					 doTest = false;
				} else {
					assertEquals(id + " " + i + ", " + j, expected[i][j], val);
				}
			}
			
			if (doTest) {
				l.setData("");
				for (int j = 0; j < expected[0].length; j++) {
					l.getFieldValue(0, j).set(expected[i][j]);
				}			
				tstLine(id, data, data2, i, l.getData());
				
				l.setData("");
				for (int j = expected[0].length - 1; j>= 0; j--) {
					l.getFieldValue(0, j).set(expected[i][j]);
				}			
				tstLine(id, data, data2, i, l.getData());	
			}
		}
	}

	private void tstLine(String id, String[] data, String[] data2,int i, String t) {
		if (data2[i] == null) {
			assertEquals(id + " #2 " + i, data[i], t);
		} else {
			assertEquals(id + " #3 " + i, data2[i], t);
		}
	}
	

	private void tstLine(String id, byte[][] data, byte[][] data2,int i, byte[] t) {
		if (data2[i] == null) {
			assertTrue(id + " #2 " + i + " " + new String(t), Arrays.equals(data[i], t));
		} else {
			assertTrue(id + " #3 " + i, Arrays.equals(data2[i], t));
		}
	}

}
