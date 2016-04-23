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

package net.sf.JRecord.zTest.copy;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.CsvLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.utilityClasses.Copy;
import net.sf.JRecord.utilityClasses.SchemaLoader;
import net.sf.JRecord.zExamples.cobol.toCsv.test.TestCobol2Csv_1;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import junit.framework.TestCase;

public class TstCopy extends TestCase {

	static LineIOProvider ioProvider = LineIOProvider.getInstance();
	
	private static final String DTAR020_DATA_FILE_NAME = TestCobol2Csv_1.class.getResource("DTAR020.bin").getFile();  
	private static final String DTAR020_COPBOOK_FILE_NAME = TestCobol2Csv_1.class.getResource("DTAR020.cbl").getFile();  
	private static final String DTAR021_COPBOOK_FILE_NAME = TstCopy.class.getResource("DTAR021.cbl").getFile();  
	private static final String DTAR022_COPBOOK_FILE_NAME = TstCopy.class.getResource("DTAR022.cbl").getFile();  
	
	private static final byte[] DTAR020_DATA = loadFile(DTAR020_DATA_FILE_NAME);
	
	
	
	public void testCopyFileByMatchingFieldNames1() throws Exception, RecordException {
	
		LayoutDetail outSchema = SchemaLoader.loadSchema(
				DTAR021_COPBOOK_FILE_NAME, CopybookLoader.SPLIT_NONE, 
				Conversion.DEFAULT_ASCII_CHARSET, ICopybookDialects.FMT_MAINFRAME)
				.asLayoutDetail();
		doTest(outSchema, true);
	}
	
	
	public void testCopyFileByMatchingFieldNames2() throws Exception, RecordException {
	
		LayoutDetail outSchema = SchemaLoader.loadSchema(
				DTAR022_COPBOOK_FILE_NAME, CopybookLoader.SPLIT_NONE, Conversion.DEFAULT_ASCII_CHARSET, ICopybookDialects.FMT_MAINFRAME)
				.asLayoutDetail();
		doTest(outSchema, false);
	}

	
	public void testCopyFileByMatchingFieldNamesCsv1() throws Exception, RecordException {
		CommonBits.setUseCsvLine(false);
		doTest( getCsvSchema(), true);
	}

	
	public void testCopyFileByMatchingFieldNamesCsv2() throws Exception, RecordException {
		CommonBits.setUseCsvLine(false);
		LayoutDetail outSchema = getCsvSchemaReverse();
			
		doTest(outSchema, false);
	}
	
	
	
	public void testCopyFileByMatchingFieldNamesCsv3() throws Exception, RecordException {
		CommonBits.setUseCsvLine(true);
		doTest( getCsvSchema(), true);
	}

	
	public void testCopyFileByMatchingFieldNamesCsv4() throws Exception, RecordException {
		CommonBits.setUseCsvLine(true);
		LayoutDetail outSchema = getCsvSchemaReverse();
			
		doTest(outSchema, false);
	}

	
	public void testCsvCopy1() throws Exception, RecordException {
		CommonBits.setUseCsvLine(false);
		tstCsvCopy1();
	}

	
	public void testCsvCopy2() throws Exception, RecordException {
		CommonBits.setUseCsvLine(true);
		tstCsvCopy1();
	}

	
	public void tstCsvCopy1() throws Exception, RecordException {
	
		LayoutDetail csvSchema1 = getCsvSchema();
		LayoutDetail csvSchema2 = getCsvSchemaReverse();
			
		LayoutDetail inSchema = SchemaLoader.loadSchema(DTAR020_COPBOOK_FILE_NAME, CopybookLoader.SPLIT_NONE, "CP037", ICopybookDialects.FMT_MAINFRAME)
				.asLayoutDetail();
		List<AbstractLine> DTAR020_LINES = readStream(inSchema, DTAR020_DATA);
				
		ByteArrayOutputStream out = new ByteArrayOutputStream(DTAR020_DATA.length * 3 / 2);
		ByteArrayOutputStream out2 = new ByteArrayOutputStream(DTAR020_DATA.length * 3 / 2);
		
		Copy.copyFileByMatchingFieldNames(
				getReader(inSchema, DTAR020_DATA), 
				getWriter(csvSchema1, out), 
				CommonBits.useCsvLine()? new CsvLine(csvSchema1):new Line(csvSchema1));
		byte[] outData1 = out.toByteArray();
		
		Copy.copyFileByMatchingFieldNames(
				getReader(csvSchema1, outData1), 
				getWriter(csvSchema2, out2), 
				CommonBits.useCsvLine()? new CsvLine(csvSchema2): new Line(csvSchema2));
		byte[] outData2 = out2.toByteArray();
		
		compare("Compare in & out 1: ", readStream(csvSchema1, outData1), readStream(csvSchema2, outData2), false);
		compare("Compare in & out 2: ", DTAR020_LINES, readStream(csvSchema2, outData2), false);
		
		compareCsv("Compare 3:", DTAR020_LINES, outData1, "\\|", true);
		compareCsv("Compare 3:", DTAR020_LINES, outData2, csvSchema2.getDelimiter(), false);
	}
	

	/**
	 * Copy a file from Fixed-Width binary file to another format. 
	 * Check the data is the same in the output file
	 * Then copy the "copied" file back to Fixed-Width Binary
	 * The input / output is compared to make sure the data is always the same
	 * 
	 * @param outSchema output schema
	 * @param normalSequence wether the fields are in the same or reverse sequence in the file
	 * 
	 * @throws Exception any error that occurs.
	 * @throws RecordException
	 */
	private void doTest(LayoutDetail outSchema, boolean normalSequence) throws Exception, RecordException {
		
		LayoutDetail inSchema = SchemaLoader.loadSchema(DTAR020_COPBOOK_FILE_NAME, CopybookLoader.SPLIT_NONE, "CP037", ICopybookDialects.FMT_MAINFRAME)
				.asLayoutDetail();
		List<AbstractLine> DTAR020_LINES = readStream(inSchema, DTAR020_DATA);
				
		ByteArrayOutputStream out = new ByteArrayOutputStream(DTAR020_DATA.length * 3 / 2);
//		ByteArrayOutputStream out1 = new ByteArrayOutputStream(DTAR020_DATA.length);
		
		Copy.copyFileByMatchingFieldNames(
				getReader(inSchema, DTAR020_DATA), 
				getWriter(outSchema, out), 
				new Line(outSchema));
		byte[] outData = out.toByteArray();
		compare("Compare in & out 1: ", DTAR020_LINES, readStream(outSchema, outData), normalSequence);
		
		
		if (normalSequence) {
			ByteArrayOutputStream out1 = new ByteArrayOutputStream(DTAR020_DATA.length  * 3 / 2);
			Copy.copyFileByFieldNumber(
					getReader(inSchema, DTAR020_DATA), 
					getWriter(outSchema, out1), 
					new Line(outSchema));
			
			byte[] outDataFieldSeq = out.toByteArray();
			compare("Compare in & out 2: ", DTAR020_LINES, readStream(outSchema, outDataFieldSeq), normalSequence);
			assertTrue(Arrays.equals(outData, outDataFieldSeq));
		}
		
		ByteArrayOutputStream out2 = new ByteArrayOutputStream(DTAR020_DATA.length);
		Copy.copyFileByMatchingFieldNames(
				getReader(outSchema, outData), 
				getWriter(inSchema, out2), 
				new Line(inSchema));
		
		byte[] outData2 = out2.toByteArray();
		compare("Compare in & out 3: ", DTAR020_LINES, readStream(inSchema, outData2), true);
		assertTrue(Arrays.equals(DTAR020_DATA, outData2));
	}


	private LayoutDetail getCsvSchema() throws RecordException {
		return 	ExternalRecord.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", "|", "\"")
					.addCsvField("KEYCODE-NO", Type.ftChar, 0)
					.addCsvField("STORE-NO",   Type.ftNumLeftJustified, 0)
					.addCsvField("DATE",       Type.ftNumLeftJustified, 0)
					.addCsvField("DEPT-NO",    Type.ftNumLeftJustified, 0)
					.addCsvField("QTY-SOLD",   Type.ftNumLeftJustified, 0)
					.addCsvField("SALE-PRICE", Type.ftNumLeftJustified, 2)
				.asLayoutDetail();
	}
	
	private LayoutDetail getCsvSchemaReverse() throws RecordException {
		return	ExternalRecord.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", ";", "\"")
					.addCsvField("SALE-PRICE", Type.ftNumLeftJustified, 2)
					.addCsvField("QTY-SOLD",   Type.ftNumLeftJustified, 0)
					.addCsvField("DEPT-NO",    Type.ftNumLeftJustified, 0)
					.addCsvField("DATE",       Type.ftNumLeftJustified, 0)
					.addCsvField("STORE-NO",   Type.ftNumLeftJustified, 0)
					.addCsvField("KEYCODE-NO", Type.ftChar, 0)
				.asLayoutDetail();
	}
	private static AbstractLineReader getReader(LayoutDetail schema, byte[] b) throws IOException, RecordException {
		return getReader(schema, new ByteArrayInputStream(b));
	}

	
	private static AbstractLineReader getReader(LayoutDetail schema, InputStream inputStream ) throws IOException, RecordException {
		AbstractLineReader lineReader = ioProvider.getLineReader(schema); 
		lineReader.open(inputStream, schema);
		
		return lineReader;
	}
	
	private static  AbstractLineWriter getWriter(LayoutDetail schema, ByteArrayOutputStream out) throws IOException {
		AbstractLineWriter w = ioProvider.getLineWriter(schema);
		w.open(out);
		return w;
	}
	
	private static byte[] loadFile(String filename) {
		return TestCommonCode.loadFile(filename);
//		File f = new File(filename);
//		byte[] b = new byte[(int)f.length()];
//		try {
//			BufferedInputStream in = new BufferedInputStream(new FileInputStream(f));
//			int n = in.read(b);
//			int num = n;
//			
//			while (n > 0) {
//				n = in.read(b, num, b.length - num);
//				num += n;
//			}
//			in.close();
//			return b;
//		} catch (IOException e) {
//			e.printStackTrace();
//		}
//		
//		return new byte[0];
	}
	
	private static void compare(String id, List<AbstractLine> l1, List<AbstractLine> l2, boolean normalFieldSequence) {
		int m = Math.min(l1.size(), l2.size());
		int fieldCount = l1.get(0).getLayout().getRecord(0).getFieldCount();
		int sub = fieldCount - 1;
		if (normalFieldSequence) {
			sub = 0;
		}
		
		for (int i = 0; i < m; i++) {
			for (int j = 0; j < fieldCount; j++) {
				assertEquals(id + " " + i + ", " + j + ", " + Math.abs(j - sub),
						l1.get(i).getFieldValue(0, j).asString(), l2.get(i).getFieldValue(0, Math.abs(j - sub)).asString());
			}
		}
		assertEquals(id, l1.size(), l2.size());
		assertEquals(id, fieldCount, l2.get(0).getLayout().getRecord(0).getFieldCount());
	}
	

	private static void compareCsv(String id, List<AbstractLine> l1, byte[] b, String delim, boolean normalFieldSequence) {
		int m = l1.size();
		LayoutDetail layout = l1.get(0).getLayout();
		int fieldCount = layout.getRecord(0).getFieldCount();
		
		String[] lines = Conversion.replace(new String(b),"\r", "").toString().split("\n");
		int sub = fieldCount - 1;
		if (normalFieldSequence) {
			sub = 0;
		}
		
		for (int i = 0; i < m; i++) {
			String[] fields = lines[i+1].split(delim);
			for (int j = 0; j < fieldCount; j++) {
				assertEquals(id + " " + i + ", " + j + ", " + Math.abs(j - sub),
						l1.get(i).getFieldValue(0, j).asString(), fields[Math.abs(j - sub)]);
			}
		}

	}

	
	private static List<AbstractLine> readStream(LayoutDetail schema, byte[] b) throws IOException, RecordException {
		
			AbstractLineReader r = getReader(schema, b);
			ArrayList<AbstractLine> lines = new ArrayList<AbstractLine>(b.length / Math.max(20,schema.getMaximumRecordLength()) + 2); 
			AbstractLine l;
			while ((l = r.read()) != null) {
				lines.add(l);
			}
			r.close();
			return lines;

	}
}
