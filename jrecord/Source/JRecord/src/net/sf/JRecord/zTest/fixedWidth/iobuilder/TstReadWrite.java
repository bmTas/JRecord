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

package net.sf.JRecord.zTest.fixedWidth.iobuilder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import junit.framework.TestCase;


/**
 * Test Reading and writing files using Fixed-Width-IO builders
 * 
 * @author Bruce Martin
 *
 */
public class TstReadWrite extends TestCase {

	public static String[][] FIELD_VALUES = {
		{"63604808", "20", "40118", "170", "1", "4.87"},
		{"69684558", "20", "40118", "280", "1", "19.00"},
		{"69684558", "20", "40118", "280", "-1", "-19.00"},
		{"69694158", "20", "40118", "280", "1", "5.01"},
		{"62684671", "20", "40118", "685", "1", "69.99"},
		{"62684671", "20", "40118", "685", "-1", "-69.99"},
		{"61664713", "59", "40118", "335", "1", "17.99"},
		{"61664713", "59", "40118", "335", "-1", "-17.99"},
		{"61684613", "59", "40118", "335", "1", "12.99"},
		{"68634752", "59", "40118", "410", "1", "8.99"},
		{"60694698", "59", "40118", "620", "1", "3.99"},
		{"60664659", "59", "40118", "620", "1", "3.99"},
		{"60614487", "59", "40118", "878", "1", "5.95"},
		{"68654655", "166", "40118", "60", "1", "5.08"},
		{"69624033", "166", "40118", "80", "1", "18.19"},
		{"60604100", "166", "40118", "80", "1", "13.30"},
		{"68674560", "166", "40118", "170", "1", "5.99"},	
	};
	
	public static String FILE_LINES 
						= "63604808 20040118170 1  4.87\n"
						+ "69684558 20040118280 1 19.00\n"
						+ "69684558 20040118280-1-19.00\n"
						+ "69694158 20040118280 1  5.01\n"
						+ "62684671 20040118685 1 69.99\n"
						+ "62684671 20040118685-1-69.99\n"
						+ "61664713 59040118335 1 17.99\n"
						+ "61664713 59040118335-1-17.99\n"
						+ "61684613 59040118335 1 12.99\n"
						+ "68634752 59040118410 1  8.99\n"
						+ "60694698 59040118620 1  3.99\n"
						+ "60664659 59040118620 1  3.99\n"
						+ "60614487 59040118878 1  5.95\n"
						+ "68654655166040118 60 1  5.08\n"
						+ "69624033166040118 80 1 18.19\n"
						+ "60604100166040118 80 1 13.30\n"
						+ "68674560166040118170 1  5.99\n";
	
	public void testRead01() throws IOException  {
		doReadTest(defineByLength());
	}

		
	public void testRead02() throws IOException  {
		doReadTest(defineByPosition());
	}

	public void testWrite01() throws IOException  {
		doWriteTest(defineByLength());
	}

		
	public void testWrite02() throws IOException  {
		doWriteTest(defineByPosition());
	}


	
	private void doReadTest(IFixedWidthIOBuilder iob) throws IOException {
		
		AbstractLineReader r = iob.newReader(
				new ByteArrayInputStream(FILE_LINES.getBytes()));
		AbstractLine line;
		int i = 0;

		while ((line = r.read()) != null) {
			for (int j = 0; j < FIELD_VALUES[i].length; j++) {
				assertEquals(i + ", " + j , FIELD_VALUES[i][j], line.getFieldValue(0, j).asString());
			}
			i += 1;
		}
		assertEquals(FIELD_VALUES.length, i);
	}

	private void doWriteTest(IFixedWidthIOBuilder iob) throws IOException {
		ByteArrayOutputStream os = new ByteArrayOutputStream(FILE_LINES.length() + 100);
		AbstractLineWriter w = iob.newWriter(os);
		
		for (int i = 0; i < FIELD_VALUES.length; i++) {
			AbstractLine line = iob.newLine();
			for (int j = 0; j < FIELD_VALUES[i].length; j++) {
				line.getFieldValue(0, j).set(FIELD_VALUES[i][j]);
			}
			w.write(line);
		}
		
		w.close();
		
		assertEquals(FILE_LINES, Conversion.replace(new String(os.toByteArray()), "\r\n", "\n").toString());
	}

	/**
	 * @return
	 */
	private IFixedWidthIOBuilder defineByPosition() {
		return JRecordInterface1.FIXED_WIDTH.newIOBuilder()
				.defineFieldsByPosition()
					.addFieldByPosition("Sku"  , Type.ftChar             ,  1, 0)
					.addFieldByPosition("Store", Type.ftNumRightJustified,  9, 0)
					.addFieldByPosition("Date" , Type.ftNumZeroPadded    , 12, 0)
					.addFieldByPosition("Dept" , Type.ftNumRightJustified, 18, 0)
					.addFieldByPosition("Qty"  , Type.ftNumRightJustified, 21, 0)
					.addFieldByPosition("Price", Type.ftNumRightJustified, 23, 2)
				.endOfRecord(29);
	}

	/**
	 * @return
	 */
	private IFixedWidthIOBuilder defineByLength() {
		return JRecordInterface1.FIXED_WIDTH.newIOBuilder()
				.defineFieldsByLength()
					.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
					.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
					.addFieldByLength("Date" , Type.ftNumZeroPadded    , 6, 0)
					.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
					.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
					.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
				.endOfRecord();
	}

}
