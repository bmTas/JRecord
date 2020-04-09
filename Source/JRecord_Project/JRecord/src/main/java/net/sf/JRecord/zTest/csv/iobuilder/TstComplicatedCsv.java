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

package net.sf.JRecord.zTest.csv.iobuilder;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import junit.framework.TestCase;

/**
 * Testing reading / writing Csv files with fields
 * containing comma's and quotes
 * 
 * @author Bruce Martin
 *
 */
public class TstComplicatedCsv extends TestCase {

	public void testRead() throws IOException {
		tstRead(true);
		tstRead(false);
	}

	public void testWrite() throws IOException {
		tstWrite(true);
		tstWrite(false);
	}

	public void tstRead(boolean useCsvLine) throws IOException {
		byte[] b = Conversion.getBytes(
				CsvData.buildComplicatedCsv(),
				Conversion.DEFAULT_ASCII_CHARSET);
		AbstractLineReader r = JRecordInterface1.CSV
					.newIOBuilder(",", "'")
					.newReader(new ByteArrayInputStream(b));
		AbstractLine l;
		CommonBits.setUseCsvLine(useCsvLine);
		int i = 0, idx, lastIdx = -1;
		
		
		while ((l = r.read()) != null) {
			idx = l.getFieldValue(0, 0).asInt();
			if (idx != lastIdx) {
				i = 0;
			}
			for (int j = 1; j < 4; j++) {
				AbstractFieldValue fieldValue = l.getFieldValue(0, j);
				String msg = useCsvLine + " " + idx + ", " + i + ", " + j;
				if (idx == j) {
					assertEquals(msg, CsvData.STRINGS[i], fieldValue.asString());					
				} else {
					assertEquals(msg, j * 11, fieldValue.asInt());
				}
			}
			lastIdx = idx;
			i += 1;
		}
		
		r.close();
	}
	
	public void tstWrite(boolean useCsvLine) throws IOException {
		
		String expected = CsvData.buildComplicatedCsv();
		ICsvIOBuilder iob = JRecordInterface1.CSV
				.newIOBuilder(",", "'")
					.defineFields()
						.addCsvField(CsvData.COLS[0], Type.ftChar, 0)
						.addCsvField(CsvData.COLS[1], Type.ftChar, 0)
						.addCsvField(CsvData.COLS[2], Type.ftChar, 0)
						.addCsvField(CsvData.COLS[3], Type.ftChar, 0)
					.endOfRecord();
		ByteArrayOutputStream os = new ByteArrayOutputStream(expected.length());
		AbstractLineWriter w = iob.newWriter(os);
		AbstractLine line;
		CommonBits.setUseCsvLine(useCsvLine);

		for (int i = 1; i < 4; i++) {
			line = iob.newLine();
			for (String s : CsvData.STRINGS) {
				line.getFieldValue(0, 0).set(i);
				for (int j = 1; j < 4; j++) {
					if (i == j) {
						line.getFieldValue(0, j).set(s);
					} else {
						line.getFieldValue(0, j).set(11 * j);
					}
				}
				w.write(line);
			}
		}
		w.close();
		assertEquals(useCsvLine + " ", expected, Conversion.toString(os.toByteArray(), Conversion.DEFAULT_ASCII_CHARSET));
	}
}
