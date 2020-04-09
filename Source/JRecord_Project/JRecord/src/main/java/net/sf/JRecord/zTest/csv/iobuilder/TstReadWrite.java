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
import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import junit.framework.TestCase;

/**
 * Reading / Writing with Csv IOBuilders
 * 
 * @author Bruce Martin
 *
 */
public class TstReadWrite extends TestCase {

	
	public void testReadComma() throws IOException {
		tst(',');
	}
	
	public void testReadSemi() throws IOException {
		tst(';');
	}

	public void testReadX00() throws IOException {
		tstBin((byte)0, "x'00'");
	}

	public void testReadX01() throws IOException {
		tstBin((byte)1, "x'01'");
	}

	public void testReadXFE() throws IOException {
		tstBin((byte)0xFe, "x'FE'");
	}

	public void testWriteComma() throws IOException {
		tstWrite(',');
	}
	
	public void testWriteSemi() throws IOException {
		tstWrite(';');
	}

	public void testWriteX00() throws IOException {
		tstWriteBin((byte)0, "x'00'");
	}

	public void testWriteX01() throws IOException {
		tstWriteBin((byte)1, "x'01'");
	}

	public void testWriteXFE() throws IOException {
		tstWriteBin((byte)0xFe, "x'FE'");
	}

	private void tst(char sepChar) throws IOException {
		tstRead((byte)sepChar, new String(new char[]{sepChar}));
	}

	private void tstRead(byte sepByte, String sep) throws IOException {
		byte[] data = CsvData.asBytes(sepByte);
		
		tstRead1(data, sep, true,  Constants.IO_UNICODE_NAME_1ST_LINE);
		tstRead1(data, sep, false, Constants.IO_UNICODE_NAME_1ST_LINE);
	}


	private void tstBin(byte sepByte, String sep) throws IOException {
		byte[] data = CsvData.asBytes(sepByte);
		
		tstRead1(data, sep, true,  Constants.IO_BIN_NAME_1ST_LINE);
		tstRead1(data, sep, false, Constants.IO_BIN_NAME_1ST_LINE);
	}

	private void tstRead1(byte[] data, String sep, boolean csvLine, int io) throws IOException {
		CommonBits.setUseCsvLine(csvLine);
		
		AbstractLineReader reader = JRecordInterface1.CSV
				.newIOBuilder(sep, "\"")
					.setFileOrganization(io)
					.setFont(Conversion.DEFAULT_ASCII_CHARSET)
				.newReader(new ByteArrayInputStream(data));
		AbstractLine l;
		int i = 1;
		
		while ((l = reader.read()) != null) {
			for (int j = 0; j < CsvData.CsvLineData[i].length; j++) {
				assertEquals(i + ", " + j, CsvData.CsvLineData[i][j], l.getFieldValue(0, j).asString());
			}
			i += 1;
		}
		assertEquals("lines read", CsvData.CsvLineData.length, i);
	}
	
	private void tstWrite(char sepChar) throws IOException {
		tstWrite((byte)sepChar, new String(new char[]{sepChar}));
	}

	private void tstWrite(byte sepByte, String sep) throws IOException {
		byte[] data = CsvData.asBytes(sepByte);
		
		tstWrite(data, sep, true,  Constants.IO_UNICODE_NAME_1ST_LINE);
		tstWrite(data, sep, false, Constants.IO_UNICODE_NAME_1ST_LINE);
	}

	private void tstWriteBin(byte sepByte, String sep) throws IOException {
		byte[] data = CsvData.asBytes(sepByte);
		
		tstWrite(data, sep, true,  Constants.IO_BIN_NAME_1ST_LINE);
		tstWrite(data, sep, false, Constants.IO_BIN_NAME_1ST_LINE);
	}

	private void tstWrite(byte[] data, String sep, boolean csvLine, int io) throws IOException {
		CommonBits.setUseCsvLine(csvLine);
		ICsvIOBuilder iob = JRecordInterface1.CSV
								.newIOBuilder(sep, "\"")
	        					.defineFields()
		       						.addCsvField(CsvData.CsvLineData[0][0], Type.ftChar, 0)
		       						.addCsvField(CsvData.CsvLineData[0][1], Type.ftNumAnyDecimal, 0)
		       						.addCsvField(CsvData.CsvLineData[0][2], Type.ftNumAnyDecimal, 0)
		       						.addCsvField(CsvData.CsvLineData[0][3], Type.ftNumAnyDecimal, 0)
		       						.addCsvField(CsvData.CsvLineData[0][4], Type.ftNumAnyDecimal, 0)
		       						.addCsvField(CsvData.CsvLineData[0][5], Type.ftNumAnyDecimal, 0)
		       					.endOfRecord()
								.setFont(Conversion.DEFAULT_ASCII_CHARSET)
								.setFileOrganization(io);
		ByteArrayOutputStream os = new ByteArrayOutputStream(data.length +  100);
		AbstractLineWriter w = iob.newWriter(os);
		AbstractLine l;
		
		for (int i = 1; i < CsvData.CsvLineData.length; i++) {
			l = iob.newLine();
			for (int j = 0; j < CsvData.CsvLineData[i].length; j++) {
				l.getFieldValue(0, j).set(CsvData.CsvLineData[i][j]);
			}
			w.write(l);
		}
		w.close();
		
		byte[] b = os.toByteArray();
		if (io == Constants.IO_UNICODE_NAME_1ST_LINE) {
			assertEquals(new String(data), new String(b));
		} else {
			assertEquals(new String(data), new String(b));
//			assertEquals(data.length, b.length);
//			assertTrue();
		}
	}
}
