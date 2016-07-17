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

package net.sf.JRecord.zTest.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.ByteIO.FixedLengthByteWriter;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.FixedLengthWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.IO.LineWriterWrapper;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.JRecordInterface1;
import junit.framework.TestCase;

public class TstFixedLineReaderWriter extends TestCase {
	private static final int WRITE_COUNT = 65;
	private static final String CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRWXYZ1234567890,.<>:=-!@#$";
	
	private static final int[] TEST_SIZES = {1, 5, 10, 20};
	
	private static String[] copybooks = {
		"          01  fld1         pic x(1).\n",
		"          01  fld1         pic x(5).\n",
		"          01  fld1         pic x(10).\n",
		"          01  fld1         pic x(20).\n",
	};
	
	
	public void testWrite1() throws IOException, RecordException {
		
		for (int k = 0; k < TEST_SIZES.length; k++) {
			chkWrite(TEST_SIZES[k], buildByteArray1(k));
		}
	}
	
	
	public void testWrite2() throws IOException, RecordException {
		
		for (int k = 0; k < TEST_SIZES.length; k++) {
			chkWrite(TEST_SIZES[k], buildByteArray2(k));
		}
	}
	public void testWrite3() throws IOException, RecordException {
		
		for (int k = 0; k < TEST_SIZES.length; k++) {
			chkWrite(TEST_SIZES[k], buildByteArray3(k));
		}
	}
	public void testWrite4() throws IOException, RecordException {
		
		for (int k = 0; k < TEST_SIZES.length; k++) {
			chkWrite(TEST_SIZES[k], buildByteArray4(k));
		}
	}

	private void chkWrite(int size, byte[] b) {
		
		for (int i = 0; i < WRITE_COUNT; i++) {
			int st = i * size;
			int en1 = st + Math.min(size, i);
			int en2 = st + size;
			byte[] expected = getExpected(i, size);
			for (int j = st; j < en1; j++) {
				assertEquals("Tst1: " + size + " - " + i + ", " + j,
						expected[j-st], 
						b[j]);
			}
			for (int j = en1; j < en2; j++) {
				if (b[j] != 32) { //Space
					assertEquals("Tst2: " + size + " - "+ i + ", " + j, 0, b[j]);
				}
			}
		}
	}
	
	
	public void testRead() throws IOException, RecordException { 
		
		for (int k = 0; k < TEST_SIZES.length; k++) {
			int size = TEST_SIZES[k];
			byte[] buf = buildByteArray1(k);
			AbstractLine l;
			ICobolIOBuilder ioBldr = getBldr(k);	
			AbstractLineReader r = ioBldr.newReader(new ByteArrayInputStream(buf));
			
			int i = 0;
			while ((l = r.read()) != null) {
				byte[] expected = getExpected(i, size);
				byte[] b = l.getData();
				
				for (int j = 0; j < expected.length; j++) {
					assertEquals(expected[j], b[j]);
				}
				for (int j = expected.length; j < b.length; j++) {
					if (b[j] != 32) { //Space
						assertEquals(0, b[j]);
					}
				}
				assertEquals(size, b.length);
				i += 1;
			}
			r.close();
		}
	}
	
	private byte[] buildByteArray1(int idx) throws IOException, RecordException {
		
		ICobolIOBuilder ioBldr = getBldr(idx);		
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(650);
		doWrite(ioBldr.newWriter(os), ioBldr.getLayout());
		return os.toByteArray();
	}

	
	private byte[] buildByteArray2(int idx) throws IOException, RecordException {
		
		ICobolIOBuilder ioBldr = getBldr(idx);		
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(650);
		FixedLengthWriter w = new FixedLengthWriter();

		w.open(os);
		doWrite(w, ioBldr.getLayout());
		return os.toByteArray();
	}
	
	
	private byte[] buildByteArray3(int idx) throws IOException, RecordException {
		
		ICobolIOBuilder ioBldr = getBldr(idx);		
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(650);
		LayoutDetail layout = ioBldr.getLayout();
		LineWriterWrapper w = new LineWriterWrapper(new FixedLengthByteWriter(layout.getMaximumRecordLength()));
		w.open(os);
		doWrite(w, layout);
		return os.toByteArray();
	}

	
	private byte[] buildByteArray4(int idx) throws IOException, RecordException {
		
		ICobolIOBuilder ioBldr = getBldr(idx);		
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(650);
		LayoutDetail layout = ioBldr.getLayout();
		@SuppressWarnings("deprecation")
		AbstractLineWriter lineWriter = LineIOProvider.getInstance().getLineWriter(Constants.IO_FIXED_LENGTH);
		lineWriter.open(os);
		doWrite(lineWriter, layout);
		return os.toByteArray();
	}


	private void doWrite(AbstractLineWriter w, LayoutDetail schema) throws IOException, RecordException{
	 
		for (int i = 0; i < WRITE_COUNT; i++) {
			Line l = new Line(schema, getStr(i).getBytes());
			
			w.write(l);
		}
		w.close();
	}
	
	private ICobolIOBuilder getBldr(int idx) {
		return JRecordInterface1.COBOL.newIOBuilder(
				new ByteArrayInputStream(copybooks[idx].getBytes()),
				"Copybook"
		).setFileOrganization(Constants.IO_FIXED_LENGTH);
	}
	
	private static byte[] getExpected(int idx, int recordLength) {
		String s = getStr(idx);
		if (s.length() > recordLength) {
			s = s.substring(0, recordLength);
		}
		
		return s.getBytes();
	}
	
	private static String getStr(int idx) {
		return CHARS.substring(0, idx);
	}
}
