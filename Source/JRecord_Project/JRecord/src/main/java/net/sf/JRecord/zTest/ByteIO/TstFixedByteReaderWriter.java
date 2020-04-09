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

package net.sf.JRecord.zTest.ByteIO;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.ByteIO.FixedLengthByteReader;
import net.sf.JRecord.ByteIO.FixedLengthByteWriter;
import junit.framework.TestCase;

public class TstFixedByteReaderWriter extends TestCase {
	private static final int WRITE_COUNT = 65;
	private static final String CHARS = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRWXYZ1234567890,.<>:=-!@#$";
	
	private static final int[] TEST_SIZES = {1, 5, 10, 20};
	
	
	public void testWrite() throws IOException {
		
		for (int size : TEST_SIZES) {
			byte[] b = buildByteArray(size);
			
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
					assertEquals("Tst2: " + size + " - "+ i + ", " + j, 0, b[j]);
				}
			}
		}
	}
	
	public void testRead() throws IOException { 
		
		for (int size : TEST_SIZES) {
			byte[] buf = buildByteArray(size);
			byte[] b;
			FixedLengthByteReader r = new FixedLengthByteReader(size);
			
			r.open(new ByteArrayInputStream(buf));
			
			int i = 0;
			while ((b = r.read()) != null) {
				byte[] expected = getExpected(i, size);
				
				for (int j = 0; j < expected.length; j++) {
					assertEquals(expected[j], b[j]);
				}
				for (int j = expected.length; j < b.length; j++) {
					assertEquals(0, b[j]);
				}
				assertEquals(size, b.length);
				i += 1;
			}
			r.close();
		}
	}
	
	private byte[] buildByteArray(int recordLength) throws IOException {
		FixedLengthByteWriter w = new FixedLengthByteWriter(recordLength);
		
		ByteArrayOutputStream os = new ByteArrayOutputStream(650);
		w.open(os);
		for (int i = 0; i < WRITE_COUNT; i++) {
			w.write(getStr(i).getBytes());
		}
		w.close();
		
		return os.toByteArray();
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
