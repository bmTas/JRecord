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

package net.sf.JRecord.zTest.charIO;


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.sf.JRecord.charIO.FixedLengthCharReader;
import net.sf.JRecord.charIO.FixedLengthCharWriter;
import net.sf.JRecord.charIO.ICharWriter;
import junit.framework.TestCase;

/**
 * Test FixedLengthCharReader
 * - Character read for Fixed-Length-Character files.
 * @author Bruce Martin
 *
 */
public class TstFixedCharReaderWriter extends TestCase {
	private static final String SPACE_10 = "          ";
	private static final int COUNT = 20000;

	public void testFixedLengthCharReader1() throws IOException {
		FixedLengthCharReader r = new FixedLengthCharReader(SPACE_10.length());
		
		r.open(getInputStream(COUNT, null), "");
		
		for (int i = 0; i < COUNT; i++) {
			assertEquals("Checking line: " + i, formatLine(i), r.read());
		}
		assertTrue(r.read() == null);
	}
	

	public void testFixedLengthCharReader2() throws IOException {
		FixedLengthCharReader r = new FixedLengthCharReader(SPACE_10.length());
		String extra = "abc";
		
		r.open(getInputStream(COUNT, extra), "");
		
		for (int i = 0; i < COUNT; i++) {
			assertEquals("Checking line: " + i, formatLine(i), r.read());
		}
		assertEquals("Checking line: " + (COUNT), extra, r.read());
		assertTrue(r.read() == null);
	}
	
	
	/**
	 * Test a Fixed Length Char write
	 * @throws IOException any IOException
	 */
	public void testFixedLengthCharWriter() throws IOException {
		int length = SPACE_10.length();
		int num = COUNT;
		ICharWriter w = new FixedLengthCharWriter(length, "");
		ByteArrayOutputStream os = new ByteArrayOutputStream(num * length);
		
		w.open(os);
		
		for (int i = 0; i < num; i++) {
			w.write(Integer.toString(i));
		}
		
		w.close();
		
		byte[] byteArray = os.toByteArray();
		String s = new String(byteArray);
		System.out.println("--> " + (num * length) + " " + byteArray.length + " " + s.length());
		
		for (int i = 0; i < num; i++) {
			assertEquals("Checking line: " + i, formatLine(i), s.substring(i * length, i * length + length));
		}

	}
	
	private ByteArrayInputStream getInputStream(int count, String extra) {
		StringBuffer b = new StringBuffer();
		
		for (int i = 0; i < count; i++) {
			b.append(formatLine(i));
		}
		
		if (extra != null) {
			b.append(extra);
		}
		return new ByteArrayInputStream(b.toString().getBytes());
	}
	
	
	private String formatLine(int id) {
		String s = Integer.toString(id);
		return s + SPACE_10.substring(s.length());
	}
}
