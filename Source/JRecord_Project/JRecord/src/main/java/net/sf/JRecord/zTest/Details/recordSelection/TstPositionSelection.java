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

package net.sf.JRecord.zTest.Details.recordSelection;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Option.Options;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

/**
 * This class will test setting record Type by File position
 * i.e. Setting the first record to a header and the last record
 * to the trailer
 * 
 * @author Bruce Martin
 *
 */
public class TstPositionSelection extends TestCase {
	public static int COUNT = 100;
	
	public void test01() throws IOException {
		tst(true, true, true);
	}
	
	public void test02() throws IOException {
		tst(true, false, false);
		tst(false, true, false);
		tst(false, false, true);
	}

	public void test03() throws IOException {
		tst(false, true, true);
		tst(true, false, true);
		tst(true, true, false);
	}

	private void tst(boolean header, boolean middle, boolean trailer) throws IOException {
		
		ICobolIOBuilder bldr = getBuilder(header, middle, trailer);
		byte[] data = bldData(bldr);
		AbstractLineReader r = bldr.newReader(new ByteArrayInputStream(data));
		
		AbstractLine l = r.read();
		if (header) {
			assertEquals(0, l.getPreferredLayoutIdxAlt());
			assertEquals(0, l.getPreferredLayoutIdx());
		} else {
			assertTrue(l.getPreferredLayoutIdxAlt() < 0);
		}
		
		for (int i = 1; i < COUNT; i++) {
			l = r.read();
		
			if (middle) {
				assertEquals(1, l.getPreferredLayoutIdxAlt());
				assertEquals(1, l.getPreferredLayoutIdx());
			} else {
				assertTrue(l.getPreferredLayoutIdxAlt() < 0);
			}
		}
		l = r.read();
		if (trailer) {
			assertEquals(2, l.getPreferredLayoutIdxAlt());
			assertEquals(2, l.getPreferredLayoutIdx());
		} else {
			assertTrue(l.getPreferredLayoutIdxAlt() < 0);
		}
		r.close();
	}
	
	public void testxx() throws IOException {
		ICobolIOBuilder bldr = getBuilder(true, true, true);
		byte[] data = bldData(bldr);
		AbstractLineReader r = bldr.newReader(new ByteArrayInputStream(data));
		
		AbstractLine l = r.read();
		//assertEquals(0, l.getPreferredLayoutIdx());
		System.out.println();
		System.out.println();
		System.out.println("Header: " + l.getPreferredLayoutIdxAlt() + " " + l.getPreferredLayoutIdx());
		
		for (int i = 1; i < COUNT; i++) {
			l = r.read();
			System.out.println("Middle: " + l.getPreferredLayoutIdxAlt() + " " + l.getPreferredLayoutIdx());
		}
		l = r.read();
		System.out.println("Trailer: " + l.getPreferredLayoutIdxAlt() + " " + l.getPreferredLayoutIdx());
		r.close();
	}
	public byte[] bldData(ICobolIOBuilder bldr) throws IOException {
		int length = bldr.getLayout().getMaximumRecordLength();
		String s;
		AbstractLine l = bldr.newLine();
		StringBuilder b = new StringBuilder();
		l.getFieldValue("Field-1").set(1001);
		b.append(l.getFullLine()).append('\n');
		
		for (int i = 1; i < COUNT; i++) {
			l.setData(" ");
			l.getFieldValue("Field-2").set(1000 * i + 2);
			l.getFieldValue("Field-3").set(1000 * i + 3);
			s = l.getFullLine();
			if (s.length() > length) {
				s = s.substring(0, length);
			}

			//System.out.print(" > " + b.length() + " " + s.length() + ">" + s );
			b.append(s).append('\n');
		}
		l.setData(" ");
		l.getFieldValue("Record-Count").set(COUNT + 2);
		l.getFieldValue("Field-4").set(1000 * 100 + 4);
		b.append(l.getFullLine()).append('\n');
		System.out.println();
		System.out.println("*-----------------------------------------*");
		System.out.println(b);
		System.out.println("*-----------------------------------------*");
		System.out.println();
		return b.toString().getBytes();
	}
	
	
	public ICobolIOBuilder getBuilder(boolean header, boolean detail, boolean trailer) {
		ICobolIOBuilder ioBuilder =JRecordInterface1.COBOL
				.newIOBuilder( this.getClass().getResource("hdt.cbl").getFile())
					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL )
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE)
					;
		
		if (header) {
			ioBuilder.setRecordPositionCode("Header-Record", Options.RP_FIRST_RECORD_IN_FILE);
		}
		if (detail) {
			ioBuilder.setRecordPositionCode("Detail-Record", Options.RP_MIDDLE_RECORDS);
		}
		if (trailer) {
			ioBuilder.setRecordPositionCode("Trailer-Record", Options.RP_LAST_RECORD_IN_FILE);
		}
		return ioBuilder;

	}
}
