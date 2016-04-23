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

package net.sf.JRecord.zTest.Common;

import net.sf.JRecord.Common.Conversion;
import junit.framework.TestCase;

public class TestConversion extends TestCase {

	/**
	 * Check zoned conversion
	 */
	public void testToZoned() {
		char[] positiveSign = {'{', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'};

		char[] negativeSign = {'}', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',};

		
//		System.out.println(Conversion.toZoned("10") + " " + Conversion.toZoned("-10"));
		assertEquals("1{", Conversion.toZoned("10"));
		assertEquals("1}", Conversion.toZoned("-10"));
		assertEquals("10", Conversion.fromZoned("1{"));
		assertEquals("-10", Conversion.fromZoned("1}"));
		assertEquals("11", Conversion.fromZoned("1A"));
		assertEquals("-11", Conversion.fromZoned("1J"));
		assertEquals("11", Conversion.fromZoned("1a"));
		assertEquals("-11", Conversion.fromZoned("1j"));
		assertEquals("987654321{", Conversion.toZoned("9876543210"));
		for (int i = 0; i < 10; i++) {
			assertEquals("1" + positiveSign[i], Conversion.toZoned("1" + i));
			assertEquals("1" + negativeSign[i], Conversion.toZoned("-1" + i));
			assertEquals("1" + i, Conversion.fromZoned( "1" + positiveSign[i]));
			assertEquals("-1" + i, Conversion.fromZoned("1" + negativeSign[i]));
			assertEquals("1" + i, Conversion.fromZoned( ("1" + positiveSign[i]).toLowerCase()));
			assertEquals("-1" + i, Conversion.fromZoned(("1" + negativeSign[i]).toLowerCase()));
		}
		
		Conversion.setDefaultEbcidicCharacterset("CP037");
		assertEquals("1{", Conversion.toZoned("10"));
		assertEquals("1}", Conversion.toZoned("-10"));
		assertEquals("10", Conversion.fromZoned("1{"));
		assertEquals("-10", Conversion.fromZoned("1}"));
		
		
		Conversion.setDefaultEbcidicCharacterset("CP273");
//		System.out.println(Conversion.toZoned("10") + " " + Conversion.toZoned("-10"));
		assertEquals(toCp273("1{"), Conversion.toZoned("10"));
		assertEquals(toCp273("1}"), Conversion.toZoned("-10"));
		assertEquals("10", Conversion.fromZoned(toCp273("1{")));
		assertEquals("-10", Conversion.fromZoned(toCp273("1}")));
		
		Conversion.setDefaultEbcidicCharacterset("IBM037");
		assertEquals("1{", Conversion.toZoned("10"));
		assertEquals("1}", Conversion.toZoned("-10"));
		assertEquals("10", Conversion.fromZoned("1{"));
		assertEquals("-10", Conversion.fromZoned("1}"));

	}
	
	private String toCp273(String s) {
		return Conversion.toString(Conversion.getBytes(s, "CP037"), "CP273");
	}
}
