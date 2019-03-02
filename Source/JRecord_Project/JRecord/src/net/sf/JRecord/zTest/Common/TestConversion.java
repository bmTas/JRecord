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
import net.sf.JRecord.Common.RecordException;
import junit.framework.TestCase;

public class TestConversion extends TestCase {

	/**
	 * Check zoned conversion
	 */
	public void testToZoned() {
		char[] positiveSign = {'{', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I'};

		char[] negativeSign = {'}', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',};

		Conversion.setDefaultEbcidicCharacterset("cp037");
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
	
	
//	public void testToPositiveByte() {
//		for (int i = 0; i < 128; i++ ) {
//			byte b = (byte) i;
//			assertEquals(b, (b) & 255);
//		}
//		for (int i = -128; i < 0; i++ ) {
//			byte b = (byte) i;
//			assertEquals((byte) 256 + i, (b) & 255);
//		}
//	}
	public void testToByte() {
		for (int i = -128; i < 256; i++) {
			assertEquals((byte) i, Conversion.long2byte(i));
		}
	}
	
	public void testSetLong() {
		byte[] rec = new byte[3];

		for (int i = 0; i < 256; i++) {
			for (int j = 0; j < 256; j++) {
				Conversion.setLong(rec, 0, 3, i * 256 + j, false);
				assertEquals(0, rec[0]);
				assertEquals(i, 255 & (rec[1]));
				assertEquals(j, 255 & (rec[2]));
			}
			for (int j = 1; j < 256; j++) {
				Conversion.setLong(rec, 0, 3, -(i * 256 + j), false);
				assertEquals(-1, rec[0]);
				assertEquals(i + ", " + j, (byte) ~i, rec[1]);
				assertEquals(i + ", " + j, (byte) -j, rec[2]);				
			}
		}
	}
	
	public void testSetLongLow2High1() {
		byte[] rec = new byte[3];

		for (int i = 0; i < 256; i++) {
			for (int j = 0; j < 256; j++) {
				int val = i * 256 + j;
				Conversion.setLongLow2High(rec, 0, 3, val, false);
				assertEquals(0, rec[2]);
				assertEquals(i, 255 & (rec[1]));
				assertEquals(j, 255 & (rec[0]));
			}
			for (int j = 1; j < 256; j++) {
				int val = -(i * 256 + j);
				Conversion.setLongLow2High(rec, 0, 3, val, false);
				assertEquals(-1, rec[2]);
				assertEquals(i + ", " + j, (byte) ~i, rec[1]);
				assertEquals(i + ", " + j, (byte) -j, rec[0]);
			}
		}
	}
	
	
	public void testSetLongLow2High2() {
		byte[] rec = new byte[4];

		for (int i = 0; i < 256; i++) {
			for (int j = 0; j < 256; j++) {
				for (int k = 0; k < 256; k++) {
					Conversion.setLongLow2High(rec, 0, 4, (i * 256 + j) * 256 + k, false);
					assertEquals(0, rec[3]);
					assertEquals(i, 255 & (rec[2]));
					assertEquals(j, 255 & (rec[1]));
					assertEquals(k, 255 & (rec[0]));
				}
			}
			for (int j = 0; j < 256; j++) {
				for (int k = 1; k < 256; k++) {
					Conversion.setLongLow2High(rec, 0, 4, -((i * 256 + j) * 256 + k), false);
					assertEquals(-1, rec[3]);
					String message = i + ", " + j + ", " + k;
					assertEquals(message , (byte) ~i, rec[2]);
					assertEquals(message, (byte) ~j, rec[1]);
					assertEquals(message, (byte) -k, rec[0]);
				}
			}
		}
	}
	
	public void testCheckLength() {
		int num = 256 * 256 * 256;
		int numDiv2 = num / 2;
		
		for (int i = 0; i < numDiv2; i++) {
			Conversion.checkLength(i, 3, false);
			Conversion.checkLength(i, 3, true);
			Conversion.checkLength(-i, 3, false);
		}
		
		System.out.print(numDiv2);
		boolean ok;
		checkPositiveNum(numDiv2);

		for (int i = numDiv2+1; i < num; i++) {
			checkPositiveNum(i);
			
			try {
				Conversion.checkLength(-i, 3, false);
				ok = false;
				Conversion.checkLength(-i, 3, false);
			} catch (RecordException x) {
				ok = true;
			}
			assertTrue(""+ i, ok);
		}
	}

	/**
	 * @param i
	 */
	private void checkPositiveNum(int i) {
		boolean ok;
		Conversion.checkLength(i, 3, true);
		
		try {
			Conversion.checkLength(i, 3, false);
			ok = false;
		} catch (RecordException x) {
			ok = true;
		}
		assertTrue(ok);
	}

	public void testGetByteFromHexString() {
		char[] hexDigits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
		
		for (int i = 0; i < 256; i++) {
			String hexStr = "" + hexDigits[i/16] + hexDigits[i % 16];
			assertEquals((byte) i, Conversion.getByteFromHexString("X'" + hexStr + "'"));
			assertEquals((byte) i, Conversion.getByteFromHexString("x'" + hexStr + "'"));
			assertEquals((byte) i, Conversion.getByteFromHexString("x" + hexStr));
			assertEquals((byte) i, Conversion.getByteFromHexString("X" + hexStr));
		}
		for (int i = 0; i < 16; i++) {
			String hexStr = "" + hexDigits[i];
			assertEquals((byte) i, Conversion.getByteFromHexString("X'" + hexStr + "'"));
			assertEquals((byte) i, Conversion.getByteFromHexString("x'" + hexStr + "'"));
			assertEquals((byte) i, Conversion.getByteFromHexString("x" + hexStr));
			assertEquals((byte) i, Conversion.getByteFromHexString("X" + hexStr));
		}
	}
	
	
	public void testGetCsvDelimBytes() {
		char[] hexDigits = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F'};
		String[] delims = {",", "\t", "!", "|"};
		
		String ascii = Conversion.DEFAULT_ASCII_CHARSET;
		for (int i = 0; i < 256; i++) {
			String hexStr = "" + hexDigits[i/16] + hexDigits[i % 16];
			checkCsvDelimBytes( i, "x'" + hexStr + "'");
			checkCsvDelimBytes( i, "X'" + hexStr + "'");
		}
		for (int i = 0; i < 16; i++) {
			String hexStr = "" + hexDigits[i];
			checkCsvDelimBytes( i, "x'" + hexStr + "'");
			checkCsvDelimBytes( i, "X'" + hexStr + "'");
		}
		
		for (String s : delims) {
			byte[] expected = Conversion.getBytes(s, ascii);
			byte[] actual = Conversion.getCsvDelimBytes(s, ascii, '\t');
			
			for (int i = 0; i < expected.length; i++) {
				assertEquals(expected[i], actual[i]);
			}
			assertEquals(expected.length, actual.length);

		}
	}
	

	public void testPackedDecimal() {
		char[] hexDigits = "0123456789abcdef".toCharArray();
		byte[] bytes = {0,0,0};
		for (byte v3 = 0; v3 < 10; v3++) {
			bytes[1] =  v3;
			for (int v2 = 0; v2 < 10; v2++) {
				for (int v1 = 10; v1 < 16; v1++) {
					bytes[2] = (byte) (v2 * 16 + v1);
					if (v1 != 13) {
						assertEquals("id: " + v1,
								new StringBuilder("000").append(v3).append(hexDigits[v2]).toString(), 
								Conversion.getMainframePackedDecimal(bytes, 0, 3));
						assertEquals(
								new StringBuilder("0").append(v3).append(hexDigits[v2]).toString(), 
								Conversion.getMainframePackedDecimal(bytes, 1, 2));
					} else {
						assertEquals("id: " + v2 + " "+ v1,
								new StringBuilder("-000").append(v3).append(hexDigits[v2]).toString(), 
								Conversion.getMainframePackedDecimal(bytes, 0, 3));
						assertEquals(
								new StringBuilder("-0").append(v3).append(hexDigits[v2]).toString(), 
								Conversion.getMainframePackedDecimal(bytes, 1, 2));
					}
				}
			}	
		}
	}
	

	public void testDecimal() {
		char[] hexDigits = "0123456789abcdef".toCharArray();
		byte[] bytes = {0,0,0};
		for (byte v3 = 0; v3 < 10; v3++) {
			bytes[1] =  v3;
			for (int v2 = 0; v2 < 16; v2++) {
				for (int v1 = 0; v1 < 16; v1++) {
					bytes[2] = (byte) (v2 * 16 + v1);
					assertEquals(
							new StringBuilder("000").append(v3).append(hexDigits[v2]).append(hexDigits[v1]).toString(), 
							Conversion.getDecimal(bytes, 0, 3));
					assertEquals(
							new StringBuilder("0").append(v3).append(hexDigits[v2]).append(hexDigits[v1]).toString(), 
							Conversion.getDecimal(bytes, 1, 3));
				}
			}	
		}
	}

	public void testGetBitField() {
		
		byte[] bytes = {0,0,0};
		for (int i = 0; i < 64000; i++ ) {
			bytes[1] = (byte) (i / 256);
			bytes[2] = (byte) i;
			String s1 = Conversion.getBitField(bytes, 0, 3);
			String s2 = Conversion.getBitField(bytes, 1, 3);
			
			assertEquals("00000000", s1.substring(0, 8));
			assertEquals(s2, s1.substring(8));
			
			int calc = i;
			for (int j = s2.length() - 1; j >= 0; j--) {
				if (calc % 2 == 0) {
					assertEquals('0', s2.charAt(j));
				} else {
					assertEquals('1', s2.charAt(j));
				}
				calc = calc / 2;
			}
			
		}
	}
	
	
	public void testGetPostiveBinary() {
		byte[] bytes = {0,0,0};
		for (int v3 = 0; v3 < 256; v3++) {
			bytes[2] = (byte) v3;
			for (int v2 = 0; v2 < 256; v2++) {
				bytes[1] = (byte) v2;
				for (int v1 = 0; v1 < 256; v1++) {
					int expected = (v3 * 256 + v2) * 256 + v1;
					bytes[0] = (byte) v1;

					assertEquals(Integer.toString(expected), Conversion.getPostiveBinary(bytes, 0, 3));
				}
			}
		}
	}
	
	public void testGetLittleEndianBigInt() {
		byte[] bytes = {0,0,0};
		for (int v3 = 0; v3 < 128; v3++) {
			bytes[2] = (byte) v3;
			for (int v2 = 0; v2 < 256; v2++) {
				bytes[1] = (byte) v2;
				for (int v1 = 0; v1 < 256; v1++) {
					int expected = (v3 * 256 + v2) * 256 + v1;
					bytes[0] = (byte) v1;

					assertEquals(expected, Conversion.getLittleEndianBigInt(bytes, 0, 3).intValue());
				}
			}
		}
	}
	
	
	public void testGetPositiveBigInt() {
		byte[] bytes = {0,0,0};
		for (int v3 = 0; v3 < 256; v3++) {
			bytes[0] = (byte) v3;
			for (int v2 = 0; v2 < 256; v2++) {
				bytes[1] = (byte) v2;
				for (int v1 = 0; v1 < 256; v1++) {
					int expected = (v3 * 256 + v2) * 256 + v1;
					bytes[2] = (byte) v1;

					assertEquals(expected, Conversion.getPositiveBigInt(bytes, 0, 3).intValue());
				}
			}
		}
	}

	private void checkCsvDelimBytes(int i, String x) {
		byte[] csvDelimBytes = Conversion.getCsvDelimBytes(x, Conversion.DEFAULT_ASCII_CHARSET, '\t');
		assertEquals(x, (byte) i, csvDelimBytes[0]);
		assertEquals(x, 1, csvDelimBytes.length);
	}

	
	private String toCp273(String s) {
		return Conversion.toString(Conversion.getBytes(s, "CP037"), "CP273");
	}
}
