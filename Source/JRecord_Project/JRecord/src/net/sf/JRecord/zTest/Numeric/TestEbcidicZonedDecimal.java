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

package net.sf.JRecord.zTest.Numeric;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeZoned;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

public class TestEbcidicZonedDecimal extends TestCase {
	
	private static final String[] CHARSET_TO_TEST = TstConstants.EBCDIC_SINGLE_BYTE_CHARSETS;

    private static final byte HIGH_NYBLE = (byte) 0xf0;
    private static final byte LOW_NYBLE  = (byte) 0x0f;

	
	private TypeZoned tz = new TypeZoned();
	
	
	public void testZonedSetValue() throws RecordException {
		checkType(Type.ftZonedNumeric);
	}
	
	@SuppressWarnings("deprecation")
	public void testSmallZonedSetValue() throws RecordException {
		checkType(Type.ftZonedEbcdicSmall);
	}

	/**
	 * @param typeCode
	 */
	private void checkType(int typeCode) {
		byte[] pm1 = {(byte) (0xC0) , (byte) (0xD0), (byte) (0xF0), (byte) (0xF0)};
		for (String charset : CHARSET_TO_TEST) {
			FieldDetail fld = new FieldDetail("TstFld", "", typeCode, 0, charset, 0, "");
			fld.setPosLen(1, 8);
			
			System.out.println(charset + "\t" + Conversion.getString(pm1, 0, 2, charset));
			tstValue(fld,  0, (byte) 0xC0);
			for (int i = 1; i < 100; i+=1) {
				tstValue(fld, i);
			}
			
			for (int i = 100; i < 10000; i+=7) {
				tstValue(fld, i);
			}
		}
	}
	
	private void tstValue(FieldDetail fld, int val) throws RecordException {
		tstValue(fld,  val, (byte) 0xC0);
		
		tstValue(fld, -val, (byte) 0xD0);
	}
	
	
	private void tstValue(FieldDetail fld, int val, byte highNyble) throws RecordException {
		byte[] bi = {0,0,0,0,0,0,0,0,0,0,0,0,0};
		byte[] b = tz.setField(bi, 1, fld, val);
		int absVal = Math.abs(val);
		String id = " " + fld.getFontName() + " " + val; 
		
		int fldEnd = fld.getEnd() - 1;
		assertEquals("Test Sign: " +id, highNyble, (byte) (b[fldEnd] & HIGH_NYBLE));
		assertEquals("Test last char: "  +id, absVal % 10,  (b[fldEnd] & LOW_NYBLE));
		
		for (int j = 1; j <= fldEnd; j++) {
			absVal = absVal / 10;
			assertEquals("Test digit: " + (j + 1) + " in "  +id, absVal % 10,  (b[fldEnd - j] & LOW_NYBLE));
			assertEquals("Test nyble: " + id, ((byte) 0xF0), (byte) (b[fldEnd - j] & HIGH_NYBLE));
		}

		Object fldValue = tz.getField(b, 1, fld);
//		System.out.println("-->" + fldValue + "<--" + fldValue.getClass().getName());
		assertEquals("Check Value: "  +id, Integer.toString(val), fldValue.toString());
		
		byte[] record = new byte[fldEnd+1];
		System.arraycopy(b, 0, record, 0, record.length);
		
		if ("IBM930".equals(fld.getFontName())) {
			System.out.print('*');
		}
		assertEquals("Check formatValueForRecord: " + id, 
				Conversion.toString(record, fld.getFontName()), 
				tz.formatValueForRecord(fld, Integer.toString(val)));
	}
	
//	
//	public void testZonedLeadingAscii() {
//		String[] STD_EBCDIC = {
//				"IBM037",		"IBM1047",		"IBM273",		"IBM280",
//				"IBM285",		"IBM297",		"IBM500",		"IBM930",
//				"IBM935",		"IBM937",
//				
//				"cp037",	"cp273",	"cp277",	"cp278",
//				"cp280",	"cp284",	"cp285",	"cp290",
//				"cp297"
//		};
//		FieldDetail fld = new FieldDetail("TstFld", "", Type.ftZonedNumeric, 0, Conversion.DEFAULT_ASCII_CHARSET, 0, "");
//		fld.setPosLen(1, 3);
//		
//		for (String charset : STD_EBCDIC) {
//			System.out.println(">> " + charset + " ");
//			
//			Conversion.setDefaultEbcidicCharacterset(charset);
//		
//			chkSignLeading1(fld);
//		}
//	}
//
//	
//	public void testZonedLeadingEbcdic() {
//		
//		for (String charset : CHARSET_TO_TEST) {
//			System.out.println(">> " + charset + " ");
//			FieldDetail ebcdicFld = new FieldDetail("TstFld", "", Type.ftZonedNumeric, 0, charset, 0, "");
//			ebcdicFld.setPosLen(1, 3);
//			
//			Conversion.setDefaultEbcidicCharacterset(charset);
//		
//			chkSignLeading1(ebcdicFld);
//		}
//	}
//
//	String spaces8 = "        ";
//	
//	/**
//	 * @param fld
//	 */
//	protected void chkSignLeading1(FieldDetail fld) {
//		TypeZonedLeading zd = new TypeZonedLeading();
//		TypeZonedLeading zdp = new TypeZonedLeading(true);
//		String charset = fld.getFontName();
//		byte[] space8Bytes = Conversion.getBytes(spaces8, charset);
//		String tstCharset = Conversion.getDefaultSingleByteCharacterset();
//		boolean doGet =  ! ("IBM930".equals(tstCharset) || "IBM935".equals(tstCharset) || "IBM937".equals(tstCharset)
//				|| "CP930".equals(tstCharset) || "CP935".equals(tstCharset) || "CP937".equals(tstCharset));
//		
//		for (int i = 0; i < 1000; i++) {
//			int highestNum = i / 100;
//			StringBuilder expectedU = new StringBuilder(3)
//					.append(highestNum)
//					.append((i / 10) % 10)
//					.append(i % 10);
//			StringBuilder expectedP = (new StringBuilder(expectedU));
//			StringBuilder expectedN = (new StringBuilder(expectedU));
//			expectedP.setCharAt(0, toPositiveSignChar(highestNum));
//			expectedN.setCharAt(0, toNegativeSignChar(highestNum));
//			
//			String expUnsigned = expectedU.toString();
//			String message = charset + "< " + expUnsigned;
//			String numWithPlus = "+" + i;
//
////			if ("101".equals(expUnsigned)) {
////				System.out.println('*');
////			}
//			assertEquals(message, expUnsigned, zdp.formatValueForRecord(fld, Integer.toString(i)));
//			assertEquals(message, expectedP.toString(), zd.formatValueForRecord(fld, Integer.toString(i)));
//			assertEquals(message, i == 0 ? expectedP.toString() :expectedN.toString(), zd.formatValueForRecord(fld, Integer.toString(-i)));
//			assertEquals(message, expUnsigned, zdp.formatValueForRecord(fld, numWithPlus));
//			assertEquals(message, expectedP.toString(), zd.formatValueForRecord(fld, numWithPlus));
//			
//			for (int j = 0; j < 5; j++) {
//				String xU = createString(j, expectedU),
//					   xP = createString(j, expectedP),
//					   xN = createString(j, expectedN);
//				byte[] bytesU = zdp.setField(space8Bytes.clone(), j+1, fld, i);
//				byte[] bytesP = zd.setField(space8Bytes.clone(), j+1, fld, i);
//				String msg = message + " " + j;
//				assertEquals(msg, xU, Conversion.toString(bytesU, charset));
//				if (! xP.equals(Conversion.toString(bytesP, charset))) {
//					assertEquals(msg, xP, 
//							Conversion.toString(
//									zd.setField(space8Bytes.clone(), j+1, fld, i), 
//									charset));
//				}
//				assertEquals(msg, xU, 
//						Conversion.toString(
//								zdp.setField(space8Bytes.clone(), j+1, fld, numWithPlus), 
//								charset));
//				assertEquals(msg, xP, 
//						Conversion.toString(
//								zd.setField(space8Bytes.clone(), j+1, fld, numWithPlus), 
//								charset));
//				if (i > 0) {
//					byte[] bytesN = zd.setField(space8Bytes.clone(), j+1, fld, -i);
//					assertEquals(msg, xN, Conversion.toString(bytesN, charset));
//					if (doGet) assertEquals(msg, "-" + i, zd.getField(bytesN, j+1, fld).toString());
//				}
//				if (doGet) {
//					assertEquals(msg, "" + i, zd .getField(bytesP.clone(), j+1, fld).toString());
//					assertEquals(msg, "" + i, zdp.getField(bytesP.clone(), j+1, fld).toString());
//					assertEquals(msg, "" + i, zd .getField(bytesU.clone(), j+1, fld).toString());
//					assertEquals(msg, "" + i, zdp.getField(bytesU.clone(), j+1, fld).toString());
//				}
//			}
//		}
//	}
//	
//	
//	private String createString(int pos, CharSequence val) {
//		StringBuilder b = new StringBuilder(spaces8);
//		
//		for (int i = 0; i < val.length(); i++) {
//			b.setCharAt(pos + i, val.charAt(i));
//		}
//		return b.toString();
//	}
//	
//	private static char toPositiveSignChar(int v) {
//		return v == 0 ? Conversion.getPositive0EbcdicZoned()	: (char) ('0' + v + Conversion.EBCDIC_ZONED_POSITIVE_DIFF);
//	}
//	
//	
//	private static char toNegativeSignChar(int v) {
//		return v == 0 ? Conversion.getNegative0EbcdicZoned()	: (char) ('0' + v + Conversion.EBCDIC_ZONED_NEGATIVE_DIFF);
//	}

}
