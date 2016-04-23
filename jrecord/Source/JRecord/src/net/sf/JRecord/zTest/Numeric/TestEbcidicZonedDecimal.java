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
		
		byte[] pm1 = {(byte) (0xC0) , (byte) (0xD0), (byte) (0xF0), (byte) (0xF0)};
		for (String charset : CHARSET_TO_TEST) {
			FieldDetail fld = new FieldDetail("TstFld", "", Type.ftZonedNumeric, 0, charset, 0, "");
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
		
//		for (String charset : PROBLEM_CHARSET) {
//			boolean supported = Charset.isSupported(charset);
//			System.out.println(charset + "\t" + supported
//					+ "\t" + Conversion.isSingleByte(charset)
//					+ "\t" + Conversion.getString(pm1, 0, 2, charset));
//			if (supported) {
//				System.out.print(Conversion.getString(pm1, 0, 2, charset));
//			}
//		}
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
}
