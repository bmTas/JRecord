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

package net.sf.JRecord.zTest.Types;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeNum;
import net.sf.JRecord.Types.TypePackedDecimal;
import junit.framework.TestCase;

public class TstComp3 extends TestCase {

	byte[] b1 = {0x01, 0x23, 0x4c};
	byte[] b2 = {0x01, 0x23, 0x4d};
	byte[] b3 = {0x01, 0x23, 0x4f};
	byte[] b4 = {0x00, 0x00, 0x0c};
	byte[] b5 = {0x00, 0x00, 0x0f};

	public void testGetField() {
		tstGetField(Type.ftPackedDecimal);
		tstGetField(Type.ftPackedDecimalPostive);
	}
	
	private void tstGetField(int typeId) {
		TypeNum t = (TypeNum) TypeManager.getInstance().getType(typeId);
		FieldDetail f = new FieldDetail("", "", typeId, 0, "", 0, "");
		f.setPosLen(1, 3);
		
		assertTrue(t instanceof TypePackedDecimal);
		assertEquals("1234", t.getField(b1, 1, f).toString());
		if (! t.isPositive()) {
			assertEquals("-1234", t.getField(b2, 1, f).toString());
		}
		assertEquals("1234", t.getField(b3, 1, f).toString());
		assertEquals("0", t.getField(b4, 1, f).toString());
		assertEquals("0", t.getField(b5, 1, f).toString());
	}

	public void testSetField() throws RecordException {
		tstSetField(Type.ftPackedDecimal, "1234", b1);
		tstSetField(Type.ftPackedDecimal, "-1234", b2);
		tstSetField(Type.ftPackedDecimalPostive, "1234", b3);
		tstSetField(Type.ftPackedDecimal, "0", b4);
		tstSetField(Type.ftPackedDecimalPostive, "0", b5);
	}
	
	
	
	private void tstSetField(int typeId, String val, byte[] expected) throws RecordException {
		TypeNum t = (TypeNum) TypeManager.getInstance().getType(typeId);
		byte[] record = {0,0,0};
		FieldDetail f = new FieldDetail("", "", typeId, 0, "", 0, "");
		f.setPosLen(1, 3);
	
		assertEquals(toHex(expected), toHex(t.setField(record, 1, f, val)));
	}
	
	public void testSetGet() throws RecordException {
		tstSetGet(Type.ftPackedDecimal, 1, "c");
		tstSetGet(Type.ftPackedDecimal, -1, "d");
		tstSetGet(Type.ftPackedDecimalPostive, 1, "f");
	}
	
	private void tstSetGet(int typeId, int sign, String suff) throws RecordException {
		String s;
		TypeNum t = (TypeNum) TypeManager.getInstance().getType(typeId);
		byte[] record = {0,0,0};
		FieldDetail f = new FieldDetail("", "", typeId, 0, "", 0, "");
		f.setPosLen(1, 3);

		for (int i = 1; i < 10000; i++) {
			s = i + suff;
			s = "000000".substring(s.length()) + s;
			int value = sign * i;
			byte[] rec = t.setField(record, 1, f, value);
			assertEquals(s, toHex(rec));
			assertEquals(value + "", t.getField(rec, 1, f).toString());
		}
	}

	private String toHex(byte[] b) {
		StringBuffer buf = new StringBuffer();
		String s;
		for (int i = 0; i < b.length; i++) {
			s = Integer.toString(b[i] &0xff, 16);
			if (s.length() == 1) {
				buf.append("0");
			}
			buf.append(s);
		}
		return buf.toString();
	}
}
