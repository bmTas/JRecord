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
import net.sf.JRecord.Types.TypeSignSeparate;
import junit.framework.TestCase;

public class TstSignSeperate extends TestCase {

	static final String[][] VALUES = {
			{"-1", "01-", "-01" },
			{"-01", "01-", "-01" },
			{"-12", "12-", "-12" },
			{"+0", "00+", "+00" },
			{"+1", "01+", "+01" },
			{"+01", "01+", "+01" },
			{"+12", "12+", "+12" },
			{"0", "00+", "+00" },
			{"00", "00+", "+00" },
			{"1", "01+", "+01" },
			{"01", "01+", "+01" },
			{"12", "12+", "+12" },
	};
	

	static final String[][] VALUES_1_DECIMAL = {
			{"-.1",    "0001-",   "-0001" },
			{"-0.1",   "0001-",   "-0001" },
			{"-1",     "0010-",   "-0010" },
			{"-12",    "0120-",   "-0120" },
			{"-12.0",  "0120-",   "-0120" },
			{"-12.3",  "0123-",   "-0123" },
			{"-123",   "1230-",   "-1230" },
			{"-123.0", "1230-",   "-1230" },
			{"-123.4", "1234-",   "-1234" },
			{"+.1",    "0001+",   "+0001" },
			{"+0.1",   "0001+",   "+0001" },
			{"+1",     "0010+",   "+0010" },
			{"+12",    "0120+",   "+0120" },
			{"+12.0",  "0120+",   "+0120" },
			{"+12.3",  "0123+",   "+0123" },
			{"+123",   "1230+",   "+1230" },
			{"+123.0", "1230+",   "+1230" },
			{"+123.4", "1234+",   "+1234" },
			{"0",      "0000+",   "+0000" },
			{"0.0",    "0000+",   "+0000" },
			{"00",     "0000+",   "+0000" },
			{"00.0",   "0000+",   "+0000" },
			{".1",     "0001+",   "+0001" },
			{"0.1",    "0001+",   "+0001" },
			{"00.1",   "0001+",   "+0001" },
			{"000.1",  "0001+",   "+0001" },
			{"1",      "0010+",   "+0010" },
			{"01",     "0010+",   "+0010" },
			{"001",    "0010+",   "+0010" },
			{"12",     "0120+",   "+0120" },
			{"12.0",   "0120+",   "+0120" },
			{"12.3",   "0123+",   "+0123" },
			{"123",    "1230+",   "+1230" },
			{"123.0",  "1230+",   "+1230" },
			{"123.4",  "1234+",   "+1234" },
	};
	

	static final String[][] VALUES_2_DECIMAL = {
			{"-.01",     "00001-",  "-00001" },
			{"-0.01",    "00001-",  "-00001" },
			{"-0.12",    "00012-",  "-00012" },
			{"-1.23",    "00123-",  "-00123" },
			{"-12.34",   "01234-",  "-01234" },
			{"-123.45",  "12345-",  "-12345" },
			{"+.01",     "00001+",  "+00001" },
			{"+0.01",    "00001+",  "+00001" },
			{"+0.12",    "00012+",  "+00012" },
			{"+1.23",    "00123+",  "+00123" },
			{"+12.34",   "01234+",  "+01234" },
			{"+123.45",  "12345+",  "+12345" },
			{"0.12",     "00012+",  "+00012" },
			{"1.23",     "00123+",  "+00123" },
			{"12.34",    "01234+",  "+01234" },
			{"123.45",   "12345+",  "+12345" },
	};

	
	public void testTrailing() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateTrail;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[1], sep.formatValueForRecord(f, vals[0]));
			assertEquals(updateForCompareNoDecimal(vals[0]), sep.getField(vals[1].getBytes(), 1, f).toString());
		}
	}
	
	public void testLeading() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateLead;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES) {
			String id = (idx++) + " " + vals[0];
			assertEquals(id, vals[2], sep.formatValueForRecord(f, vals[0]));
			assertEquals(id, updateForCompareNoDecimal(vals[0]), sep.getField(vals[2].getBytes(), 1, f).toString());
		}
	}
	


	public void testTrailing1decimal() throws RecordException {
		int signSepTrailTypeId = Type.ftSignSeparateTrail;
		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 5, 1);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEquals(id, vals[1], sep.formatValueForRecord(sepTrailFld, vals[0]));
			assertEquals(id, updateForCompare1decimal(vals[0]), sep.getField(vals[1].getBytes(), 1, sepTrailFld).toString());
		}
	}

	public void testLeading1decimal() throws RecordException {
		int signSepTypeId = Type.ftSignSeparateLead;
		FieldDetail signSepFld = getField(signSepTypeId, 5, 1);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEquals(id, vals[2], sep.formatValueForRecord(signSepFld, vals[0]));
			assertEquals(id, updateForCompare1decimal(vals[0]), sep.getField(vals[2].getBytes(), 1, signSepFld).toString());
		}
	}


	public void testTrailing2decimal() throws RecordException {
		int signSepTrailTypeId = Type.ftSignSeparateTrail;
		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 6, 2);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);

		doCompare(sepTrailFld, sep, gen2DecimalTests(), 1);
		doCompare(sepTrailFld, sep, VALUES_2_DECIMAL, 1);
	}

	public void testLeading2decimal() throws RecordException {
		int signSepTrailTypeId = Type.ftSignSeparateLead;
		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 6, 2);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);

		doCompare(sepTrailFld, sep, gen2DecimalTests(), 2);
		doCompare(sepTrailFld, sep, VALUES_2_DECIMAL, 2);
	}


	/**
	 * @param sepTrailFld
	 * @param sep
	 * @param v
	 * @param cmpIdx
	 */
	public void doCompare(FieldDetail sepTrailFld, TypeSignSeparate sep,
			String[][] v, int cmpIdx) {
		int idx = 1;
		for (String[] vals : v) {
			String id = (idx++) + " " + vals[0];
			assertEquals(id, vals[cmpIdx], sep.formatValueForRecord(sepTrailFld, vals[0]));
			assertEquals(id, updateForCompare2decimal(vals[0]), sep.getField(vals[cmpIdx].getBytes(), 1, sepTrailFld).toString());
		}
	}

	/**
	 * @return
	 */
	public String[][] gen2DecimalTests() {
		String[][] v = new String[VALUES_1_DECIMAL.length][];
		
		for (int i = 0; i < VALUES_1_DECIMAL.length; i++) {
			v[i] = new String[3];
			StringBuilder b = new StringBuilder(VALUES_1_DECIMAL[i][1]);
			b.insert(b.length()- 1, '0');
			v[i][0] = VALUES_1_DECIMAL[i][0];
			v[i][1] = b.toString();
			v[i][2] = VALUES_1_DECIMAL[i][2] + '0';
		}
		return v;
	}

	/**
	 * @param cmp
	 * @return
	 */
	public String updateForCompare1decimal(String cmp) {
		cmp = updateForCompareNoDecimal(cmp);
		if (cmp.indexOf('.') < 0) {
			cmp = cmp + ".0";
		}
		return cmp;
	}
	/**
	 * @param cmp
	 * @return
	 */
	public String updateForCompare2decimal(String cmp) {
		cmp = updateForCompareNoDecimal(cmp);
		if (cmp.indexOf('.') < 0) {
			cmp = cmp + ".00";
		} else {
			while (cmp.indexOf('.') >= cmp.length() - 2) {
				cmp = cmp + '0';
			}
		}
		return cmp;
	}

	/**
	 * @param cmp
	 * @return
	 */
	public String updateForCompareNoDecimal(String cmp) {
		if (cmp.startsWith("+")) {
			cmp = cmp.substring(1);
		} else if (cmp.startsWith("-.")) {
			cmp = "-0" + cmp.substring(1);
		} 
		while(cmp.startsWith("-0") && (! cmp.startsWith("-0.")) && cmp.length() > 2) {
			cmp = "-" + cmp.substring(2);
		}
		while(cmp.startsWith("0") && (! cmp.startsWith("0.")) && cmp.length() > 1) {
			cmp = cmp.substring(1);
		}
		if (cmp.startsWith(".")) {
			cmp = '0' + cmp;
		}
		return cmp;
	}
	
	private FieldDetail getField(int type) {
		return getField(type, 3, 0);
//		FieldDetail f = new FieldDetail("", "", type, 0, "", 0, "");
//		f.setPosLen(1, 3);
//		return f;
	}
	

	
	private FieldDetail getField(int type, int len, int decimal) {
		FieldDetail f = new FieldDetail("", "", type, decimal, "", 0, "");
		f.setPosLen(1, len);
		return f;
	}
}
