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

import static org.junit.jupiter.api.Assertions.assertEquals;

import java.io.IOException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.TypeSignSeparate;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.JUnit3Test;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;

public class TstSignSeperateDecimalPoint extends JUnit3Test   {

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
 			{"-.1",    "000.1-",   "-000.1" },
			{"-0.1",   "000.1-",   "-000.1" },
			{"-1",     "001.0-",   "-001.0" },
			{"-12",    "012.0-",   "-012.0" },
			{"-12.0",  "012.0-",   "-012.0" },
			{"-12.3",  "012.3-",   "-012.3" },
			{"-123",   "123.0-",   "-123.0" },
			{"-123.0", "123.0-",   "-123.0" },
			{"-123.4", "123.4-",   "-123.4" },
			{"+.1",    "000.1+",   "+000.1" },
			{"+0.1",   "000.1+",   "+000.1" },
			{"+1",     "001.0+",   "+001.0" },
			{"+12",    "012.0+",   "+012.0" },
			{"+12.0",  "012.0+",   "+012.0" },
			{"+12.3",  "012.3+",   "+012.3" },
			{"+123",   "123.0+",   "+123.0" },
			{"+123.0", "123.0+",   "+123.0" },
			{"+123.4", "123.4+",   "+123.4" },
			{"0",      "000.0+",   "+000.0" },
			{"0.0",    "000.0+",   "+000.0" },
			{"00",     "000.0+",   "+000.0" },
			{"00.0",   "000.0+",   "+000.0" },
			{".1",     "000.1+",   "+000.1" },
			{"0.1",    "000.1+",   "+000.1" },
			{"00.1",   "000.1+",   "+000.1" },
			{"000.1",  "000.1+",   "+000.1" },
			{"1",      "001.0+",   "+001.0" },
			{"01",     "001.0+",   "+001.0" },
			{"001",    "001.0+",   "+001.0" },
			{"12",     "012.0+",   "+012.0" },
			{"12.0",   "012.0+",   "+012.0" },
			{"12.3",   "012.3+",   "+012.3" },
			{"123",    "123.0+",   "+123.0" },
			{"123.0",  "123.0+",   "+123.0" },
			{"123.4",  "123.4+",   "+123.4" },           
	};
	

	static final String[][] VALUES_2_DECIMAL = {
			{"-.01",     "000.01-",  "-000.01" },
			{"-0.01",    "000.01-",  "-000.01" },
			{"-0.12",    "000.12-",  "-000.12" },
			{"-1.23",    "001.23-",  "-001.23" },
			{"-12.34",   "012.34-",  "-012.34" },
			{"-123.45",  "123.45-",  "-123.45" },
			{"+.01",     "000.01+",  "+000.01" },
			{"+0.01",    "000.01+",  "+000.01" },
			{"+0.12",    "000.12+",  "+000.12" },
			{"+1.23",    "001.23+",  "+001.23" },
			{"+12.34",   "012.34+",  "+012.34" },
			{"+123.45",  "123.45+",  "+123.45" },
			{"0.12",     "000.12+",  "+000.12" },
			{"1.23",     "001.23+",  "+001.23" },
			{"12.34",    "012.34+",  "+012.34" },
			{"123.45",   "123.45+",  "+123.45" }, 
	};

	
	@Test public void testTrailing() throws RecordException {
		int signSepTypeId = Type.ftSignSepTrailActualDecimal;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		
		for (String[] vals : VALUES) {
			assertEquals(vals[1], sep.formatValueForRecord(f, vals[0]));
			assertEquals(updateForCompareNoDecimal(vals[0]), sep.getField(vals[1].getBytes(), 1, f).toString());
		}
	}
	
	@Test public void testLeading() throws RecordException {
		int signSepTypeId = Type.ftSignSepLeadActualDecimal;
		FieldDetail f = getField(signSepTypeId);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[2], sep.formatValueForRecord(f, vals[0]));
			assertEqualsV3o(id, updateForCompareNoDecimal(vals[0]), sep.getField(vals[2].getBytes(), 1, f).toString());
		}
	}
	


	@Test public void testTrailing1decimal() throws RecordException {
		int signSepTrailTypeId = Type.ftSignSepTrailActualDecimal;
		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 6, 1);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[1], sep.formatValueForRecord(sepTrailFld, vals[0]));
			assertEqualsV3o(id, updateForCompare1decimal(vals[0]), sep.getField(vals[1].getBytes(), 1, sepTrailFld).toString());
		}
	}

	@Test public void testLeading1decimalCobol() throws RecordException, IOException {
		CobolDetails cblDetails = new CobolDetails("   03 field-1 pic s9(3).9 sign leading SEPARATE.");
		assertEquals(Type.ftSignSepLeadActualDecimal, cblDetails.field.getType());
		//int signSepTypeId = Type.ftSignSepLeadActualDecimal;
		//FieldDetail signSepFld = getField(signSepTypeId, 6, 1);
		//TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[2], cblDetails.type.formatValueForRecord(cblDetails.field, vals[0]));
			assertEqualsV3o(id, updateForCompare1decimal(vals[0]), cblDetails.type.getField(vals[2].getBytes(), 1, cblDetails.field).toString());
			cblDetails.line.getFieldValue(0, 0).set(vals[0]);
			assertEqualsV3o(id, vals[2], cblDetails.line.getFullLine());
		}
	}
	@Test public void testTrailing1decimalCobol() throws RecordException, IOException  {
		CobolDetails cblDetails = new CobolDetails("   03 field-1 pic s9(3).9 sign trailing SEPARATE.");
		
		assertEquals(Type.ftSignSepTrailActualDecimal, cblDetails.field.getType());
//		int signSepTrailTypeId = Type.ftSignSepTrailActualDecimal;
//		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 6, 1);
//		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[1], cblDetails.type.formatValueForRecord(cblDetails.field, vals[0]));
			assertEqualsV3o(id, updateForCompare1decimal(vals[0]), cblDetails.type.getField(vals[2].getBytes(), 1, cblDetails.field).toString());
			
			cblDetails.line.getFieldValue(0, 0).set(vals[0]);
			assertEqualsV3o(id, vals[1], cblDetails.line.getFullLine());
		}
	}

	@Test public void testLeading1decimal() throws RecordException {
		int signSepTypeId = Type.ftSignSepLeadActualDecimal;
		FieldDetail signSepFld = getField(signSepTypeId, 6, 1);
		TypeSignSeparate sep = new TypeSignSeparate(signSepTypeId);
		int idx = 1;
		
		for (String[] vals : VALUES_1_DECIMAL) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[2], sep.formatValueForRecord(signSepFld, vals[0]));
			assertEqualsV3o(id, updateForCompare1decimal(vals[0]), sep.getField(vals[2].getBytes(), 1, signSepFld).toString());
		}
	}


	@Test public void testTrailing2decimal() throws RecordException, IOException {

		CobolDetails cblDetails = new CobolDetails("   03 field-1 pic s9(3).99 sign trailing SEPARATE.");
		
		test2Decimal(Type.ftSignSepTrailActualDecimal, cblDetails, 1, VALUES_2_DECIMAL);
	}

	private void test2Decimal(int typeCode, CobolDetails cblDetails, int expectedIndex,
			String[][] testValues) {
		FieldDetail sepTrailFld = getField(typeCode, 7, 2);
		TypeSignSeparate sep = new TypeSignSeparate(typeCode);
		String[][] geneneratedDecimalTests = gen2DecimalTests();
		
		assertEquals(typeCode, cblDetails.field.getType());

		
		doCompare(sepTrailFld, sep, geneneratedDecimalTests, expectedIndex);
		doCompare(sepTrailFld, sep, testValues, expectedIndex);
		
		doCompare(sepTrailFld, cblDetails.type, gen2DecimalTests(), expectedIndex);
		doCompare(sepTrailFld, cblDetails.type, VALUES_2_DECIMAL, expectedIndex);
		
		for (String[] vals : testValues) {
			cblDetails.line.getFieldValue(0, 0).set(vals[0]);
			assertEquals(vals[expectedIndex], cblDetails.line.getFullLine());
		}
	}

	@Test public void testLeading2decimal() throws RecordException, IOException {

		CobolDetails cblDetails = new CobolDetails("   03 field-1 pic s9(3).99 sign leading SEPARATE.");
		
		test2Decimal(Type.ftSignSepLeadActualDecimal, cblDetails, 2, VALUES_2_DECIMAL);

		
//		int signSepTrailTypeId = Type.ftSignSepLeadActualDecimal;
//		FieldDetail sepTrailFld = getField(signSepTrailTypeId, 7, 2);
//		TypeSignSeparate sep = new TypeSignSeparate(signSepTrailTypeId);
//
//		doCompare(sepTrailFld, sep, gen2DecimalTests(), 2);
//		doCompare(sepTrailFld, sep, VALUES_2_DECIMAL, 2);
	}


	/**
	 * @param sepTrailFld
	 * @param sep
	 * @param v
	 * @param cmpIdx
	 */
	public void doCompare(FieldDetail sepTrailFld, Type sep,
			String[][] v, int cmpIdx) {
		int idx = 1;
		for (String[] vals : v) {
			String id = (idx++) + " " + vals[0];
			assertEqualsV3o(id, vals[cmpIdx], sep.formatValueForRecord(sepTrailFld, vals[0]));
			assertEqualsV3o(id, updateForCompare2decimal(vals[0]), sep.getField(vals[cmpIdx].getBytes(), 1, sepTrailFld).toString());
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
	
	private static class CobolDetails {
		final ICobolIOBuilder iob;
		final AbstractLine line;
		final FieldDetail field;
		final Type type;
		
		CobolDetails(String cobolDefinition) throws IOException {
			iob = JRecordInterface1.COBOL
					.newIOBuilder(new ReadCobolCopybook().setCopybookName("Calc").addFreeFormatCobolText(cobolDefinition))
					.setFont("ISO-8859-1")
					.setFileOrganization(Constants.IO_STANDARD_TEXT_FILE) // or IO_BIN_TEXT
					.setSplitCopybook(CopybookLoader.SPLIT_NONE);
				
			line = iob.newLine();
			field = iob.getLayout().getField(0, 0);
			type = TypeManager.getInstance().getType(field.getType());
		}

	}
}
