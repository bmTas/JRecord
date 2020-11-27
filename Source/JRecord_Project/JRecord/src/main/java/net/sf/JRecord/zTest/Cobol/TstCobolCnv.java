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

package net.sf.JRecord.zTest.Cobol;

import java.io.IOException;
import java.io.StringReader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstCobolCnv extends TestCase {
	private static final String COBOL_COPBOOK1
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic 9(4)V99+.\n";
	private static final String COBOL_COPBOOK2
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic 9(4)V99-.\n";
	private static final String COBOL_COPBOOK3
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic +9(4)V99.\n";
	private static final String COBOL_COPBOOK4
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic -9(4)V99.\n";
	private static final String COBOL_COPBOOK5
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic 9(4).99+.\n";
	private static final String COBOL_COPBOOK6
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic 9(4).99-.\n";
	private static final String COBOL_COPBOOK7
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic +9(4).99.\n";
	private static final String COBOL_COPBOOK8
				= "         01 Test-Record.\n"
				+ "           03 field1              Pic -9(4).99.\n";
	private static final String COBOL_COPBOOK9
				= "         01 Test-Record.\n"
				+ "           03 field2              Pic 9(4)V99.\n";
	
	public void testGet1() throws IOException {
		chkSignTrailing(getIOBuilder());
	}

	public void testGet2() throws IOException {
		chkSignTrailing(getIOBuilder2());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSignTrailing(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldDetail fieldDef = fieldValue.getFieldDetail();
		
		assertEquals(fieldValue.getTypeName(), Type.ftSignSeparateTrail, fieldDef.getType());
		assertEquals(1, fieldDef.getPos());
		assertEquals(7, fieldDef.getLen());
		assertEquals(2, fieldDef.getDecimal());
		
		line.setData("012345+");
		assertEquals("123.45", fieldValue.asString());
		line.setData("012345-");
		assertEquals("-123.45", fieldValue.asString());
		line.setData("123456+");
		assertEquals("1234.56", fieldValue.asString());
		line.setData("123456-");
		assertEquals("-1234.56", fieldValue.asString());
	}
	

	public void testGet3() throws IOException {
		chkSignLeading(getIOBuilder3());
	}

	public void testGet4() throws IOException {
		chkSignLeading(getIOBuilder4());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSignLeading(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldDetail fieldDef = fieldValue.getFieldDetail();
		
		assertEquals(Type.ftSignSeparateLead, fieldDef.getType());
		assertEquals(1, fieldDef.getPos());
		assertEquals(7, fieldDef.getLen());
		assertEquals(2, fieldDef.getDecimal());
		
		line.setData("+012345");
		assertEquals("123.45", fieldValue.asString());
		line.setData("-012345");
		assertEquals("-123.45", fieldValue.asString());
		line.setData("+123456");
		assertEquals("1234.56", fieldValue.asString());
		line.setData("-123456");
		assertEquals("-1234.56", fieldValue.asString());
	}

	public void testGet5() throws IOException {
		chkTrailingDot(getIOBuilder5());
	}

	public void testGet6() throws IOException {
		chkTrailingDot(getIOBuilder6());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkTrailingDot(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldDetail fieldDef = fieldValue.getFieldDetail();
		
		assertEquals(fieldValue.getTypeName(), Type.ftSignSepTrailActualDecimal, fieldDef.getType());
		assertEquals(1, fieldDef.getPos());
		assertEquals(8, fieldDef.getLen());
		assertEquals(2, fieldDef.getDecimal());
		
		line.setData("0123.45+");
		assertEquals("123.45", fieldValue.asString());
		line.setData("0123.45-");
		assertEquals("-123.45", fieldValue.asString());
		line.setData("1234.56+");
		assertEquals("1234.56", fieldValue.asString());
		line.setData("1234.56-");
		assertEquals("-1234.56", fieldValue.asString());
	}


	public void testGet7() throws IOException {
		chkLeadingDot(getIOBuilder7());
	}


	public void testGet8() throws IOException {
		chkLeadingDot(getIOBuilder8());
	}
	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkLeadingDot(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldDetail fieldDef = fieldValue.getFieldDetail();
		
//		assertEquals(Type.ftSignSeparateTrail, fieldDef.getType());
		assertEquals(1, fieldDef.getPos());
		assertEquals(8, fieldDef.getLen());
		assertEquals(2, fieldDef.getDecimal());
		
		line.setData("+0123.45");
		assertEquals("123.45", fieldValue.asString());
		line.setData("-0123.45");
		assertEquals("-123.45", fieldValue.asString());
		line.setData("-1234.56");
		assertEquals("-1234.56", fieldValue.asString());
		line.setData("-1234.56");
		assertEquals("-1234.56", fieldValue.asString());
	}

	public void testGet9() throws IOException {
		ICobolIOBuilder iob = getIOBuilder9();
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		IFieldDetail fieldDef = fieldValue.getFieldDetail();
		
		assertEquals(Type.ftAssumedDecimalPositive, fieldDef.getType());
		assertEquals(1, fieldDef.getPos());
		assertEquals(6, fieldDef.getLen());
		assertEquals(2, fieldDef.getDecimal());

		line.setData("012345");
		assertEquals("123.45", fieldValue.asString());
		line.setData("123456");
		assertEquals("1234.56", fieldValue.asString());
		//System.out.println(fieldValue.asString() + " " + fieldValue.getFieldDetail().getType());
	}
	
	public void testSet1() throws IOException {
		chkSet1(getIOBuilder());
		chkSet1(getIOBuilder2());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSet1(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		fieldValue.set("123.45");
		assertEquals(fieldValue.getTypeName(), "012345+", line.getFullLine());
		fieldValue.set("+123.45");
		assertEquals("012345+", line.getFullLine());
		fieldValue.set("-123.45");
		assertEquals("012345-", line.getFullLine());
		
		fieldValue.set("1234.56");
		assertEquals("123456+", line.getFullLine());
		fieldValue.set("+1234.56");
		assertEquals("123456+", line.getFullLine());
		fieldValue.set("-1234.56");
		assertEquals("123456-", line.getFullLine());
	}
	
	
	public void testSet3() throws IOException {
		chkSet3(getIOBuilder3());
		chkSet3(getIOBuilder4());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSet3(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		fieldValue.set("123.45");
		assertEquals(fieldValue.getTypeName(), "+012345", line.getFullLine());
		fieldValue.set("+123.45");
		assertEquals("+012345", line.getFullLine());
		fieldValue.set("-123.45");
		assertEquals("-012345", line.getFullLine());
		
		fieldValue.set("1234.56");
		assertEquals("+123456", line.getFullLine());
		fieldValue.set("+1234.56");
		assertEquals("+123456", line.getFullLine());
		fieldValue.set("-1234.56");
		assertEquals("-123456", line.getFullLine());
	}

	public void testSet5() throws IOException {
		chkSet5(getIOBuilder5());
		chkSet5(getIOBuilder6());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSet5(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		fieldValue.set("123.45");
		assertEquals(fieldValue.getTypeName(), "0123.45+", line.getFullLine());
		fieldValue.set("+123.45");
		assertEquals("0123.45+", line.getFullLine());
		fieldValue.set("-123.45");
		assertEquals("0123.45-", line.getFullLine());
		
		fieldValue.set("1234.56");
		assertEquals("1234.56+", line.getFullLine());
		fieldValue.set("+1234.56");
		assertEquals("1234.56+", line.getFullLine());
		fieldValue.set("-1234.56");
		assertEquals("1234.56-", line.getFullLine());
	}
	

	public void testSet7() throws IOException {
		chkSet7(getIOBuilder7());
		chkSet7(getIOBuilder8());
	}

	/**
	 * @param iob
	 * @throws IOException
	 */
	private void chkSet7(ICobolIOBuilder iob) throws IOException {
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		fieldValue.set("123.45");
		if (! "00123.45".equals(line.getFullLine())) {
			assertEquals("+0123.45", line.getFullLine());
		}
		fieldValue.set("+123.45");
		if (! "00123.45".equals(line.getFullLine())) {
			assertEquals("+0123.45", line.getFullLine());
		}
		fieldValue.set("-123.45");
		assertEquals("-0123.45", line.getFullLine());
		
		fieldValue.set("1234.56");
		if (! "01234.56".equals(line.getFullLine())) {
			assertEquals("+1234.56", line.getFullLine());
		}
		fieldValue.set("+1234.56");
		if (! "01234.56".equals(line.getFullLine())) {
			assertEquals("+1234.56", line.getFullLine());
		}
		fieldValue.set("-1234.56");
		assertEquals("-1234.56", line.getFullLine());
	}

	public void testSet9() throws IOException {
		ICobolIOBuilder iob = getIOBuilder9();
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		fieldValue.set("123.45");
		assertEquals("012345", line.getFullLine());
		
		fieldValue.set("1234.56");
		assertEquals("123456", line.getFullLine());

		//System.out.println(fieldValue.asString() + " " + fieldValue.getFieldDetail().getType());
	}

	
	public void testSetGet() throws IOException {
		String[] copybooks = {
				COBOL_COPBOOK1, COBOL_COPBOOK2, COBOL_COPBOOK3, COBOL_COPBOOK4,
				COBOL_COPBOOK5, COBOL_COPBOOK6, COBOL_COPBOOK7, COBOL_COPBOOK8,
		};
		
		for (int i = 0; i < copybooks.length; i++) {
			ICobolIOBuilder iob = JRecordInterface1.COBOL
					.newIOBuilder(new StringReader(copybooks[i]), "Test");
			AbstractLine line = iob.newLine();
			AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
			String id = i+ " " + fieldValue.getTypeName();
			System.out.println(i + " " + copybooks[i]);
			
			getSet(id, fieldValue, "123.45");	
			getSet(id, fieldValue, "1234.56");
			getSet(id, fieldValue, "-123.45");	
			getSet(id, fieldValue, "-1234.56");
		}
	}
	
	public void testSetGet9() throws IOException {
		ICobolIOBuilder iob = getIOBuilder9();
		AbstractLine line = iob.newLine();
		AbstractFieldValue fieldValue = line.getFieldValue(0, 0);
		
		getSet("9: 1", fieldValue, "123.45");	
		getSet("9: 2", fieldValue, "1234.56");
	}
	
	private void getSet(String id, AbstractFieldValue fieldValue, String value) {
		fieldValue.set(value);
		assertEquals(id, value, fieldValue.asString());
	}

	private ICobolIOBuilder getIOBuilder() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK1), "Test");
	}
	
	private ICobolIOBuilder getIOBuilder2() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK2), "Test");
	}

	
	private ICobolIOBuilder getIOBuilder3() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK3), "Test");
	}

	private ICobolIOBuilder getIOBuilder4() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK4), "Test");
	}

	
	private ICobolIOBuilder getIOBuilder5() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK5), "Test");
	}

	private ICobolIOBuilder getIOBuilder6() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK6), "Test");
	}


	private ICobolIOBuilder getIOBuilder7() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK7), "Test");
	}

	private ICobolIOBuilder getIOBuilder8() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK8), "Test");
	}

	
	private ICobolIOBuilder getIOBuilder9() {
		return JRecordInterface1.COBOL
						.newIOBuilder(new StringReader(COBOL_COPBOOK9), "Test");
	}


}
