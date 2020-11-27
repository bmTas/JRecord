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

package net.sf.JRecord.zTest.Types1;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.LineIOProvider;
import junit.framework.TestCase;

/**
 * This class test type updates by reading through a file,
 * applying a value and checking the value against an expected value
 * 
 * @author Bruce Martin
 *
 */
public class TestTypeUpdates extends TestCase {

	public void testTypesStandardCharset() throws Exception {
		tstFile(Conversion.DEFAULT_ASCII_CHARSET);
	}

	public void testTypesCP037() throws Exception {
		tstFile("CP037");
	}
	

	public void testTypesCP273() throws Exception {
		tstFile("CP273");
	}
	

	private void tstFile(String charset) throws Exception {
		TestData td = new TestData(charset);
		AbstractLineReader r = LineIOProvider.getInstance().getLineReader(td.testDataDefinition);
		AbstractLine inLine;
		AbstractLine testLine = new Line(td.testDataDefinition);
		int num = 0;
		
		r.open(TestDataConstants.getTestDataFileName(charset), td.testDataDefinition);
		try {
			while ((inLine = r.read()) != null) {
				int fldLength = inLine.getFieldValue(td.fieldLength).asInt();
				IFieldDetail fld = TestDataConstants.getType(
								1, 
								fldLength, 
								inLine.getFieldValue(td.decimalLength).asInt(), 
								inLine.getFieldValue(td.typeNumber).asInt(), 
								charset);
				AbstractFieldValue fieldValue = testLine.getFieldValue(fld);
				
				num += 1;
				fieldValue.set(inLine.getFieldValue(td.testValue).asString());
				System.out.println(inLine.getFieldValue(td.testResultHex).asString() + " " + fieldValue.asHex() + " " +  fieldValue.asString());
				assertEquals(num + " Type 1: " + fld.getType(), inLine.getFieldValue(td.testResultHex).asString(), fieldValue.asHex());
				
				if (! fieldValue.isBinary()) {
//					assertEquals("Type 2: " + fld.getType(), 
//							inLine.getFieldValue(td.testResult).asString(), testLine.getFullLine().substring(0, fldLength));
				}
			}
		} finally {
			r.close();
		}
	}
}
