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

package net.sf.JRecord.zTest.External;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstSignTrailing extends TestCase {
	private static final String CPYBOOK1
				= "         05 group.\n"
				+ "            07 num1               pic 9(4)-.\n"
				+ "            07 num2               pic 999.99-.\n"
				+ "            07 num3               pic 9(5).99-.\n";
	
	public void test1() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(CPYBOOK1.getBytes()), "Copybook");
		ExternalRecord rec = ioBldr.getExternalRecord();
		LayoutDetail schema = rec.asLayoutDetail();
		AbstractLine line = new Line(schema);
		
		assertEquals(Type.ftSignSeparateTrail, rec.getRecordField(0).getType());
		for (int i = 1 ; i < rec.getNumberOfRecordFields(); i++) {
			assertEquals(Type.ftSignSepTrailActualDecimal, rec.getRecordField(i).getType());
//			System.out.println(rec.getRecordField(i).getName() + "\t" + rec.getRecordField(i).getType() + " " + Type.ftSignSeparateTrail
//					+ " " + TypeManager.isNumeric(Type.ftSignSeparateTrail));
		}
		
		RecordDetail r = schema.getRecord(0);
		assertEquals(Type.ftSignSeparateTrail, r.getField(0).getType());
		for (int i = 1; i < r.getFieldCount(); i++) {
			assertEquals(Type.ftSignSepTrailActualDecimal, r.getField(i).getType());
		}
		
		line.getFieldValue(0,0).set(-100);
		line.getFieldValue(0,1).set(-121);
		line.getFieldValue(0,2).set(-131);
		assertTrue(line.getFieldValue(0,0).isNumeric());
		assertTrue(line.getFieldValue(0,1).isNumeric());
		assertTrue(line.getFieldValue(0,2).isNumeric());
		System.out.println(line.getFullLine());
		
		assertEquals("0100-121.00-00131.00-", line.getFullLine());
		line.getFieldValue(0,0).set(100);
		line.getFieldValue(0,1).set(121);
		line.getFieldValue(0,2).set(131);

		System.out.println(line.getFullLine());
		
		assertEquals("0100+121.00+00131.00+", line.getFullLine());
	}
}
