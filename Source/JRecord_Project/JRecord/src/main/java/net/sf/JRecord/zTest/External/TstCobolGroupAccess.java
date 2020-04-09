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
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstCobolGroupAccess extends TestCase {

	private static final String PART_1
						= "          05 Group occurs 10.\n"
						+ "             07 Group-x occurs 10.\n"
						+ "                 10 field-1      pic 999.\n";
	private static final String FIELD_NAME = "field-1 (1, 3)";
	private static final String CPYBOOK1
					= "       02 HDR.\n" 
					+ PART_1;
	private static final String CPYBOOK2
					= "       02 HDR.\n" 
					+  "          05 Group-a.\n"
					+ "                 10 field-1      pic 999.\n"
					+ PART_1;
	
	public void testGroupArray1() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(CPYBOOK1.getBytes()), "Copybook");
		LayoutDetail l = ioBldr.getLayout();
		
		
		check("1a: ", l.getFieldFromName(FIELD_NAME));
		check("2a: ", l.getGroupField("Group", FIELD_NAME));
		check("3a: ", l.getGroupField("Group-x", FIELD_NAME));
		check("4a: ", l.getGroupField("Group", "Group-x", FIELD_NAME));
	}
	
	public void testGroupArray2() throws RecordException, IOException {
		ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(CPYBOOK2.getBytes()), "Copybook");
		LayoutDetail l = ioBldr.getLayout();
				
		check("2b: ", l.getGroupField("Group", FIELD_NAME));
		check("3b: ", l.getGroupField("Group-x", FIELD_NAME));
		check("4b: ", l.getGroupField("Group", "Group-x", FIELD_NAME));
	}
	
	private void check(String id, IFieldDetail fld) {
		assertTrue(id, fld != null);
		assertEquals(id, FIELD_NAME, fld.getName());
		System.out.println(id + fld.getPos() + " " + fld.getLen());
	}
}
