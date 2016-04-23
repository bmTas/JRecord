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

package net.sf.JRecord.zTest.iobuilders;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IFixedWidthIOBuilder;
import junit.framework.TestCase;

public class TstFixedBuilder extends TestCase {

	private static final String PRICE = "Price";
	private static final String QTY = "Qty";
	private static final String DEPT = "Dept";
	private static final String STORE = "Store";
	private static final String DATE = "Date";
	private static final String SKU = "Sku";
	
	private final R[] result = {
			new R(SKU,1,8,0,   Type.ftChar, Type.ftChar),
			new R(STORE,9,3,0, Type.ftNumRightJustified, Type.ftNumAnyDecimal),
			new R(DATE,12,6,0, Type.ftNumRightJustified, Type.ftNumAnyDecimal),
			new R(DEPT,18,3,0, Type.ftNumRightJustified, Type.ftNumAnyDecimal),
			new R(QTY,21,2,0,  Type.ftNumRightJustified, Type.ftNumAnyDecimal),
			new R(PRICE,23,6,2,Type.ftNumRightJustified, Type.ftNumAnyDecimal),
	};

	public void testFixed() throws RecordException, IOException {
		
		IFixedWidthIOBuilder iob = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
				.defineFieldsByLength()
					.addFieldByLength(SKU  , Type.ftChar,              8, 0)
					.addFieldByLength(STORE, Type.ftNumRightJustified, 3, 0)
					.addFieldByLength(DATE , Type.ftNumRightJustified, 6, 0)
					.addFieldByLength(DEPT , Type.ftNumRightJustified, 3, 0)
					.addFieldByLength(QTY  , Type.ftNumRightJustified, 2, 0)
					.addFieldByLength(PRICE, Type.ftNumRightJustified, 6, 2)
				.endOfRecord();

		ExternalRecord rec = iob.getExternalRecord();
		LayoutDetail l = iob.getLayout();
		
		for (int i = 0; i < result.length; i++) {
			ExternalField f = rec.getRecordField(i);
			assertEquals(result[i].name, f.getName());
			assertEquals(result[i].pos, f.getPos());
			assertEquals(result[i].len, f.getLen());
			assertEquals(result[i].decimal, f.getDecimal());
			assertEquals(result[i].type, f.getType());
			
			//System.out.println(f.getName().toUpperCase() + "," + f.getPos() + "," + f.getLen() + "," + f.getDecimal() + "," + f.getType());
		}
		
		assertEquals(result.length, rec.getNumberOfRecordFields());
		
		for (int i = 0; i < result.length; i++) {
			IFieldDetail f = l.getField(0, i);
			assertEquals(result[i].name, f.getName());
			assertEquals(result[i].pos, f.getPos());
			assertEquals(result[i].len, f.getLen());
			assertEquals(result[i].decimal, f.getDecimal());
			assertEquals(result[i].type, f.getType());
		}
	}
	
	public void testCsv() throws RecordException, IOException {
		ICsvIOBuilder iob = JRecordInterface1.CSV.newIOBuilder()
				.defineFields()
					.addCsvField("Sku",   Type.ftChar, 0)
					.addCsvField("Store", Type.ftNumAnyDecimal, 0)
					.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
					.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
					.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
					.addCsvField("Price", Type.ftNumAnyDecimal, 0)
				.endOfRecord();

		ExternalRecord rec = iob.getExternalRecord();
		LayoutDetail l = iob.getLayout();
		
		for (int i = 0; i < result.length; i++) {
			ExternalField f = rec.getRecordField(i);
			assertEquals(result[i].name, f.getName());
			assertEquals(i+1, f.getPos());
			//assertEquals(result[i].len, f.getLen());
			assertEquals(0, f.getDecimal());
			assertEquals(result[i].type1, f.getType());
			
			//System.out.println(f.getName().toUpperCase() + "," + f.getPos() + "," + f.getLen() + "," + f.getDecimal() + "," + f.getType());
		}
		
		assertEquals(result.length, rec.getNumberOfRecordFields());

		for (int i = 0; i < result.length; i++) {
			IFieldDetail f = l.getField(0, i);
			assertEquals(result[i].name, f.getName());
			assertEquals(i+1, f.getPos());
			//assertEquals(result[i].len, f.getLen());
			assertEquals(0, f.getDecimal());
			assertEquals(result[i].type1, f.getType());
			
			//System.out.println(f.getName().toUpperCase() + "," + f.getPos() + "," + f.getLen() + "," + f.getDecimal() + "," + f.getType());
		}
	}
	
	private class R {
		final String name;
		final int pos, len, decimal, type, type1;
		
		private R(String name, int pos, int len, int decimal, int type, int type1) {
			super();
			this.name = name;
			this.pos = pos;
			this.len = len;
			this.decimal = decimal;
			this.type = type;
			this.type1 = type1;
		}
		
	}
}
