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

package net.sf.JRecord.zTest.fixedWidth.iobuilder;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.Types.Type;
import junit.framework.TestCase;

public class TstCheckSchema extends TestCase {

	public void test01() throws IOException {
		ExternalRecord xr;
		xr = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037")
					.defineFieldsByLength()
						.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
						.addFieldByLength("Store", Type.ftPackedDecimal, 2, 0)
						.addFieldByLength("Date" , Type.ftPackedDecimal, 4, 0)
						.addFieldByLength("Dept" , Type.ftPackedDecimal, 2, 0)
						.addFieldByLength("Qty"  , Type.ftPackedDecimal, 5, 0)
						.addFieldByLength("Price", Type.ftPackedDecimal, 6, 2)
					.endOfRecord()
					.getExternalRecord();
		
		check(xr, true);
	}

	public void test02() throws IOException {
		ExternalRecord xr;
		xr = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037")
					.defineFieldsByLength()
						.addFieldByLength("Sku"  , Type.ftChar,   8, 0)
						.addFieldByLength("Store", Type.ftPackedDecimal, 2, 0)
//						.addFieldByLength("Date" , Type.ftPackedDecimal, 4, 0)
//						.addFieldByLength("Dept" , Type.ftPackedDecimal, 2, 0)
						.skipBytes(6)
						.addFieldByLength("Qty"  , Type.ftPackedDecimal, 5, 0)
						.addFieldByLength("Price", Type.ftPackedDecimal, 6, 2)
					.endOfRecord()
					.getExternalRecord();
		
		check(xr, false);
	}

	public void test03() throws IOException {
		ExternalRecord xr;
		xr = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037")
					.defineFieldsByPosition()
						.addFieldByPosition("Sku"  , Type.ftChar         ,  1, 0)
						.addFieldByPosition("Store", Type.ftPackedDecimal,  9, 0)
						.addFieldByPosition("Date" , Type.ftPackedDecimal, 11, 0)
						.addFieldByPosition("Dept" , Type.ftPackedDecimal, 15, 0)
						.addFieldByPosition("Qty"  , Type.ftPackedDecimal, 17, 0)
						.addFieldByPosition("Price", Type.ftPackedDecimal, 22, 2)
					.endOfRecord(28)
					.getExternalRecord();
		
		check(xr, true);
	}


	public void test04() throws IOException {
		ExternalRecord xr;
		xr = JRecordInterface1.FIXED_WIDTH.newIOBuilder()
					.setFileOrganization(Constants.IO_FIXED_LENGTH)
					.setFont("CP037")
					.defineFieldsByPosition()
						.addFieldByPosition("Sku"  , Type.ftChar         ,  1, 0)
						.addFieldByPosition("Store", Type.ftPackedDecimal,  9, 0)
//						.addFieldByPosition("Date" , Type.ftPackedDecimal, 11, 0)
//						.addFieldByPosition("Dept" , Type.ftPackedDecimal, 15, 0)
						.skipFieldPosition(11)
						.addFieldByPosition("Qty"  , Type.ftPackedDecimal, 17, 0)
						.addFieldByPosition("Price", Type.ftPackedDecimal, 22, 2)
					.endOfRecord(28)
					.getExternalRecord();
		
		check(xr, false);
	}

	private void check(ExternalRecord xr, boolean all) {
		int pos, idx = 0;
		assertEquals(Constants.IO_FIXED_LENGTH, xr.getFileStructure());
		assertEquals("CP037", xr.getFontName());
		
		pos = check(xr.getRecordField(idx++), 1,   "Sku"  , Type.ftChar,   8, 0);
		pos = check(xr.getRecordField(idx++), pos, "Store", Type.ftPackedDecimal, 2, 0);
		if (all) {
			assertEquals(6, xr.getNumberOfRecordFields());
			pos = check(xr.getRecordField(idx++), pos, "Date" , Type.ftPackedDecimal, 4, 0);
			pos = check(xr.getRecordField(idx++), pos, "Dept" , Type.ftPackedDecimal, 2, 0);
		} else {
			assertEquals(4, xr.getNumberOfRecordFields());
			pos += 6;
		}
		pos = check(xr.getRecordField(idx++), pos, "Qty"  , Type.ftPackedDecimal, 5, 0);
		pos = check(xr.getRecordField(idx++), pos, "Price", Type.ftPackedDecimal, 6, 2);

	}
	
	
	private int check(ExternalField fld, int pos, String n, int type, int len, int decimal) {
		assertEquals(n, fld.getName());
		assertEquals(pos, fld.getPos());
		assertEquals(type, fld.getType());
		assertEquals(decimal, fld.getDecimal());
		assertEquals(len, fld.getLen());
		return pos + len;
	}

}
