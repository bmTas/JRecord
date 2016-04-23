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

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.IIOBuilder;

public class TstXmlSchema1 extends TestCase {
	
	private static final FieldDetail[] EXPECTED_DTAR020 = {
		bldType("KEYCODE-NO", 1, 8, 0, Type.ftChar, "CP037"),
		bldType("STORE-NO", 9, 2, 0, Type.ftPackedDecimal, "CP037"),
		bldType("DATE", 11, 4, 0, Type.ftPackedDecimal, "CP037"),
		bldType("DEPT-NO", 15, 2, 0, Type.ftPackedDecimal, "CP037"),
		bldType("QTY-SOLD", 17, 5, 0, Type.ftPackedDecimal, "CP037"),
		bldType("SALE-PRICE", 22, 6, 2, Type.ftPackedDecimal, "CP037"),
	};
	
	private static final FieldDetail[][] EXPECTED_AMS_PO = {
		{
			bldType("Record Type", 1, 2, 0, Type.ftChar, ""),
			bldType("Pack Qty", 3, 9, 4, Type.ftAssumedDecimal, ""),
			bldType("Pack Cost", 12, 13, 4, Type.ftAssumedDecimal, ""),
			bldType("APN", 25, 13, 0, Type.ftNumZeroPadded, ""),
			bldType("Filler", 38, 1, 0, Type.ftChar, ""),
			bldType("Product", 39, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("pmg dtl tech key", 72, 15, 0, Type.ftChar, ""),
			bldType("Case Pack id", 87, 15, 0, Type.ftChar, ""),
			bldType("Product Name", 101, 50, 0, Type.ftChar, ""),
		}, {
			bldType("Record Type", 1, 2, 0, Type.ftChar, ""),
			bldType("Sequence Number", 3, 5, 3, Type.ftAssumedDecimal, ""),
			bldType("Vendor", 8, 10, 0, Type.ftNumZeroPadded, ""),
			bldType("PO", 18, 12, 0, Type.ftAssumedDecimal, ""),
			bldType("Entry Date", 30, 6, 0, Type.ftChar, ""),
			bldType("Filler", 36, 8, 0, Type.ftChar, ""),
			bldType("beg01 code", 44, 2, 0, Type.ftChar, ""),
			bldType("beg02 code", 46, 2, 0, Type.ftChar, ""),
			bldType("Department", 48, 4, 0, Type.ftChar, ""),
			bldType("Expected Reciept Date", 52, 6, 0, Type.ftChar, ""),
			bldType("Cancel by date", 58, 6, 0, Type.ftChar, ""),
			bldType("EDI Type", 68, 1, 0, Type.ftChar, ""),
			bldType("Add Date", 69, 6, 0, Type.ftChar, ""),
			bldType("Filler", 75, 1, 0, Type.ftChar, ""),
			bldType("Department Name", 76, 10, 0, Type.ftChar, ""),
			bldType("Prcoess Type", 86, 1, 0, Type.ftChar, ""),
			bldType("Order Type", 87, 2, 0, Type.ftChar, ""),
		}, {
			bldType("Record Type", 1, 2, 0, Type.ftChar, ""),
			bldType("DC Number 1", 3, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 1", 7, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 2", 15, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 2", 19, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 4", 39, 4, 0, Type.ftChar, ""),
			bldType("Pack Quantity 4", 43, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 5", 51, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 5", 55, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 6", 63, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 6", 67, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 7", 75, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 7", 79, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 8", 87, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 8", 91, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 9", 99, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 9", 103, 8, 0, Type.ftNumZeroPadded, ""),
			bldType("DC Number 10", 111, 4, 0, Type.ftNumZeroPadded, ""),
			bldType("Pack Quantity 10", 115, 8, 0, Type.ftNumZeroPadded, ""),
		}
	};

	
	private static final String[][] EXPECTED_DTAR020_DATA = {
		{"63604808", "20", "40118", "170", "1", "4.87"},
		{"69684558", "20", "40118", "280", "1", "19.00"},
		{"69684558", "20", "40118", "280", "-1", "-19.00"},
		{"69694158", "20", "40118", "280", "1", "5.01"},
		{"62684671", "20", "40118", "685", "1", "69.99"},
		{"62684671", "20", "40118", "685", "-1", "-69.99"},
		{"61664713", "59", "40118", "335", "1", "17.99"},
		{"61664713", "59", "40118", "335", "-1", "-17.99"},
		{"61684613", "59", "40118", "335", "1", "12.99"},
		{"68634752", "59", "40118", "410", "1", "8.99"},
		{"60694698", "59", "40118", "620", "1", "3.99"},
		{"60664659", "59", "40118", "620", "1", "3.99"},
		{"60614487", "59", "40118", "878", "1", "5.95"},
		{"68654655", "166", "40118", "60", "1", "5.08"},
		{"69624033", "166", "40118", "80", "1", "18.19"},
		{"60604100", "166", "40118", "80", "1", "13.30"},
		{"68674560", "166", "40118", "170", "1", "5.99"},
	};
	
	
    String dataDTAR020         = this.getClass().getResource("DTAR020_tst1.bin").getFile();
    String copybookNameDTAR020 = this.getClass().getResource("DTAR020.Xml").getFile();
    String copybookNameAmsPO   = this.getClass().getResource("ams_PO_Download.Xml").getFile();

    public void testDTAR020SchemaLoad() throws RecordException, IOException {
    	IIOBuilder ioBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(copybookNameDTAR020);
    	LayoutDetail l = ioBldr.getLayout();
    	RecordDetail r = l.getRecord(0);
    	
    	for (int i = 0; i < r.getFieldCount(); i++) {
    		FieldDetail field = r.getField(i);
    		FieldDetail ef = EXPECTED_DTAR020[i];
    		
    		assertEquals(ef.getName(), field.getName());
       		assertEquals(ef.getPos(),  field.getPos());
       		assertEquals(ef.getLen(),  field.getLen());
       		assertEquals(ef.getDecimal(), field.getDecimal());
       		assertEquals(ef.getType(), field.getType());
//			System.out.println("\tbldType(\"" + field.getName() 
//					+ "\", " + field.getPos()
//					+ ", "   + field.getLen()
//					+ ", "   + field.getDecimal()
//					+ ", "   + field.getType()
//					+ ", \"" + l.getFontName() 
//					+ "\"),"
//			);
    	}
    	assertEquals(EXPECTED_DTAR020.length, r.getFieldCount());
    	assertEquals("CP037", l.getFontName());
    	assertEquals(1, l.getRecordCount());
    	assertEquals(27, l.getMaximumRecordLength());
    	assertEquals(Constants.IO_FIXED_LENGTH, l.getFileStructure());
    	assertEquals(Constants.rtRecordLayout, l.getLayoutType());
    }
    
    public void testDTAR020Read() throws RecordException, IOException {
    	 AbstractLineReader reader = JRecordInterface1.SCHEMA_XML.newIOBuilder(copybookNameDTAR020)
    	 					.newReader(dataDTAR020);
    	 AbstractLine l;
    	 
    	 int i = 0;
    	 while ((l = reader.read()) != null) {
    		 FieldIterator fieldIterator = l.getFieldIterator(0);
    		 int j = 0;
    		 for (AbstractFieldValue fv : fieldIterator) {
    			assertEquals(i + ", " + j, EXPECTED_DTAR020_DATA[i][j++], fv.asString()); 
    		 }
    		 
    		 i += 1;
    	 }
    	 reader.close();
    }
    
    
    public void testAmsPOSchemaLoad() throws RecordException, IOException {
    	IIOBuilder ioBldr = JRecordInterface1.SCHEMA_XML.newIOBuilder(copybookNameAmsPO);
    	LayoutDetail l = ioBldr.getLayout();
    	
    	for (int j = 0; j < l.getRecordCount(); j++) {
	    	RecordDetail r = l.getRecord(j);
	    	
	    	System.out.println("\t}, {");
	    	for (int i = 0; i < r.getFieldCount(); i++) {
	    		FieldDetail field = r.getField(i);
	    		FieldDetail ef = EXPECTED_AMS_PO[j][i];
	    		
	    		assertEquals(ef.getName(), field.getName());
	       		assertEquals(ef.getPos(),  field.getPos());
	       		assertEquals(ef.getLen(),  field.getLen());
	       		assertEquals(ef.getDecimal(), field.getDecimal());
	       		assertEquals(ef.getType(), field.getType());
		
	
//				System.out.println("\t\tbldType(\"" + field.getName() 
//						+ "\", " + field.getPos()
//						+ ", "   + field.getLen()
//						+ ", "   + field.getDecimal()
//						+ ", "   + field.getType()
//						+ ", \"" + l.getFontName() 
//						+ "\"),"
//				);
	    	}
    	}
    	
    }
    
	private static FieldDetail bldType(String name, int pos, int len, int decimal, int type, String font) {
		FieldDetail fd = new FieldDetail(name, "", type, decimal, font, 0, "");
		fd.setPosLen(pos, len);
		
		return fd;
	}

}
