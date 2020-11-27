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

package net.sf.JRecord.zTest.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.IO.ContinuousLineReader;
import net.sf.JRecord.IO.ContinuousLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

/**
 * Basic Test for continuous IO (different records)
 * 
 * @author Bruce Martin
 *
 */
public class TstContinuousIO01 extends TestCase {
	
	private static final int DETAIL_COUNT = 15;
	private static final String TRAILER_ID = "T";
	private static final String DETAIL_ID = "D";
	private static final String HEADER_ID = "H";
	private static final String RECORD_TYPE = "Record-Type";

	public boolean lowerCase = false;
	
	public void testRead1() throws Exception {
		tstRead();
	}
	
	public void testRead2() throws Exception {
		lowerCase = true;
		tstRead();
	}

	public void tstRead() throws Exception {

		AbstractLine line;
		List<Line> list = bldLines();
		String s = toString(list);
		
		ContinuousLineReader r = new ContinuousLineReader();
		
		if (lowerCase) {
			s = s.toLowerCase();
		}
		r.open(new ByteArrayInputStream(s.getBytes()), list.get(0).getLayout());
		
		line  = r.read();
		
		iAssertEquals(HEADER_ID, line.getFieldValue(RECORD_TYPE).asString());
		iAssertEquals("20150711", line.getFieldValue("Creation-Date").asString());
		iAssertEquals("1.00", line.getFieldValue("Version").asString());
		iAssertEquals(list.get(0).getFullLine(), line.getFullLine());
		
		for (int i = 1; i < DETAIL_COUNT; i++) {
			line = r.read();
			System.out.println(i +" >" + line.getFullLine() + "<");
			iAssertEquals(list.get(i).getFullLine(), line.getFullLine());
			iAssertEquals(DETAIL_ID, line.getFieldValue(RECORD_TYPE).asString());
			iAssertEquals("Fld_1_" + i, line.getFieldValue("Field-1").asString());
			iAssertEquals("Fld_2_" + i, line.getFieldValue("Field-2").asString());
			iAssertEquals("Fld_3_" + i, line.getFieldValue("Field-3").asString());
		}


		line = r.read();
		iAssertEquals(list.get(list.size() - 1).getFullLine(), line.getFullLine());
		iAssertEquals(TRAILER_ID, line.getFieldValue(RECORD_TYPE).asString());
		assertEquals(list.size(), line.getFieldValue("Record-Count").asInt());
		assertTrue(r.read() == null);

		r.close();
	}
	
	
	private void iAssertEquals(String expected, String actual) {
		if (lowerCase) {
			assertEquals(expected.toLowerCase(), actual);
		} else {
			assertEquals(expected, actual);
		}
	}

	
	public void testWrite() throws Exception {
		List<Line> list = bldLines();
		String expected = toString(list);

		ContinuousLineWriter w = new ContinuousLineWriter();
		
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream(expected.length() + 100);
		w.open(outputStream);
		
		for (Line l : list) {
			w.write(l);
		}
		
		w.close();
		
		assertEquals(expected, outputStream.toString());
	}
	
	
	private String toString(List<Line> lines) {
		StringBuilder b = new StringBuilder();
		
		for (Line l : lines) {
			b.append(l.getFullLine());
		}
		return b.toString();
	}
	
	
	private List<Line> bldLines() throws Exception {
		LayoutDetail l = loadRecordDefinition();
		ArrayList<Line> lines = new ArrayList<Line>();
		Line line = new Line(l);
		
		line.getFieldValue(RECORD_TYPE).set(HEADER_ID);
		line.getFieldValue("Creation-Date").set("20150711");
		line.getFieldValue("Version").set(1);
		lines.add(line);
		
		for (int i = 1; i < DETAIL_COUNT; i++) {
			line = new Line(l);
			line.getFieldValue(RECORD_TYPE).set(DETAIL_ID);
			line.getFieldValue("Field-1").set("Fld_1_" + i);
			line.getFieldValue("Field-2").set("Fld_2_" + i);
			line.getFieldValue("Field-3").set("Fld_3_" + i);

			lines.add(line);
		}
		
		line = new Line(l);
		
		line.getFieldValue(RECORD_TYPE).set(TRAILER_ID);
		line.getFieldValue("Record-Count").set(lines.size() + 1);
		lines.add(line);
		return lines;
				
    /* ------------------------------------------------------------
    
       01 Header-Record.
          05 Record-Type                            Pic X.
             88 Header-Record         value 'H'.
         05 Creation-Date                           Pic 9(8).
         05 Version                                 pic 9(3)V99.
         
       01 Detail-Record.
          05 Rec-Type                               Pic X.
             88 Detail-Record         value 'D'.
          05 Field-1                                Pic X(10).
          05 Field-2                                Pic X(20).
          05 Field-3                                Pic X(10).
              
       01 Trailer-Record.
          05 Rec-Type                               Pic X.
             88 Trailer-Record        value 'T'.
          05 Record-Count                           Pic 9(9).  

    		
       ------------------------------------------------------------ */

	}
	
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private LayoutDetail loadRecordDefinition() throws Exception{
    	
    	String copyName = this.getClass().getResource("MultiRecordTest.cbl").getFile();
 
    	ICobolIOBuilder IOBldr = CobolIoProvider.getInstance()
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
    					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
    					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
    					;
    	
    	ExternalRecord er = IOBldr.getExternalRecord();
    	
    	
    	/*
    	 * When using IO_CONTINOUS_NO_LINE_MARKER you must set either
    	 * * Record Selections for every Record
    	 * * use a record decider class !!
    	 */
    	for (int i = 0; i < er.getNumberOfRecords(); i++) {
    		ExternalRecord rec = er.getRecord(i);
    		String name = rec.getRecordName();
    		
    		if ("Header-Record".equalsIgnoreCase(name)) {
    			rec.setRecordSelection(newFieldSelection(RECORD_TYPE, HEADER_ID));
    		} else if ("Detail-Record".equalsIgnoreCase(name)) {
    			rec.setRecordSelection(newFieldSelection(RECORD_TYPE, DETAIL_ID));
    		} else if ("Trailer-Record".equalsIgnoreCase(name)) {
    			rec.setRecordSelection(newFieldSelection(RECORD_TYPE, TRAILER_ID));
    		}
    	}
    	
    	return er.asLayoutDetail();
    }
    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }

}
