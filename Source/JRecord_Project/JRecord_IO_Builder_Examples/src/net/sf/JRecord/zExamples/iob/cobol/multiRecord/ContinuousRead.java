/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord IOBuilder examples
 *    
 *    Sub-Project purpose: Examples of using JRecord IOBuilders
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.iob.cobol.multiRecord;

import java.io.ByteArrayInputStream;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.ContinuousLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class ContinuousRead {

	private static final int DETAIL_COUNT = 15;
	private static final String TRAILER_ID = "T";
	private static final String DETAIL_ID = "D";
	private static final String HEADER_ID = "H";
	private static final String RECORD_TYPE = "Record-Type";

	
	public static void main(String[] args) throws Exception {
		new ContinuousRead();
	}
	
	public ContinuousRead() throws Exception {
		AbstractLine line;
		List<Line> list = bldLines();
		String fileData = toString(list);
		LayoutDetail layout = list.get(0).getLayout();
		
		ContinuousLineReader r = new ContinuousLineReader();
		
		r.open(new ByteArrayInputStream(fileData.getBytes()), layout);
		
		while ((line = r.read()) != null) {
			String recordType = line.getFieldValue(RECORD_TYPE).asString();
			
			if (HEADER_ID.equals(recordType)) {
				System.out.println("Header Information:");
				System.out.println();
				System.out.println("\tHeader Date: " + line.getFieldValue("Creation-Date").asString());
				System.out.println("\t    Version: " + line.getFieldValue("Version").asString());
				System.out.println();
				System.out.println("\t Field-1\t Field-2\t Field-3");
				System.out.println("\t -------\t -------\t -------");
			} else if (DETAIL_ID.equals(recordType)) {
				System.out.println(
						  "\t " + line.getFieldValue("Field-1").asString().trim()
						+ "\t " + line.getFieldValue("Field-2").asString().trim()
						+ "\t " + line.getFieldValue("Field-3").asString().trim()		
				);
			} else {
				System.out.println();
				System.out.println("Trailer Count: " + line.getFieldValue("Record-Count").asString());
			}
		}
		
		r.close();

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
 
    	ICobolIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
    					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
    					.setFileOrganization(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER)
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
