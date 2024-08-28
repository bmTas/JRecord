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
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

public class ContinuousRead5 {

	private static final int DETAIL_COUNT = 15;
	private static final String TRAILER_ID = "T";
	private static final String DETAIL_ID = "D";
	private static final String REC_A_ID  = "A";
	private static final String REC_B_ID  = "B";
	private static final String HEADER_ID = "H";
	private static final String RECORD_TYPE = "Record-Type";

	
	public static void main(String[] args) throws Exception {
		new ContinuousRead5();
	}
	
	ICobolIOBuilder ioBldr = loadRecordDefinition();

	public ContinuousRead5() throws Exception {
		AbstractLine line;
		List<Line> list = bldLines();
		String fileData = toString(list);
		
		AbstractLineReader r = ioBldr.newReader(new ByteArrayInputStream(fileData.getBytes()));
		
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
			} else if (REC_A_ID.equals(recordType)) {
				System.out.println(
						  "AA\t " + line.getFieldValue("Field-1a").asString().trim()
						+ "\t " + line.getFieldValue("Field-2a").asString().trim()
						+ "\t " + line.getFieldValue("Field-3a").asString().trim()		
						+ "\t " + line.getFieldValue("Field-4a").asString().trim()		
				);
			} else if (REC_B_ID.equals(recordType)) {
				System.out.println(
						  "BB\t " + line.getFieldValue("Field-1b").asString().trim()
						+ "\t " + line.getFieldValue("Field-2b").asString().trim()
						+ "\t " + line.getFieldValue("Field-3b").asString().trim()		
						+ "\t " + line.getFieldValue("Field-4b").asString().trim()		
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
		LayoutDetail l = ioBldr.getLayout();
		ArrayList<Line> lines = new ArrayList<Line>();
		Line line = new Line(l);
		Line lineA = new Line(l);
		Line lineB = new Line(l);
		
		line.getFieldValue(RECORD_TYPE).set(HEADER_ID);
		line.getFieldValue("Creation-Date").set("20150711");
		line.getFieldValue("Version").set(1);
		lines.add(line);
		
		for (int i = 1; i < DETAIL_COUNT; i++) {
			line = new Line(l);
			lineA = new Line(l);
			lineB = new Line(l);
			line.getFieldValue(RECORD_TYPE).set(DETAIL_ID);
			lineA.getFieldValue(RECORD_TYPE).set(REC_A_ID);
			lineB.getFieldValue(RECORD_TYPE).set(REC_B_ID);
			
			for (int j = 1; j < 4; j++) {
				line.getFieldValue("Field-" + j).set("Fld_" + j + "_" + i);
				lineA.getFieldValue("Field-" + j + "A").set("Fld_" + j + "a_" + i);
				lineB.getFieldValue("Field-" + j + "B").set("Fld_" + j + "b_" + i);
			}
			lineA.getFieldValue("Field-" + 4 + "A").set("Fld_" + 4 + "a_z" + i);
			lineB.getFieldValue("Field-" + 4 + "B").set("Fld_" + 4 + "b_z" + i);

			System.out.println(lineA.getFullLine());
			System.out.println(lineB.getFullLine());

			lines.add(line);
			lines.add(lineA);
			lines.add(lineB);

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
    private ICobolIOBuilder loadRecordDefinition() throws Exception{
    	
    	String copyFileName1 = this.getClass().getResource("RecordA.cbl").getFile();
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest.cbl").getFile();
    	String copyFileName3 = this.getClass().getResource("RecordB.cbl").getFile();
 
    	ICobolMultiCopybookIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newMultiCopybookIOBuilder("MultiRecordTest")
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
     					.setFileOrganization(IFileStructureConstants.IO_CONTINOUS_NO_LINE_MARKER)
     					.addCopyBook(copyFileName1)
     						.setRecordSelectionCurrentCopybook(newFieldSelection(RECORD_TYPE, REC_A_ID))
     					.addCopyBook(copyFileName2)
     						.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
     					.addCopyBook(copyFileName3)
      						.setRecordSelectionCurrentCopybook(newFieldSelection(RECORD_TYPE, REC_B_ID))
 					;
    	
    	// For IO_CONTINOUS you must either
    	// * define record selection criteria as below
    	// * or define a RecordDecider
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(RECORD_TYPE, HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(RECORD_TYPE, DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(RECORD_TYPE, TRAILER_ID));
    	
    	
//      You can also use and's / or's: 
//    	
//    	IOBldr.setRecordSelection(
//    			"Trailer-Record",
//    			ExternalGroupSelection.newOr(
//    					new ExternalFieldSelection("Record-Type", "D"),
//    					new ExternalFieldSelection("Record-Type", "E"),
//    					new ExternalFieldSelection("Record-Type", "F")
//    	));
    	
    	return IOBldr;
    }
    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
}
