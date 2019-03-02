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
      
package net.sf.JRecord.zExamples.iob.recordEditorXml.readWrite;

import java.util.HashMap;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.IIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Reading / writing files using a RecordEditor-XML copybook
 *
 * @author Bruce Martin
 *
 */
public final class XmplLineIOC {


	    /**
	     * Example of
	     * 1) Loading an XML copybook (RecordEditor-Xml)
	     * 2) LineReader / LineWrite classes
	     */
	    private XmplLineIOC() {
	        super();

		    String installDir     = TstConstants.SAMPLE_DIRECTORY;
		    String amsPoFile      = installDir + "Ams_PODownload_20041231.txt";
//		    String salesFileOut   = installDir + "DTAR020out.bin";
		    String copybookName   = TstConstants.RE_XML_DIRECTORY
	    					+ "ams PO Download.Xml";
	        int lineNum = 0;

	        AbstractLine amsPoRecord;

            HashMap<String, RecordDetail> recordIdxMap = new HashMap<String, RecordDetail>();
	        String recordType = "";
	        try {
	        	IIOBuilder ioBldr = JRecordInterface1 .SCHEMA_XML.newIOBuilder(copybookName);
				AbstractLineReader reader  = ioBldr.newReader(amsPoFile);
				LayoutDetail schema = ioBldr.getLayout();
	
	            recordIdxMap.put("H1", schema.getRecord("ams PO Download: Detail"));
	            recordIdxMap.put("D1", schema.getRecord("ams PO Download: Header"));
	            recordIdxMap.put("S1", schema.getRecord("ams PO Download: Allocation"));

	            while ((amsPoRecord = reader.read()) != null) {
	            	recordType = amsPoRecord.getFieldValue("Record Type").asString();
	                lineNum += 1;

	                if (recordIdxMap.containsKey(recordType)) {
	                	RecordDetail recordDetail = recordIdxMap.get(recordType);
		                System.out.println("Line " + lineNum + " Record : " + recordType);
		                System.out.println(" " + recordDetail.getRecordName());
		                for (int i = 0; i < recordDetail.getFieldCount(); i++) {
		                	FieldDetail field = recordDetail.getField(i);
							System.out.println(
									  "\t" + field.getName()
									+ "\t\t" + amsPoRecord.getFieldValue(field).asString());
		                }
	                } else {
	                	System.out.println("Invalid Record Type: " + recordType + " at Line Number: " + lineNum
	                			+ amsPoRecord.getFullLine());
	                }

	                System.out.println();
	                System.out.println();
	            }

	            reader.close();
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + recordType + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIOC();
	    }
}
