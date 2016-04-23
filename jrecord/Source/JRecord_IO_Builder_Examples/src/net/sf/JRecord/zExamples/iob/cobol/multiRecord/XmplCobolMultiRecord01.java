/**
 * 
 */
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
import java.io.InputStream;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

/**
 * This program is an example of processing a Cobol file with 
 * multiple (header, detail trailer) Records with the layout defined 
 * using a redefines
 * 
 * @author Bruce Martin
 *
 */
public class XmplCobolMultiRecord01 {

	public static String[] cobolCopybook = {
		"        01  Header-Record.\n",
		"            05  Record-Type              pic x.\n",
		"                88 Header-Record  value 'H'.\n",
		"                88 Detail-Record  value 'D'.\n",
		"                88 Detail-Record  value 'T'.\n",
		"            05  Filler                   pic x.\n",
		"            05  Run-Number               pic 9(5).\n",
		"            05  Filler                   pic x.\n",
		"            05  Run-Date                 pic 9(8).\n",
		"            05  Filler                   pic x(34).\n",
		
		"        01  Detail-Record.\n",
		"            05  Record-Type              pic x.\n",
		"            05  field1                   pic x(3).\n",
		"            05  field2                   pic x(12).\n",
		"            05  field3                   pic x(10).\n",
		"            05  Filler                   pic x(14).\n",

		"        01  Trailer-Record.\n",
		"            05  Record-Type              pic x.\n",
		"            05  record-count             pic 9(8).\n",
		"            05  Filler                   pic x(31).\n",	};
	
	
	public static String[] fileData = {
		"H 00012 20100101\n",
		"D11 12          13        \n",
		"D21 222         233       \n",
		"D31 3222        3333      \n",
		"D41142222       43333     \n",
		"T00000006\n",	
	};
	
	
	private final static String  HEADER_RECORD = "H";
	private final static String  DETAIL_RECORD = "D";
	private final static String  TRAILER_RECORD = "T";
	
	
	private XmplCobolMultiRecord01() throws Exception{
		
		String recordType;
		InputStream inputFile = array2stream(fileData);
		AbstractLineReader reader = getIOBuilder().newReader(inputFile);
		AbstractLine l;

		
		while ((l = reader.read()) != null) {
			recordType = l.getFieldValue("Record-Type").asString();
			if (HEADER_RECORD.equals(recordType)) {
				System.out.println("Run   Date:" + l.getFieldValue("Run-Date").asString());
				System.out.println("Run Number:" + l.getFieldValue("Run-Number").asInt());
				System.out.println();
				System.out.println("Field 1\tField 2\tField 3");
				System.out.println("-------\t-------\t-------");
				System.out.println();
			} else if (DETAIL_RECORD.equals(recordType)) {
				System.out.println(
						  l.getFieldValue("field1").asString().trim() + "\t"
						+ l.getFieldValue("field2").asString().trim() + "\t"
						+ l.getFieldValue("field3").asString().trim() + "\t"
				);
			} else if (TRAILER_RECORD.equals(recordType)) {
				System.out.println();
				System.out.println("Record Count:" + l.getFieldValue("record-count").asInt());
			}
		}
		
		reader.close();
	}
	
	
	private ICobolIOBuilder getIOBuilder() {
		return JRecordInterface1.COBOL
							.newIOBuilder(array2stream(cobolCopybook), "CblRecord")
								.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
								.setDialect(ICopybookDialects.FMT_INTEL);
	}
//	private LayoutDetail getLayout() throws RecordException, Exception {		
//		return getExternalLayout().asLayoutDetail();
//	}
//	
//	private ExternalRecord getExternalLayout() throws Exception {
//		ByteArrayInputStream bs = array2stream(cobolCopybook);
//		
//		return  (new CobolCopybookLoader())
//					.loadCopyBook(
//							bs, "RedefTest", CopybookLoader.SPLIT_01_LEVEL, 0, "",
//							Cb2xmlConstants.USE_STANDARD_COLUMNS, ICopybookDialects.FMT_INTEL, 0, new TextLog());
//	}
	
	
	private ByteArrayInputStream array2stream(String[] array) {
		StringBuilder b = new StringBuilder();
		
		for (int i = 0; i < array.length; i++) {
			b.append(array[i]);
		}
		
		System.out.println(b.toString());
		return new ByteArrayInputStream(b.toString().getBytes());
	}
	/**
	 * @param args
	 */
	public static void main(String[] args)  throws Exception {
		new XmplCobolMultiRecord01();
	}

}
