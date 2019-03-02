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
      
package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Read / Write Mainframe Cobol file using a Cobol Copybook
 *
 * @author Bruce Martin
 *
 */
public final class XmplLineIOB {

	    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
	    private String salesFile      = installDir + "DTAR020.bin";
	    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

	    /**
	     * Example of LineReader / LineWrite classes
	     */
	    private XmplLineIOB() {
	        super();

	        int lineNum = 0;
	        AbstractLine saleRecord;
	        ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
	        		.newIOBuilder(copybookName)
	        			.setDialect( ICopybookDialects.FMT_MAINFRAME)
	        			.setFont("cp037")
	        			.setFileOrganization(Constants.IO_FIXED_LENGTH)
	        			.setDropCopybookNameFromFields(true);

	        try {
	        	AbstractLineReader reader  = ioBldr.newReader(salesFile);
	            LayoutDetail schema = reader.getLayout();
	            int recordId = 0; // Assuming only one record In the file


	            while ((saleRecord = reader.read()) != null) {
	                lineNum += 1;

	                System.out.println("Line " + lineNum + " Record : " + schema.getRecord(recordId).getRecordName());
	                for (int i = 0; i < schema.getRecord(recordId).getFieldCount(); i++) {
	                	FieldDetail field = schema.getRecord(recordId).getField(i);
						System.out.println(
								  "\t" + field.getName()
								+ "\t\t" + saleRecord.getFieldValue(field).asString());
	                }

	                System.out.println();
	                System.out.println();
	            }

	            reader.close();
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIOB();
	    }
}
