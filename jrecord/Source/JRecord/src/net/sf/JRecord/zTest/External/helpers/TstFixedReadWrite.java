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

package net.sf.JRecord.zTest.External.helpers;


import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import junit.framework.TestCase;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.TstData;

/**
 * Do basic check of Fixed-Readers/Writers with Helper methods
 * 
 * @author Bruce Martin
 *
 */
public final class TstFixedReadWrite  extends TestCase {

	
	    private String salesFile           = TstData.class.getResource("DTAR020s.txt").getFile();

	
	    /**
	     * Example of LineReader / LineWrite classes
	     * @throws RecordException 
	     * @throws IOException 
	     */
	    public void testFixedReadByLength() throws IOException, RecordException {
           tstRead(new FileInputStream(salesFile), getSchemaByLength());
           tstRead(new FileInputStream(salesFile), getSchemaByPos());
           tstRead(new FileInputStream(salesFile), getSchemaByPosLength());
	    }
	    
	    public void testWrite() throws IOException, RecordException {
	    	tstWrite(getSchemaByLength());
	    	tstWrite(getSchemaByPos());
	    	tstWrite(getSchemaByPosLength());
	    }
	    
	    private void tstWrite(LayoutDetail schema) throws IOException, RecordException {
	    	ByteArrayOutputStream out = new ByteArrayOutputStream();
	    	
			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineWriter writer = ioProvider.getLineWriter(schema);
			AbstractLine l = new Line(schema);
            
            writer.open(out);
            
            for (int i = 0; i < CommonBits.CSV_FILE_DETAILS.length; i++) {
            	for (int j = 0; j < CommonBits.CSV_FILE_DETAILS[i].length; j++) {
            		l.getFieldValue(0, j).set(CommonBits.CSV_FILE_DETAILS[i][j]);
            	}
            	writer.write(l);
            }
            writer.close();
            
            byte[] b = out.toByteArray(); 
            tstRead(new ByteArrayInputStream(b), schema);
            CommonBits.compare(new FileInputStream(salesFile), new ByteArrayInputStream(b), true);
	    }

 /*
	    public void testDataCreate() throws IOException, RecordException {
//	    	ByteArrayOutputStream out = new ByteArrayOutputStream();
	    	
	    	FileOutputStream out = new FileOutputStream(TstConstants.TEMP_DIRECTORY + "DTAR020s.txt");
	    	LayoutDetail schema = getSchemaByLength();
			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineWriter writer = ioProvider.getLineWriter(schema);
			AbstractLine l = new Line(schema);
            
            writer.open(out);
            
            for (int i = 0; i < CommonBits.CSV_FILE_DETAILS.length; i++) {
            	for (int j = 0; j < CommonBits.CSV_FILE_DETAILS[i].length; j++) {
            		l.getFieldValue(0, j).set(CommonBits.CSV_FILE_DETAILS[i][j]);
            	}
            	writer.write(l);
            }
            writer.close();
	    }
*/
	    
	    
	    
	    private void tstRead(InputStream salesStream, LayoutDetail inSchema) throws IOException, RecordException {

			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineReader reader = ioProvider.getLineReader(inSchema);
	        
	        reader.open(salesStream, inSchema);       // Open with a null layout and let the reader create the schema
	        	        
	        CommonBits.compareToExpected(reader, CommonBits.CSV_FILE_DETAILS);
	    }
	    
	    private LayoutDetail getSchemaByLength() throws RecordException {
	    	return 	ExternalRecord
	    				.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
							.addFieldByLength("Sku"  , Type.ftChar,              8, 0)
							.addFieldByLength("Store", Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Date" , Type.ftNumRightJustified, 6, 0)
							.addFieldByLength("Dept" , Type.ftNumRightJustified, 3, 0)
							.addFieldByLength("Qty"  , Type.ftNumRightJustified, 2, 0)
							.addFieldByLength("Price", Type.ftNumRightJustified, 6, 2)
						.asLayoutDetail();
	    }
	    
	    private LayoutDetail getSchemaByPos() throws RecordException {
	    	return  ExternalRecord
	    				.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
							.addFieldByPosition("Sku"  , Type.ftChar             ,  1, 0)
							.addFieldByPosition("Store", Type.ftNumRightJustified,  9, 0)
							.addFieldByPosition("Date" , Type.ftNumRightJustified, 12, 0)
							.addFieldByPosition("Dept" , Type.ftNumRightJustified, 18, 0)
							.addFieldByPosition("Qty"  , Type.ftNumRightJustified, 21, 0)
							.addFieldByPosition("Price", Type.ftNumRightJustified, 23, 6, 2)
						.asLayoutDetail();

	    }
	    
	    private LayoutDetail getSchemaByPosLength() throws RecordException {
	    	return  ExternalRecord
    					.newFixedWidthRecord("My_Record", Constants.IO_TEXT_LINE, "")
							.addField("Sku"  , Type.ftChar             ,  1, 8, 0)
							.addField("Store", Type.ftNumRightJustified,  9, 3, 0)
							.addField("Date" , Type.ftNumRightJustified, 12, 6, 0)
							.addField("Dept" , Type.ftNumRightJustified, 18, 3, 0)
							.addField("Qty"  , Type.ftNumRightJustified, 21, 2, 0)
							.addField("Price", Type.ftNumRightJustified, 23, 6, 2)
						.asLayoutDetail();
	    }
}
