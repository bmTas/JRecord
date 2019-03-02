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
 * Do basic check of Csv-Readers/Writers with Helper methods
 * 
 * @author Bruce Martin
 *
 */
public final class TstCsvReadWrite  extends TestCase {

	
	    private String salesFile           = TstData.class.getResource("DTAR020s.csv").getFile();

	
	    /**
	     * Example of LineReader / LineWrite classes
	     * @throws RecordException 
	     * @throws IOException 
	     */
	    public void testCsvRead() throws IOException, RecordException {
	    	String[] colNames = {"keycode-no", "Store-No", "Date", "Dept-No", "Qty-Sold", "Sale-Price"};

            tstRead(new FileInputStream(salesFile), colNames);
	    }
	    
	    public void testCsvWrite() throws IOException, RecordException {
	    	String[] colNames = {"Sku", "Store", "Date", "Dept", "Qty", "Price",};
	    	ByteArrayOutputStream out = new ByteArrayOutputStream();
			ExternalRecord csvDef 
				= ExternalRecord
					.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", "\t", "\"")
						.addCsvField("Sku",   Type.ftChar, 0)
						.addCsvField("Store", Type.ftNumAnyDecimal, 0)
						.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
						.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
						.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
						.addCsvField("Price", Type.ftNumAnyDecimal, 0)
					.asExternalRecord();
					; 
			LayoutDetail schema = csvDef.asLayoutDetail();
			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineWriter writer = ioProvider.getLineWriter(schema);
			AbstractLine l = new Line(schema, "");
            
            writer.open(out);
            
            for (int i = 0; i < CommonBits.CSV_FILE_DETAILS.length; i++) {
            	for (int j = 0; j < CommonBits.CSV_FILE_DETAILS[i].length; j++) {
            		l.getFieldValue(0, j).set(CommonBits.CSV_FILE_DETAILS[i][j]);
            	}
            	writer.write(l);
            }
            writer.close();
            
            byte[] b = out.toByteArray(); 
            tstRead(new ByteArrayInputStream(b), colNames);
            CommonBits.compare(new FileInputStream(salesFile), new ByteArrayInputStream(b), true);
	    }

	    
	    private void tstRead(InputStream salesStream, String[] colNames) throws IOException, RecordException {

	    	LayoutDetail schema;
			LayoutDetail inSchema 
				= ExternalRecord	
	    			.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", "\t", "\"") 
	    				.asLayoutDetail();
			
			LineIOProvider ioProvider = LineIOProvider.getInstance();
			AbstractLineReader reader = ioProvider.getLineReader(inSchema);
	        
	        reader.open(salesStream, inSchema);       // Open with a null layout and let the reader create the schema
	        schema = reader.getLayout();
	        
	        assertEquals(colNames.length, schema.getRecord(0).getFieldCount());
	        for (int i = 0; i < colNames.length; i++) {
	        	assertEquals(colNames[i], schema.getRecord(0).getField(i).getName());
	        }
	        
	        CommonBits.compareToExpected(reader, CommonBits.CSV_FILE_DETAILS);
	    }
}
