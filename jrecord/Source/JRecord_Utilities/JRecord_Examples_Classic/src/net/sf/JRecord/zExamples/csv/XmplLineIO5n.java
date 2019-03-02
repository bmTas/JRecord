/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
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
      
package net.sf.JRecord.zExamples.csv;


import java.math.BigDecimal;
import java.math.RoundingMode;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Example of Reading / writing CSV files with out names on the first line
 * @author Bruce Martin
 *
 */
public final class XmplLineIO5n {

	    private static final BigDecimal GST_CONVERSION = new BigDecimal("1.1");

	    private String salesFile           = this.getClass().getResource("DTAR020n.csv").getFile();
	    private String salesFileOut        = TstConstants.TEMP_DIRECTORY + "DTAR020n_out1.csv";

	
	    /**
	     * Example of LineReader / LineWrite classes
	     */
	    private XmplLineIO5n() {
	        super();

	        int lineNum = 0;
	        BigDecimal gstExclusive, price, gst;
	        AbstractLine saleRecord;
	        
	          // When reading a Csv without names on the first line:
	          // 1) use FileStructure = Constants.IO_TEXT_LINE
	          // 2) you must define the fields
	    	ExternalRecord inputCsvRecordDefinition 
	    		= ExternalRecord	
	    			.newCsvRecord("", Constants.IO_TEXT_LINE, "", "\t", "\"")
						.addCsvField("KEYCODE-NO",  Type.ftChar, 0)
						.addCsvField("Store-NO",    Type.ftNumAnyDecimal, 0)
						.addCsvField("Date",        Type.ftNumAnyDecimal, 0)
						.addCsvField("Dept-NO",     Type.ftNumAnyDecimal, 0)
						.addCsvField("Qty-Sold",    Type.ftNumAnyDecimal, 0)
						.addCsvField("Sale-Price",  Type.ftNumAnyDecimal, 0)
					.asExternalRecord();
	    	
	    	  // Define the output record (with the fields)
	    	  // without names on the first line:
	    	  // 1) use FileStructure = Constants.IO_TEXT_LINE
	          // 2) you must define the fields
			ExternalRecord outputCsvRecordDefinition 
				= ExternalRecord
					.newCsvRecord("", Constants.IO_TEXT_LINE, "", "|", "\"")
						.addCsvField("Sku",   Type.ftChar, 0)
						.addCsvField("Store", Type.ftNumAnyDecimal, 0)
						.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
						.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
						.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
						.addCsvField("Price", Type.ftNumAnyDecimal, 0)
						.addCsvField("GST",   Type.ftNumAnyDecimal, 0)
					.asExternalRecord(); 

	        try {
				LayoutDetail inSchema  = inputCsvRecordDefinition .asLayoutDetail();
				LayoutDetail outSchema = outputCsvRecordDefinition.asLayoutDetail();

				AbstractLine outCsvRecord = new Line(outSchema);
				
				LineIOProvider ioProvider = LineIOProvider.getInstance();
				AbstractLineReader reader = ioProvider.getLineReader(inSchema);
				AbstractLineWriter writer = ioProvider.getLineWriter(outSchema);
				
				System.out.println("Input  File: " + salesFile);
				System.out.println("Output File: " + salesFileOut);
	            
	            reader.open(salesFile, inSchema);       // Open with a null layout and let the reader create the schema
	            writer.open(salesFileOut);

	            while ((saleRecord = reader.read()) != null) {
	                lineNum += 1;

	                outCsvRecord.getFieldValue("Sku")  .set(saleRecord.getFieldValue("KEYCODE-NO").asString());
	                outCsvRecord.getFieldValue("Store").set(saleRecord.getFieldValue("Store-NO").asString());
	                outCsvRecord.getFieldValue("Date") .set(saleRecord.getFieldValue("Date").asString());
	                outCsvRecord.getFieldValue("Dept") .set(saleRecord.getFieldValue("Dept-NO").asString());
	                outCsvRecord.getFieldValue("Qty")  .set(saleRecord.getFieldValue("Qty-Sold").asString());
	                outCsvRecord.getFieldValue("GST")  .set(saleRecord.getFieldValue("KEYCODE-NO").asString());

	                price =  saleRecord.getFieldValue("SALE-PRICE").asBigDecimal();
	                gstExclusive = price.divide(GST_CONVERSION, 2, RoundingMode.HALF_DOWN);
	                gst = price.subtract(gstExclusive);
	                
	                outCsvRecord.getFieldValue("Price").set(gstExclusive);
	                outCsvRecord.getFieldValue("GST")  .set(gst);
	        
	                writer.write(outCsvRecord);
	            }

	            reader.close();
	            writer.close();
	 
	            System.out.println("Processed " + lineNum + " Records");
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIO5n();
	    }
}
