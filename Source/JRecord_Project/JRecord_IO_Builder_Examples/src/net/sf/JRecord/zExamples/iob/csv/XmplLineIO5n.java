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
      
package net.sf.JRecord.zExamples.iob.csv;


import java.math.BigDecimal;
import java.math.RoundingMode;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Example of Reading / writing CSV files <b>with out</b> names on the first line
 * @author Bruce Martin
 *
 */
public final class XmplLineIO5n {
	
    private static final String FLD_PRICE = "Price";
	private static final String FLD_GST   = "GST";
	private static final String FLD_QTY   = "Qty";
	private static final String FLD_DEPT  = "Dept";
	private static final String FLD_DATE  = "Date";
	private static final String FLD_STORE = "Store";
	private static final String FLD_SKU = "Sku";

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

        try {
          // When reading a Csv without names on the first line:
          // 1) use FileStructure = Constants.IO_TEXT_LINE
          // 2) you must define the fields
        	AbstractLineReader reader = JRecordInterface1.CSV
        			.newIOBuilder("\t", "\"")
       					.setFileOrganization(Constants.IO_UNICODE_TEXT)
       					.defineFields()
       						.addCsvField("KEYCODE-NO",  Type.ftChar, 0)
       						.addCsvField("Store-NO",    Type.ftNumAnyDecimal, 0)
       						.addCsvField("Date",        Type.ftNumAnyDecimal, 0)
       						.addCsvField("Dept-NO",     Type.ftNumAnyDecimal, 0)
       						.addCsvField("Qty-Sold",    Type.ftNumAnyDecimal, 0)
       						.addCsvField("Sale-Price",  Type.ftNumAnyDecimal, 0)
						.endOfRecord()
					.newReader(salesFile);
      	  // Define the output record (with the fields)
      	  // without names on the first line:
      	  // 1) use FileStructure = Constants.IO_TEXT_LINE
          // 2) you must define the fields
        	ICsvIOBuilder outIOBlbdr = JRecordInterface1.CSV.newIOBuilder(";", "\"")
       				.setFileOrganization(Constants.IO_UNICODE_TEXT)
					.defineFields()
						.addCsvField(FLD_SKU,   Type.ftChar, 0)
						.addCsvField(FLD_STORE, Type.ftNumAnyDecimal, 0)
						.addCsvField(FLD_DATE,  Type.ftNumAnyDecimal, 0)
						.addCsvField(FLD_DEPT,  Type.ftNumAnyDecimal, 0)
						.addCsvField(FLD_QTY,   Type.ftNumAnyDecimal, 0)
						.addCsvField(FLD_PRICE, Type.ftNumAnyDecimal, 0)
						.addCsvField(FLD_GST,   Type.ftNumAnyDecimal, 0)
					.endOfRecord();

			AbstractLine outCsvRecord = outIOBlbdr.newLine();
			

			AbstractLineWriter writer = outIOBlbdr.newWriter(salesFileOut);
			
			System.out.println("Input  File: " + salesFile);
			System.out.println("Output File: " + salesFileOut);
            
            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                outCsvRecord.getFieldValue(FLD_SKU)  .set(saleRecord.getFieldValue("KEYCODE-NO").asString());
                outCsvRecord.getFieldValue(FLD_STORE).set(saleRecord.getFieldValue("Store-NO")  .asString());
                outCsvRecord.getFieldValue(FLD_DATE) .set(saleRecord.getFieldValue("Date")      .asString());
                outCsvRecord.getFieldValue(FLD_DEPT) .set(saleRecord.getFieldValue("Dept-NO")   .asString());
                outCsvRecord.getFieldValue(FLD_QTY)  .set(saleRecord.getFieldValue("Qty-Sold")  .asString());
  
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
