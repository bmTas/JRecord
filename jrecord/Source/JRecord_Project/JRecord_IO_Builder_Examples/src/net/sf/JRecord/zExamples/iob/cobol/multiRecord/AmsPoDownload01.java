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

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.IRecordDeciderX;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;



/**
 * <b>Purpose:</b> This program demonstrates reading a multi-record cobol file
 * 
 *                   
 * @author Bruce Martin
 *
 */
public class AmsPoDownload01 {

	
	private static final String LOCATION_RECORD = "Location-Record";
	private static final String PRODUCT_RECORD = "Product-Record";
	private static final String PO_RECORD = "PO-Record";
	private static final String copybookFileName = "amsPoDownload.cbl";
	private static final String dataFileName = "Ams_PODownload_20041231.txt";
	
    public static void main(String[] args) throws Exception {
    	new AmsPoDownload01();
    }
    
    
    private AmsPoDownload01() throws Exception {
      	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	String poFile   = this.getClass().getResource(dataFileName).getFile();
    	AbstractLine l;
  
    	IRecordDeciderX recordDecider = JRecordInterface1.RECORD_DECIDER_BUILDER
    					 .singleFieldDeciderBuilder("Record-Type", true)
    					 		.addRecord("H1", PO_RECORD)
    					 		.addRecord("D1", PRODUCT_RECORD)
    					 		.addRecord("S1", LOCATION_RECORD)
    					 .build();
   	
    	ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
    					.setFileOrganization(Constants.IO_BIN_TEXT)
    					.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
    					.setRecordDecider(recordDecider);
    	
    	LayoutDetail layout  = ioBldr	.getLayout();
		AbstractLineReader r = ioBldr	.newReader(poFile);
    	
    	while ((l = r.read()) != null) {
    		String recordName = layout.getRecord(l.getPreferredLayoutIdx()).getRecordName();
   		
    		switch (recordName) {
    		case PO_RECORD: 
       			System.out.println("PO: " 
    					+         l.getFieldValue("PO").asString()
    					+ " "   + l.getFieldValue("Vendor").asString()
    			);
       			break;
    		case PRODUCT_RECORD:
       			System.out.println("\tProduct: " 
    					+       l.getFieldValue("Product").asString()
    					+ "\t" + l.getFieldValue("Product-Name").asString()
       			);
       			break;
    		case LOCATION_RECORD:
      			System.out.println("\t\tLocation: " 
    					+       l.getFieldValue("DC-Number (1)").asString()
    					+ " " + l.getFieldValue("Pack-Quantity (1)").asString()
    					+ "\t" + l.getFieldValue("DC-Number (2)").asString()
    					+ " " + l.getFieldValue("Pack-Quantity (2)").asString()
      			);
    		}
 
    	}
    	r.close();
    }
    
}
