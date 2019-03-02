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
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.ExternalRecord;
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
public class AmsReceipt01 {

	
	private static final String copybookFileName = "amsPoDownload.cbl";
	private static final String dataFileName = "Ams_PODownload_20041231.txt";
	
    public static void main(String[] args) throws Exception {
    	new AmsReceipt01();
    }
    
    
    private AmsReceipt01() throws Exception {
      	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	String poFile   = this.getClass().getResource(dataFileName).getFile();
    	AbstractLine l;
  
    	AbstractLineReader r = JRecordInterface1.COBOL
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
    				.newReader(poFile);
    	
    	while ((l = r.read()) != null) {
    		String id = l.getFieldValue("Record-Type").asString();
    		if ("H1".equals(id)) {
    			System.out.println("PO: " 
    					+         l.getFieldValue("PO").asString()
    					+ " "   + l.getFieldValue("Vendor").asString()
    			);
    		} else if ("D1".equals(id)) {
       			System.out.println("\tProduct: " 
    					+       l.getFieldValue("Product").asString()
    					+ "\t" + l.getFieldValue("Product-Name").asString()
       			);
    		} else if ("S1".equals(id)) {
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
