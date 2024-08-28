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
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
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
public class AmsPoDownload03 {

	
	private static final String copybookFileName = "amsPoDownload.cbl";
	private static final String dataFileName = "Ams_PODownload_20041231.txt";
	
    public static void main(String[] args) throws Exception {
    	new AmsPoDownload03();
    }
    
    
    private AmsPoDownload03() throws Exception {
      	
    	String copyName = this.getClass().getResource(copybookFileName).getFile();
    	String poFile   = this.getClass().getResource(dataFileName).getFile();
    	AbstractLine line;
  
    	ICobolIOBuilder iob = JRecordInterface1.COBOL
    				.newIOBuilder(copyName)
    					.setDialect(ICopybookDialects.FMT_FUJITSU);
		AbstractLineReader r = iob	.newReader(poFile);
		LayoutDetail layout = iob.getLayout();
    	
    	while ((line = r.read()) != null) {
    		String id = line.getFieldValue("Record-Type").asString();
    		if ("H1".equals(id)) {
    			System.out.println("PO: " 
    					+         line.getFieldValue("PO").asString()
    					+ " "   + line.getFieldValue("Vendor").asString()
    			);
    		} else if ("D1".equals(id)) {
       			System.out.println("\tProduct: " 
    					+       line.getFieldValue("Product").asString()
    					+ "\t" + line.getFieldValue("Product-Name").asString()
       			);
    		} else if ("S1".equals(id)) {
       			System.out.print("\t\tLocation: " );
       			

       			IFieldDetail field;
       			int i=0;
       			
       			while((field=layout.getFieldFromName("DC-Number ("+(i++)+")")) != null){
       				System.out.print("\t" + line.getFieldValue(field).asString());
       			}  
       			System.out.print("\t~~~\t");
       			i=0;
      			while((field=layout.getFieldFromName("Pack-Quantity ("+(i++)+")")) != null){
       				System.out.print("\t" + line.getFieldValue(field).asString());
       			}  
      			
       			System.out.println();
    		}
    	}
    	r.close();
    }
    
}
