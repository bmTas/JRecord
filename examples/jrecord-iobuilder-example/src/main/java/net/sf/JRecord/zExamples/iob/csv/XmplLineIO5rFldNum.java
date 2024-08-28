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


import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;

/**
 * Example of Reading a CSV files with names on the first line
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIO5rFldNum {


	    private String salesFile           = this.getClass().getResource("DTAR020.csv").getFile();
	
	    /**
	     * Example of LineReader / LineWrite classes
	     */
	    private XmplLineIO5rFldNum() {
	        super();

	        int lineNum = 0;
	        AbstractLine saleRecord;

	        try {
	        	AbstractLineReader reader = JRecordInterface1.CSV
	        			.newIOBuilder("\t", "\"")
	        					.newReader(salesFile);
				
	            while ((saleRecord = reader.read()) != null) {
	                lineNum += 1;
	                System.out.println(
	                		         saleRecord.getFieldValue(0, 0).asString()
	                		+ "\t" + saleRecord.getFieldValue(0, 1).asString()
	                		+ "\t" + saleRecord.getFieldValue(0, 4).asString()
	                		+ "\t" + saleRecord.getFieldValue(0, 5).asString()
	                		);
	            }

	            reader.close();
	        } catch (Exception e) {
	            System.out.println("~~> " + lineNum + " " + e.getMessage());
	            System.out.println();

	            e.printStackTrace();
	        }
	    }

	    public static void main(String[] args) {
	    	new XmplLineIO5rFldNum();
	    }
}
