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
      
package net.sf.JRecord.zExamples.iob.cobol.iobuilder;

import java.io.ByteArrayOutputStream;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * Read / Write Mainframe Cobol file using a 
 * Cobol Copybook - <b>CobolIOBuilder</b> version.
 * 
 * <p><b>Note:</b> The input and output file formats are exactly the same. 
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplReadWritecsv {

    private static final double GST_CONVERSION = 1.1;

    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String dataFile      = installDir + "DTAR020.bin";
//    private String salesFileOut   = installDir + "DTAR020out.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplReadWritecsv() {
        super();

        try {
        	ICobolIOBuilder iob = JRecordInterface1.COBOL
        								.newIOBuilder(copybookName)
        									.setFont("cp037")                                   // US EBCDIC
        									.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH_RECORDS);  
            AbstractLineReader reader = iob.newReader(dataFile);
            ICsvIOBuilder csvOBuilder = JRecordInterface1.CSV.newIOBuilder(",", "\"");
            IDefineCsvFields csvFields = csvOBuilder.defineFields();
            
            AbstractLine line = reader.read();
            if (line != null) {
            	FieldIterator fieldIterator = line.getFieldIterator(0);
            	for (AbstractFieldValue fv : fieldIterator) {
            		csvFields.addCsvField(fv.getFieldDetail().getName(), Type.ftChar, 0);
            	}
            	csvOBuilder = csvFields.endOfRecord();
            	
        		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            	AbstractLineWriter csvWriter = csvOBuilder.newWriter(byteArrayOutputStream);
        		while (line != null) {
        			AbstractLine csvLine = csvOBuilder.newLine();
        			int fldNum = 0;
        			for (AbstractFieldValue fv : fieldIterator) {
        				csvLine.getFieldValue(0, fldNum++).set(fv.asString());
        			}
        			csvWriter.write(csvLine);
           			line = reader.read();
        		}
        		csvWriter.close();
        		System.out.println(byteArrayOutputStream.toString());
            }

            reader.close();
            
        } catch (Exception e) {
            System.out.println("~~> " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
    	new XmplReadWritecsv();
    }
}
