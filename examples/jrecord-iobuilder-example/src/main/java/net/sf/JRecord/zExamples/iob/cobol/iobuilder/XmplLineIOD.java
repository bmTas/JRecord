/*
 * @Author Bruce Martin
 * Created on 10/09/2005
 *
 * Purpose: Provide examples of
 *
 *    LineProvider,  LineIOProvider, AbsLineReader -etc
 *
 * Requirements:
 *
 * 1) Check the values in Constants.java are correct !!!
 *
 */
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

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;


/**
 *
 * Purpose: Provide sample program to use in Example bat / shell scripts
 *
 *
 * @author Bruce Martin
 * Created on 10/09/2005
 */
public final class XmplLineIOD {

    private String salesFile        = "DTAR020.bin";
    private String copybookName     = "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIOD() {
        super();

        int lineNum = 0;
        AbstractLine saleRecord;

        try {
        	ICobolIOBuilder iob = CobolIoProvider.getInstance()
					.newIOBuilder(copybookName)
						.setFont("cp037")                                   // US EBCDIC
						.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH_RECORDS);  
        	AbstractLineReader reader = iob.newReader(salesFile);

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                System.out.println(saleRecord.getFieldValue("DTAR020-KEYCODE-NO").asString()
                        + " " + saleRecord.getFieldValue("DTAR020-QTY-SOLD").asString()
                        + " " + saleRecord.getFieldValue("DTAR020-SALE-PRICE").asString());
            }

            reader.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }


    /**
     * LineIO example 2
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        new XmplLineIOD();
    }
}
