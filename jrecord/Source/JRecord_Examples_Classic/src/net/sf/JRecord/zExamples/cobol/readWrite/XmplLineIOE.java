/*
 * @Author Bruce Martin
 * Created on 9/09/2005
 *
 * Purpose:
 *   This Example program demonstrates Reading a file using Line Based
 * Routines
 *
 * Requirements:
 *
 * 1) Check the values in Constants.java are correct !!!
 *
 */
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
      
package net.sf.JRecord.zExamples.cobol.readWrite;


import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;

/**
 * This Example program demonstrates Reading a file using Line Based
 * Routines
 *
 * @author Bruce Martin
 *
 */
public final class XmplLineIOE {

    /**
     *
     */
    private XmplLineIOE() {
        super();

        String vendorFile          = TstConstants.SAMPLE_DIRECTORY
		                           + "Ams_VendorDownload_20041229.txt";
        String copybookName        = TstConstants.COBOL_DIRECTORY
		                           + "AmsVendor.cbl";
        CobolIoProvider ioProvider = CobolIoProvider.getInstance();
        AbstractLine line;
        int lineNumber = 0;

        try {
        	AbstractLineReader reader  = ioProvider.getLineReader(
                    Constants.IO_TEXT_LINE, ICopybookDialects.FMT_INTEL,
                    CopybookLoader.SPLIT_NONE, copybookName, vendorFile
            );

            System.out.println("  Vendor \t Name");
            System.out.println("  ===========================================");

            while ((line = reader.read()) != null) {
            	for (AbstractFieldValue v : line.getFieldIterator(0)) {
            		System.out.println(v.getFieldDetail().getName()
            				+ "\t\t" + v.getTypeName()
            				+ "\t\t" + v.asString());
            	}

            	System.out.println();
//                lineNumber += 1;
//                    System.out.println(
//                            "  "    + line.getFieldValue("Vendor-Number").asString()
//                          + "  \t " + line.getFieldValue("Vendor-Name").asString());

            }

            reader.close();

        } catch (Exception e) {
            System.out.println("Error Line " + lineNumber + " " + e.getMessage());
            System.out.println("      File " + vendorFile);
            System.out.println();
            e.printStackTrace();
        }

        System.out.println();
        System.out.println("Lines Read " + lineNumber);
    }


    /**
     * LineIO example
     *
     * @param args program arguments
     */
    public static void main(String[] args) {
        new XmplLineIOE();
    }
}
