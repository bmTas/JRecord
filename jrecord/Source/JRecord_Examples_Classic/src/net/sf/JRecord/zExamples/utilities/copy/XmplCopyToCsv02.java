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
      
package net.sf.JRecord.zExamples.utilities.copy;

import java.io.BufferedWriter;
import java.io.FileWriter;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.FieldIterator;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zTest.Common.TstConstants;
import net.sf.cb2xml.def.Cb2xmlConstants;

/**
 * This example shows how to write a generic utility
 * and access the fields using a field iterator.
 * It assumes there is only <b>one record</b> in the copybook 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplCopyToCsv02 {

    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = installDir + "DTAR020out.csv";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplCopyToCsv02() {
        super();

        int lineNum = 0;

        AbstractLine saleRecord;
        System.out.println(" Input File: " + salesFile);
        System.out.println("Output File: " + salesFileOut);

        try {
            int fileStructure = Constants.IO_FIXED_LENGTH;
            CobolIoProvider ioProvider = CobolIoProvider.getInstance();
            AbstractLineReader reader  = ioProvider.getLineReader(
                   fileStructure, ICopybookDialects.FMT_MAINFRAME,
                    CopybookLoader.SPLIT_NONE,  Cb2xmlConstants.USE_STANDARD_COLUMNS,
                    copybookName, salesFile
            );
 
            BufferedWriter w = new BufferedWriter(new FileWriter(salesFileOut));

            while ((saleRecord = reader.read()) != null) {
                String sep = "";
                FieldIterator fieldIterator = saleRecord.getFieldIterator(0); // assuming 1 record per file
                for (AbstractFieldValue v : fieldIterator) {
            		w.write(sep);
            		w.write(v.asString()); 
            		sep = ",";
            	}
                w.newLine();
            }

            reader.close();
            w.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
    	new XmplCopyToCsv02();
    }
}
