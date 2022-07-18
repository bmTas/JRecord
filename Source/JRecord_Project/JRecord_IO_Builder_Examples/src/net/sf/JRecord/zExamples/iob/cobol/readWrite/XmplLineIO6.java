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
      
package net.sf.JRecord.zExamples.iob.cobol.readWrite;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Read / Write Mainframe Cobol file using a Cobol Copybook
 * 
 * 
 * @author Bruce Martin
 *
 */
public final class XmplLineIO6 {

    private static final double GST_CONVERSION = 1.1;

    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String salesFileOut   = installDir + "DTAR020out.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    /**
     * Example of LineReader / LineWrite classes
     */
    private XmplLineIO6() {
        super();

        int lineNum = 0;
        double gstExclusive;
        AbstractLine saleRecord;
        Conversion.setDefaultEbcidicCharacterset("IBM273");
        ICobolIOBuilder ioBldr = JRecordInterface1.COBOL
        		.newIOBuilder(copybookName)
        			.setDialect( ICopybookDialects.FMT_MAINFRAME)
        			.setFont("cp037")
        			.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
        			.setDropCopybookNameFromFields(true);


        try {
            AbstractLineReader reader  = ioBldr.newReader(salesFile);
            AbstractLineWriter writer  = ioBldr.newWriter(salesFileOut);
            		

            while ((saleRecord = reader.read()) != null) {
                lineNum += 1;

                System.out.print(saleRecord.getFieldValue("KEYCODE-NO").asString()
                        + " " + saleRecord.getFieldValue("QTY-SOLD").asString()
                        + " " + saleRecord.getFieldValue("SALE-PRICE").asString());

                gstExclusive = saleRecord.getFieldValue("SALE-PRICE").asDouble() / GST_CONVERSION;
                saleRecord.getFieldValue("SALE-PRICE").set(gstExclusive);
                writer.write(saleRecord);

                System.out.println(" " + saleRecord.getFieldValue("SALE-PRICE").asString());
            }

            reader.close();
            writer.close();
        } catch (Exception e) {
            System.out.println("~~> " + lineNum + " " + e.getMessage());
            System.out.println();

            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
    	new XmplLineIO6();
    }
}
