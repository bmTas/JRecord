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

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.Numeric.Convert;
import net.sf.cb2xml.def.Cb2xmlConstants;

public class TstUserCode {
    private String installDir     = TstConstants.SAMPLE_DIRECTORY;
    private String salesFile      = installDir + "DTAR020.bin";
    private String copybookName   = TstConstants.COBOL_DIRECTORY + "DTAR020.cbl";

    private TstUserCode() throws Exception {
    	CobolIoProvider ioProvider = CobolIoProvider.getInstance();
    	AbstractLineReader reader  = ioProvider.getLineReader(
                Constants.IO_FIXED_LENGTH,  
                Convert.FMT_MAINFRAME,
                CopybookLoader.SPLIT_NONE, 
                Cb2xmlConstants.USE_SUPPLIED_COLUMNS,
                copybookName, 
                salesFile);
    	AbstractLine saleRecord;
    	while ((saleRecord = reader.read()) != null) {
 
             System.out.println(saleRecord.getFieldValue("KEYCODE-NO").asString()
                     + " " + saleRecord.getFieldValue("QTY-SOLD").asString()
                     + " " + saleRecord.getFieldValue("SALE-PRICE").asString());
    	}
    	reader.close();
    }

    public static void main(String[] args) throws Exception {
    	new TstUserCode();
    }
}
