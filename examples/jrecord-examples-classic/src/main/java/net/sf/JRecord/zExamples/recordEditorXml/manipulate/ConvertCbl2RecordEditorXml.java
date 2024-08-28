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
      
package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.ByteArrayInputStream;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.Log.TextLog;
import net.sf.JRecord.Numeric.ICopybookDialects;


/**
 * Purpose: This program demonstrates converting a Cobol Copybook to a
 *     Xml copybook.
 *
 * @author Bruce Martin
 *
 */
public class ConvertCbl2RecordEditorXml {

	private static byte[] copyBookBytes
			= (
				  "              03  DTAR020-KCODE-STORE-KEY.\n"
				+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).\n"
				+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3.\n"
				+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3.\n"
				+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3.\n"
				+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3.\n"
				+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3.\n"
			).getBytes();

    public static void main(String[] args) throws Exception {
    	CobolCopybookLoader loaderCBL = new CobolCopybookLoader();
    	ExternalRecord extlayoutCBL = loaderCBL.loadCopyBook(
    	    new ByteArrayInputStream(copyBookBytes),
    	    Conversion.getCopyBookId("DTAR020.cbl"),
    	    CopybookLoader.SPLIT_NONE, 0, "", ICopybookDialects.FMT_MAINFRAME, 0, new TextLog());
    	CopybookWriterManager.getInstance()
    			.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER)
    		    .writeCopyBook(
    		    		new FileOutputStream("H:\\Temp\\XML_DTAR020.Xml"), 
    		    		extlayoutCBL, 
    		    		null);
    }
}
