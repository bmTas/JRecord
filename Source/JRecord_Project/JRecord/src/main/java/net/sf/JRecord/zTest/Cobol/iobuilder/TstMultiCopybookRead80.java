/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
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

package net.sf.JRecord.zTest.Cobol.iobuilder;

import java.io.FileInputStream;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class TstMultiCopybookRead80 extends TestCase {


	ICobolIOBuilder ioBld = null, ioBld2 = null;

	public void testRead1() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(false, getIoBuilder());
		
		tst.tstRead();
	}
	
	public void testRead2() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(true, getIoBuilder());
		
		tst.tstRead();
	}

	public void testRead3() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(false, getIoBuilder2());
		
		tst.tstRead();
	}
	
	public void testRead4() throws Exception {
		TstMultiCopybookCommon tst = new TstMultiCopybookCommon(true, getIoBuilder2());
		
		tst.tstRead();
	}
	
	private ICobolIOBuilder getIoBuilder() throws Exception {
		if (ioBld == null) {
			ioBld = loadRecordDefinition();
		}
		
		return ioBld;
	}
	
	private ICobolIOBuilder getIoBuilder2() throws Exception {
		if (ioBld2 == null) {
			ioBld2 = loadRecordDefinition2();
		}
		
		return ioBld2;
	}

    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition() throws Exception{
    	
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest10.cbl").getFile();
 
    	ICobolIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newIOBuilder(copyFileName2)
    						.setFont(Conversion.DEFAULT_ASCII_CHARSET)
     						.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL);
    	return setupRecordDefinition(IOBldr);
    }
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition2() throws Exception{
    	
    	String copyFileName2 = this.getClass().getResource("MultiRecordTest10.cbl").getFile();
 
    	ICobolIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newIOBuilder(new FileInputStream(copyFileName2), "MultiRecordTest")
							.setFont(Conversion.DEFAULT_ASCII_CHARSET)
      						.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL);
    	return setupRecordDefinition(IOBldr);
    }


	/**
	 * @param IOBldr
	 * @return
	 */
	private ICobolIOBuilder setupRecordDefinition(
			ICobolIOBuilder IOBldr) {
		IOBldr			.setDialect(ICopybookDialects.FMT_FUJITSU)
     					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
 					;
    	
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.TRAILER_ID))
    	      .setRecordSelection("Detail-Record-A", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_A_ID))
    	      .setRecordSelection("Detail-Record-B", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_B_ID))
    	;

    	return IOBldr;
	}
    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
}
