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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.def.IO.builders.ICobolMultiCopybookIOBuilder;

/**
 * Check that Multiple copybooks have loadeed correctly
 * 
 * @author Bruce Martin
 *
 */
public class TstMultiCopybookDef03 extends TestCase  {
	
	private static final String RECORD_A_CBL = "RecordA.cbl";
	private static final String MULTI_RECORD_TEST_CBL = "MultiRecordTest03.cbl";
	private static final String RECORD_B_CBL = "RecordB.cbl";

	private static final byte[] CPY1_BYTES = getBytes(RECORD_A_CBL); 
	private static final byte[] CPY2_BYTES = getBytes(MULTI_RECORD_TEST_CBL); 
	private static final byte[] CPY3_BYTES = getBytes(RECORD_B_CBL); 


	static ICobolMultiCopybookIOBuilder ioBld = null;
	
		
	public void test01() throws Exception {
		(new TstMultiCopybookCommon(false, getIoBuilder())).tstFields();
	}
	
	
	public void test02() throws Exception {
		(new TstMultiCopybookCommon(false, loadRecordDefinition2())).tstFields();
	}


	private ICobolMultiCopybookIOBuilder getIoBuilder() throws Exception {
		if (ioBld == null) {
			ioBld = loadRecordDefinition1();
		}
		
		return ioBld;
	}
	
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolMultiCopybookIOBuilder loadRecordDefinition1() throws Exception{
    	
    	String copyFileName1 = this.getClass().getResource(RECORD_A_CBL).getFile();
    	String copyFileName2 = this.getClass().getResource(MULTI_RECORD_TEST_CBL).getFile();
    	String copyFileName3 = this.getClass().getResource(RECORD_B_CBL).getFile();
 
    	ICobolMultiCopybookIOBuilder IOBldr = JRecordInterface1.COBOL
    				.newMultiCopybookIOBuilder("MultiRecordTest")
    					.setDialect(ICopybookDialects.FMT_FUJITSU)
     					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
     					.addCopyBook(copyFileName1)
     						.setRecordSelectionCurrentCopybook(
     								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_A_ID))
     					.addCopyBook(copyFileName2)
     						.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE)
     					.addCopyBook(copyFileName3)
      						.setRecordSelectionCurrentCopybook(
      								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_B_ID))
 					;
    	
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.TRAILER_ID));

    	return IOBldr;
    }
    
    /**
     * Load RecordLayout (schema) from the Cobol copybook
     * @throws Exception
     */
    private ICobolIOBuilder loadRecordDefinition2() throws Exception{
 
       	ICobolMultiCopybookIOBuilder IOBldr = JRecordInterface1.COBOL
				.newMultiCopybookIOBuilder("MultiRecordTest")
					.setDialect(ICopybookDialects.FMT_FUJITSU)
 					.setFileOrganization(Constants.IO_CONTINOUS_NO_LINE_MARKER)
 					.addCopyBook(new ByteArrayInputStream(CPY1_BYTES), "RecordA")
 						.setRecordSelectionCurrentCopybook(
 								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_A_ID))
 					.addCopyBook(new ByteArrayInputStream(CPY2_BYTES), "MultiRecord")
 						.setSplitCopybook(CopybookLoader.SPLIT_REDEFINE)
 					.addCopyBook(new ByteArrayInputStream(CPY3_BYTES), "RecordB")
  						.setRecordSelectionCurrentCopybook(
  								newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.REC_B_ID))
					;
    	
    		
    	IOBldr.setRecordSelection("Header-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.HEADER_ID))
    	      .setRecordSelection("Detail-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.DETAIL_ID))
    	      .setRecordSelection("Trailer-Record", newFieldSelection(TstMultiCopybookCommon.RECORD_TYPE, TstMultiCopybookCommon.TRAILER_ID));

    	return IOBldr;
    }

    
    private ExternalFieldSelection newFieldSelection(String fieldName, String value) {
    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
    	r.setCaseSensitive(false);
    	return r;
    }
    
    
    private static byte[] getBytes(String filename) {
    	
    	try {
        	String filename1 = TstMultiCopybookRead03.class.getResource(filename).getFile();
			FileInputStream inStream = new FileInputStream(filename1);
			ByteArrayOutputStream os = new ByteArrayOutputStream(0x8000);
			byte[] buf = new byte[0x8000];
			int l = inStream.read(buf);
			while (l > 0) {
				os.write(buf, 0, l);
				l = inStream.read(buf);
			}
			inStream.close();
			return os.toByteArray();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

    	return new byte[0];
    }

    
}
