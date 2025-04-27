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

import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.IOException;

import org.junit.jupiter.api.Test;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.CommonCodeTestCopybooksAndData;

public class TstConflictingAttributes  {

	private static byte[] COPBOOK_BYTES
				= CommonCodeTestCopybooksAndData.DTAR020_COPYBOOK
						.getBytes();
	private static int[] TEXT_IO = {
		Constants.IO_FIXED_LENGTH_CHAR,
		Constants.IO_UNICODE_CSV,
		Constants.IO_UNICODE_CSV_NAME_1ST_LINE,
		Constants.IO_UNICODE_NAME_1ST_LINE,
		Constants.IO_UNICODE_TEXT
	};
	

	@Test public void testBinWithText1() throws IOException, RecordException{
		
		for (int ti : TEXT_IO) {
			ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(ti) 
					.setFont("CP037");
			
			boolean ok = false;
			try {
				ioBuilder.getLayout();
			} catch (RuntimeException e) {
				ok = true;
				System.out.println("---> " + e.getMessage());
			}
			
			assertTrue(ok, "Expecting failure for: " + ti);
		}
	}
	

	@Test public void testBinWithText2() throws IOException, RecordException{
		
		for (int ti : TEXT_IO) {
			ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL
				.newIOBuilder(new ByteArrayInputStream(COPBOOK_BYTES), "DTAR020")
					.setFileOrganization(ti) 
					.setFont("CP037");
			
			boolean ok = false;
			try {
				ioBuilder.getLayout();
			} catch (RuntimeException e) {
				ok = true;
				System.out.println("---> " + e.getMessage());
			}
			
			assertTrue(ok, "Expecting failure for: " + ti);
		}
	}

}
