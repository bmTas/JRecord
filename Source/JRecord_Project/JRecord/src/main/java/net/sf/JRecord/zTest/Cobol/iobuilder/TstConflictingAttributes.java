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
import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.CobolIoProvider;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import junit.framework.TestCase;

public class TstConflictingAttributes extends TestCase {

	private static byte[] COPBOOK_BYTES
				=("              03  DTAR020-KCODE-STORE-KEY.                        \n"
				+ "                  05 DTAR020-KEYCODE-NO      PIC X(08).           \n"
				+ "                  05 DTAR020-STORE-NO        PIC S9(03)   COMP-3. \n"
				+ "              03  DTAR020-DATE               PIC S9(07)   COMP-3. \n"
				+ "              03  DTAR020-DEPT-NO            PIC S9(03)   COMP-3. \n"
				+ "              03  DTAR020-QTY-SOLD           PIC S9(9)    COMP-3. \n"
				+ "              03  DTAR020-SALE-PRICE         PIC S9(9)V99 COMP-3. \n")
	
			.getBytes();
	private static int[] TEXT_IO = {
		Constants.IO_FIXED_LENGTH_CHAR,
		Constants.IO_UNICODE_CSV,
		Constants.IO_UNICODE_CSV_NAME_1ST_LINE,
		Constants.IO_UNICODE_NAME_1ST_LINE,
		Constants.IO_UNICODE_TEXT
	};
	

	public void testBinWithText1() throws IOException, RecordException{
		
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
			
			assertTrue("Expecting failure for: " + ti, ok);
		}
	}
	

	public void testBinWithText2() throws IOException, RecordException{
		
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
			
			assertTrue("Expecting failure for: " + ti, ok);
		}
	}

}
