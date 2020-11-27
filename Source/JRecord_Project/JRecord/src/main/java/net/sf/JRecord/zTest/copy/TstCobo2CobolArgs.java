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

package net.sf.JRecord.zTest.copy;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.zExamples.copy.ParseArgsCobol2Cobol;
import junit.framework.TestCase;


/**
 * Check Program arguments are parsed correctly for Cobol2Csv & Csv2Cobol programs
 * 
 * @author Bruce Martin
 *
 */
public class TstCobo2CobolArgs extends TestCase {

	private static final String OUT_FONT = "font2";
	private static final String OUT_FILE = "outFile";
	private static final String IN_FONT = "font1";
	private static final String COPYBOOK = "InCopybook";
	private static final String OUT_COPYBOOK = "OutCopybook";
	private static final String IN_FILE = "inFile";

	public void testParseArgsCobol2Csv() {
		String[] parms1 = {
				ParseArgsCobol2Cobol.ARG_IN_FILE,  IN_FILE, 
				ParseArgsCobol2Cobol.ARG_IN_COPYBOOK,  COPYBOOK,
				ParseArgsCobol2Cobol.ARG_INPUT_FONT, IN_FONT,
				ParseArgsCobol2Cobol.ARG_IN_BINARY, "1",
				ParseArgsCobol2Cobol.ARG_IN_STRUCTURE, "1",
				ParseArgsCobol2Cobol.ARG_OUT_FILE, OUT_FILE,
				ParseArgsCobol2Cobol.ARG_OUTPUT_FONT, OUT_FONT,
				ParseArgsCobol2Cobol.ARG_OUT_COPYBOOK,  OUT_COPYBOOK,
				ParseArgsCobol2Cobol.ARG_OUT_BINARY, "2",
				ParseArgsCobol2Cobol.ARG_OUT_STRUCTURE, "2",
		};
		
		
		doCheckAllArgs(new ParseArgsCobol2Cobol(parms1));
	}
	
	private void doCheckAllArgs(ParseArgsCobol2Cobol a) {
		assertEquals(IN_FILE, a.infile);
		assertEquals(COPYBOOK, a.inCopybookName);
		assertEquals(IN_FONT, a.inFont);
		assertEquals(1, a.inBinFormat);
		assertEquals(1, a.inFileStructure);
		assertEquals(OUT_FILE, a.outfile);
		assertEquals(OUT_FONT, a.outFont);
		assertEquals(OUT_COPYBOOK, a.outCopybookName);
		assertEquals(2, a.outBinFormat);
		assertEquals(2, a.outFileStructure);

	}
	

	

	public void testFileStructure() {
		
		int[] structures = {
				Constants.IO_DEFAULT, Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_CHAR,
				Constants.IO_BIN_TEXT, Constants.IO_TEXT_LINE, Constants.IO_UNICODE_TEXT,
				Constants.IO_VB, Constants.IO_VB_FUJITSU, Constants.IO_VB_GNU_COBOL,
		};
		for (int fileOrganisation : structures) {
			String[] a = {
					ParseArgsCobol2Cobol.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Cobol.ARG_IN_STRUCTURE, ExternalConversion.getFileStructureAsString(0, fileOrganisation)
			};
			ParseArgsCobol2Cobol ac = new ParseArgsCobol2Cobol(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(fileOrganisation, ac.inFileStructure);
			String[] a1 = {
					ParseArgsCobol2Cobol.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Cobol.ARG_OUT_STRUCTURE, ExternalConversion.getFileStructureAsString(0, fileOrganisation)
			};
			ac = new ParseArgsCobol2Cobol(a1);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(fileOrganisation, ac.outFileStructure);
		}
	}


	public void testDialect() {
		
		int[] dialects = {
				ICopybookDialects.FMT_MAINFRAME,
				ICopybookDialects.FMT_FUJITSU,
				ICopybookDialects.FMT_GNU_COBOL,
				ICopybookDialects.FMT_OC_MICRO_FOCUS,
				ICopybookDialects.FMT_FS2000,
		};
		ParseArgsCobol2Cobol ac;
		for (int dialect : dialects) {
			String[] a = {
					ParseArgsCobol2Cobol.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Cobol.ARG_IN_BINARY, ConversionManager.getInstance().getConverter4code(dialect).getName()
			};
			ac = new ParseArgsCobol2Cobol(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(dialect, ac.inBinFormat);
			String[] a1 = {
					ParseArgsCobol2Cobol.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Cobol.ARG_OUT_BINARY, ConversionManager.getInstance().getConverter4code(dialect).getName()
			};
			ac = new ParseArgsCobol2Cobol(a1);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(dialect, ac.outBinFormat);
		}
	}

}
