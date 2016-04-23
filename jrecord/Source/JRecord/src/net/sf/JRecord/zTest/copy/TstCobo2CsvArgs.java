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
import net.sf.JRecord.External.ExternalConversion;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.ICopybookDialects; 
import net.sf.JRecord.zExamples.cobol.toCsv.ParseArgsCobol2Csv; 
import junit.framework.TestCase;


/**
 * Check Program arguments are parsed correctly for Cobol2Csv & Csv2Cobol programs
 * 
 * @author Bruce Martin
 *
 */
public class TstCobo2CsvArgs extends TestCase {

	private static final String OUT_FONT = "font2";
	private static final String OUT_FILE = "outFile";
	private static final String IN_FONT = "font1";
	private static final String COPYBOOK = "Copybook";
	private static final String IN_FILE = "inFile";

	public void testParseArgsCobol2Csv() {
		String[] parms1 = {
				ParseArgsCobol2Csv.ARG_IN_FILE,  IN_FILE, 
				ParseArgsCobol2Csv.ARG_COPYBOOK,  COPYBOOK,
				ParseArgsCobol2Csv.ARG_INPUT_FONT, IN_FONT,
				ParseArgsCobol2Csv.ARG_BINARY, "1",
				ParseArgsCobol2Csv.ARG_STRUCTURE, "1",
				ParseArgsCobol2Csv.ARG_OUT_FILE, OUT_FILE,
				ParseArgsCobol2Csv.ARG_OUTPUT_FONT, OUT_FONT,
				ParseArgsCobol2Csv.ARG_CSV_PARSER, "1",
				ParseArgsCobol2Csv.ARG_RENAME, "1",
		};
		String[] parms2= {
				ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE, 
				ParseArgsCobol2Csv.ARG_COPYBOOK1,  COPYBOOK,
				ParseArgsCobol2Csv.ARG_INPUT_FONT1, IN_FONT,
				ParseArgsCobol2Csv.ARG_BINARY1, "1",
				ParseArgsCobol2Csv.ARG_STRUCTURE1, "1",
				ParseArgsCobol2Csv.ARG_OUT_FILE1, OUT_FILE,
				ParseArgsCobol2Csv.ARG_OUTPUT_FONT1, OUT_FONT,
				ParseArgsCobol2Csv.ARG_CSV_PARSER, "1",
				ParseArgsCobol2Csv.ARG_RENAME, "1",
		};
		
		doCheckAllArgs(new ParseArgsCobol2Csv(parms1));
		doCheckAllArgs(new ParseArgsCobol2Csv(parms2));
	}
	
	private void doCheckAllArgs(ParseArgsCobol2Csv a) {
		assertEquals(IN_FILE, a.infile);
		assertEquals(COPYBOOK, a.copybookName);
		assertEquals(IN_FONT, a.inFont);
		assertEquals(1, a.binFormat);
		assertEquals(1, a.inputFileStructure);
		assertEquals(OUT_FILE, a.outfile);
		assertEquals(OUT_FONT, a.outFont);
		assertEquals(1, a.csvParser);
		assertEquals(1, a.renameOption);
	}
	

	public void testRenameArg() {
		
		ParseArgsCobol2Csv.Option[] options = ParseArgsCobol2Csv.getRenameOptions();
		for (ParseArgsCobol2Csv.Option o : options) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_RENAME, o.display
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(o.code, ac.renameOption);
		}
	}
	

	public void testCsvParserArg() {
		
		ParseArgsCobol2Csv.Option[] options = ParseArgsCobol2Csv.getCsvParserOptions();
		for (ParseArgsCobol2Csv.Option o : options) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_CSV_PARSER, o.display
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(o.code, ac.csvParser);
		}
	}

	

	public void testFileStructure() {
		
		int[] structures = {
				Constants.IO_DEFAULT, Constants.IO_FIXED_LENGTH, Constants.IO_FIXED_LENGTH_CHAR,
				Constants.IO_BIN_TEXT, Constants.IO_TEXT_LINE, Constants.IO_UNICODE_TEXT,
				Constants.IO_VB, Constants.IO_VB_FUJITSU, Constants.IO_VB_OPEN_COBOL,
		};
		for (int fileOrganisation : structures) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_STRUCTURE1, ExternalConversion.getFileStructureAsString(0, fileOrganisation)
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(fileOrganisation, ac.inputFileStructure);
		}
	}


	public void testDialect() {
		
		int[] dialects = {
				ICopybookDialects.FMT_MAINFRAME,
				ICopybookDialects.FMT_FUJITSU,
				ICopybookDialects.FMT_OPEN_COBOL,
				ICopybookDialects.FMT_OC_MICRO_FOCUS,
				ICopybookDialects.FMT_FS2000,
		};
		for (int dialect : dialects) {
			String[] a = {
					ParseArgsCobol2Csv.ARG_IN_FILE1,  IN_FILE,
					ParseArgsCobol2Csv.ARG_BINARY1, ConversionManager.getInstance().getConverter4code(dialect).getName()
			};
			ParseArgsCobol2Csv ac = new ParseArgsCobol2Csv(a);
			assertEquals(IN_FILE, ac.infile);
			assertEquals(dialect, ac.binFormat);
		}
	}

}
