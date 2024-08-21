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

package net.sf.JRecord.zTest.cobol2csv;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import junit.framework.TestCase;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Numeric.ConversionManager;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.JRecord.Option.ICobolSplitOptions;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.cbl2csv.args.RecordSelect;


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
		
		doCheckStdArgs(new ParseArgsCobol2Csv(parms1));
		doCheckStdArgs(new ParseArgsCobol2Csv(parms2));
	}
	
	public void testMultiRecArgs() {
		String[] splitStr = {"none", "01", "redefine", "Highest"};
		int[] splitVal = {ICobolSplitOptions.SPLIT_NONE, ICobolSplitOptions.SPLIT_01_LEVEL,
				ICobolSplitOptions.SPLIT_REDEFINE, ICobolSplitOptions.SPLIT_HIGHEST_REPEATING
		};
	
		String[] yesNoStr = { "yes", "YES", "y", "Y", "true", "True", "TRUE", "t", "x", "no", "n", "false"};
		
		for (int i = 0; i < yesNoStr.length; i++) {
			int splitIdx = i % splitStr.length;
			String[] parms1 = {
					ParseArgsCobol2Csv.ARG_IN_FILE,  IN_FILE, 
					ParseArgsCobol2Csv.ARG_COPYBOOK,  COPYBOOK,
					ParseArgsCobol2Csv.ARG_INPUT_FONT, IN_FONT,
					ParseArgsCobol2Csv.ARG_BINARY, "1",
					ParseArgsCobol2Csv.ARG_STRUCTURE, "1",
					ParseArgsCobol2Csv.ARG_OUT_FILE, OUT_FILE,
					ParseArgsCobol2Csv.ARG_OUTPUT_FONT, OUT_FONT,
					ParseArgsCobol2Csv.ARG_RENAME, "1",
					
					ParseArgsCobol2Csv.OPT_ADD_RECORD_NAME, yesNoStr[i],
					ParseArgsCobol2Csv.OPT_SPLIT, splitStr[splitIdx]
			};
			
			ParseArgsCobol2Csv multiRecArgs = ParseArgsCobol2Csv.multiRecArgs(parms1);
			doCheckStdArgs(multiRecArgs);
			assertEquals(splitVal[splitIdx], multiRecArgs.split);
			assertEquals(i + ": ", i < yesNoStr.length - 4, multiRecArgs.addRecordNameToCsv);
		}
		
		for (int i = 0; i < 8; i++) {
			List<String> parms= new ArrayList<String>(Arrays.asList(
					ParseArgsCobol2Csv.ARG_IN_FILE,  IN_FILE, 
					ParseArgsCobol2Csv.ARG_COPYBOOK,  COPYBOOK,
					ParseArgsCobol2Csv.ARG_INPUT_FONT, IN_FONT,
					ParseArgsCobol2Csv.ARG_BINARY, "1",
					ParseArgsCobol2Csv.ARG_STRUCTURE, "1",
					ParseArgsCobol2Csv.ARG_OUT_FILE, OUT_FILE,
					ParseArgsCobol2Csv.ARG_OUTPUT_FONT, OUT_FONT,
					ParseArgsCobol2Csv.ARG_RENAME, "1",
					
					ParseArgsCobol2Csv.OPT_ADD_RECORD_NAME, "yes",
					ParseArgsCobol2Csv.OPT_SPLIT, "none"
			));

		
			for (int j = 1; j <= i; j++) {
				parms.add(ParseArgsCobol2Csv.OPT_RECSEL);
				parms.add("rec" + j + " rec-type" + j + "=" + (j-1));
			}
			
			ParseArgsCobol2Csv multiRecArgs = ParseArgsCobol2Csv.multiRecArgs(parms.toArray(new String[parms.size()]));
			
			doCheckStdArgs(multiRecArgs);
			assertEquals(ICobolSplitOptions.SPLIT_NONE, multiRecArgs.split);
			assertEquals(true, multiRecArgs.addRecordNameToCsv);
			
			assertEquals(i, multiRecArgs.recordSelect.size());
			
			for (int j = 1; j <= i; j++) {
				RecordSelect recSel = multiRecArgs.recordSelect.get(j-1);
				String message = i + ", " + j;
				assertEquals(message, "rec"+j, recSel.recordName);
				assertEquals(message, "rec-type"+j, recSel.fieldName);
				assertEquals(message, ""+(j-1), recSel.value);
			}

		}
	}
	
	public void testMultiRecArgs02() {

		List<String> stdParms = Arrays.asList(
				ParseArgsCobol2Csv.ARG_IN_FILE,  IN_FILE, 
				ParseArgsCobol2Csv.ARG_COPYBOOK,  COPYBOOK,
				ParseArgsCobol2Csv.ARG_INPUT_FONT, IN_FONT,
				ParseArgsCobol2Csv.ARG_BINARY, "1",
				ParseArgsCobol2Csv.ARG_STRUCTURE, "1",
				ParseArgsCobol2Csv.ARG_OUT_FILE, OUT_FILE,
				ParseArgsCobol2Csv.ARG_OUTPUT_FONT, OUT_FONT,
				ParseArgsCobol2Csv.ARG_RENAME, "1"
		);
		String[] yesNoStr = { "yes", "YES", "y", "Y", "true", "True", "TRUE", "t", "x", "no", "n", "false"};
		ArrayList<String> parms;
		ParseArgsCobol2Csv multiRecArgs;
		for (int i = 0; i < yesNoStr.length; i++) {
			parms = new ArrayList<String>(stdParms);
			Collections.addAll(parms, ParseArgsCobol2Csv.OPT_REPORT_INVALID, yesNoStr[i]);
			
			doErrorChks(i + ": ", parms, i < yesNoStr.length - 4, null, null, null);
		}

		parms = new ArrayList<String>(stdParms);
		Collections.addAll(parms, ParseArgsCobol2Csv.OPT_LOW_VALUES, "NULL");	
		doErrorChks( "Low: ", parms, false, "NULL", null, null);
		
		parms = new ArrayList<String>(stdParms);
		Collections.addAll(parms, ParseArgsCobol2Csv.OPT_HIGH_VALUES, "NULL");	
		doErrorChks( "High: ", parms, false, null, "NULL", null);

		parms = new ArrayList<String>(stdParms);
		Collections.addAll(parms, ParseArgsCobol2Csv.OPT_NUM_SPACES, "SPACES");		
		doErrorChks( "Spaces: ", parms, false,  null, null, "SPACES");

		parms = new ArrayList<String>(stdParms);
		Collections.addAll(parms, 
				ParseArgsCobol2Csv.OPT_LOW_VALUES, "LOW",
				ParseArgsCobol2Csv.OPT_HIGH_VALUES, "HIGH",
				ParseArgsCobol2Csv.OPT_NUM_SPACES, "SPACES"
		);		
		doErrorChks( "All: ", parms, false, "LOW", "HIGH", "SPACES");

	}
	
	private void doErrorChks(String id, List<String> parms, boolean invalid,
			String lowValuesTxt, String highValuesTxt, String numSpaces) {
		ParseArgsCobol2Csv multiRecArgs = ParseArgsCobol2Csv.multiRecArgs(parms.toArray(new String[parms.size()]));
		doCheckStdArgs(multiRecArgs);

		assertEquals(id, invalid, multiRecArgs.reportInvalid);
		assertEquals(id, lowValuesTxt, multiRecArgs.lowValuesTxt);
		assertEquals(id, highValuesTxt, multiRecArgs.highValuesTxt);
		assertEquals(id, numSpaces, multiRecArgs.numericSpaceTxt);
	}

	
	private void doCheckStdArgs(ParseArgsCobol2Csv a) {
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
			assertEquals(o.display, o.code, ac.csvParser);
		}
	}

	

	public void testFileStructure() {
		
		int[] structures = {
				IFileStructureConstants.IO_DEFAULT, IFileStructureConstants.IO_FIXED_LENGTH, IFileStructureConstants.IO_FIXED_LENGTH_CHAR,
				IFileStructureConstants.IO_BIN_TEXT, IFileStructureConstants.IO_TEXT_LINE, IFileStructureConstants.IO_UNICODE_TEXT,
				IFileStructureConstants.IO_VB, IFileStructureConstants.IO_VB_FUJITSU, IFileStructureConstants.IO_VB_GNU_COBOL,
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
				ICopybookDialects.FMT_GNU_COBOL,
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
