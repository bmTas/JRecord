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

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringReader;

import junit.framework.TestCase;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.cbl2csv.Cobol2Csv;
import net.sf.JRecord.cbl2csv.Csv2Cobol;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
//import net.sf.JRecord.zExamples.cobol.toCsv.ParseArgsCobol2Csv;
import net.sf.JRecord.zTest.Common.TstConstants;


/**
 * Testing Copying to/from Csv files with embedded field-delimiters and Quote characters
 * 
 * @author Bruce Martin
 *
 */
public class TstCobolToCsv03 extends TestCase {

	static final String[] DELIMS = {
		"|", ",", "\t", "!", ";", "bar", "tab"
	};
	
	static final String[] CONV = {
		"Asis", "no-", "_", "Leave_Asis", "Change_Minus_To_Underscore", "Drop_Minus"
	};
	
	static final String[] QUOTES = {
		"\"", "'", "`"
	};
	
	
	static ICobolIOBuilder iobCbl = JRecordInterface1.COBOL
			.newIOBuilder(new StringReader(Cbl2CsvCommonCode.COBOL_COPBOOK2), "Rec");
	
	public void testCsv01() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q, -1);
				}
			}
		}
	};
	
	public void testCsv02() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q, IFileStructureConstants.IO_BIN_TEXT);
				}
			}
		}
	};

	public void testCsv03() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q, IFileStructureConstants.IO_UNICODE_TEXT);
				}
			}
		}
	};

	public void testCsv05() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q, IFileStructureConstants.IO_UNICODE_NAME_1ST_LINE);
				}
			}
		}
	};
	public void testCsv06() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				for (String q : QUOTES) {
					tstCsv(delim, repl, q, IFileStructureConstants.IO_NAME_1ST_LINE);
				}
			}
		}
	};

	public void tstCsv(String delim, String renameOpt, String quote, int csvFileStructure) throws IOException{
		
		String id = delim + " " + renameOpt + " " + quote;
		String csvFile = TstConstants.TEMP_DIRECTORY + "zzz" +  ".csv";
		ParseArgsCobol2Csv csvArgs;
		if (csvFileStructure < 0) {
			String[] args1 = {
					"-I", Cbl2CsvCommonCode.DTAR020_FILE_NAME, 
					"-IFS", "Text",     /* File Structure  */
					"-O", csvFile, 
		//			"-OFS", "Text",     /* File Structure  */
					"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
					"-D", delim,
					"-Q", quote,                /* Quote           */
					"-Rename", renameOpt,
			}; /* Field Seperator will default to \t */
			csvArgs = new ParseArgsCobol2Csv(true, args1);
		} else {
			String[] args1 = {
					"-I", Cbl2CsvCommonCode.DTAR020_FILE_NAME, 
					"-IFS", "Text",     /* File Structure  */
					"-O", csvFile, 
					"-OFS", "" + csvFileStructure,     /* File Structure  */
					"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
					"-D", delim,
					"-Q", quote,                /* Quote           */
					"-Rename", renameOpt,
			}; /* Field Seperator will default to \t */
			csvArgs = new ParseArgsCobol2Csv(true, args1);
			
		}

        ByteArrayOutputStream os = new ByteArrayOutputStream(Cbl2CsvCommonCode.LINES2_BYTES.length);
		
		Cobol2Csv.runCobol2Csv(csvArgs, iobCbl, 
				new ByteArrayInputStream(Cbl2CsvCommonCode.LINES2_BYTES), os);

		byte[] csvBytes = os.toByteArray();
		
		System.out.println();
		System.out.println(csvFile);
		System.out.println(new String(csvBytes));
		System.out.println();

		chkFile(id, new InputStreamReader(new ByteArrayInputStream(csvBytes)), Cbl2CsvCommonCode.buildLines2(csvArgs)); 
		
		String binFile = csvFile + ".bin";
		String[] args2;
		if (csvFileStructure < 0) {
			args2 = new String[] {
					"-I", csvFile, 
					"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
					"-D", delim,
					"-Q", quote,                /* Quote           */
					"-O", binFile, 
					"-OFS", "Text",     /* File Structure  */
					"-Rename", renameOpt,
			};
		} else {
			args2 = new String[] {
					"-I", csvFile, 
					"-IFS", "" + csvFileStructure,
					"-C", TstConstants.COBOL_DIRECTORY + "xxx.cbl", 
					"-D", delim,
					"-Q", quote,                /* Quote           */
					"-O", binFile, 
					"-OFS", "Text",     /* File Structure  */
					"-Rename", renameOpt,
			};
		}
		
//		Csv2Cobol.main(args2);
		ByteArrayOutputStream osBin = new ByteArrayOutputStream(Cbl2CsvCommonCode.COBOL_LINES2.length * 2);
		Csv2Cobol.doCopy(new ParseArgsCobol2Csv(false, args2), iobCbl, new ByteArrayInputStream(csvBytes), osBin);
		
		String cobolData = new String(osBin.toByteArray());
		System.out.println(cobolData);
		
		chkTrimFile(id, new StringReader(cobolData), Cbl2CsvCommonCode.COBOL_LINES2);
	}



	/**
	 * @param expected
	 * @param r
	 * @throws IOException
	 */
	public void chkFile(String id, Reader reader, String[] expected) throws IOException {
		String line;
		int i = 0;
		BufferedReader r = new BufferedReader(reader);
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(id + " " + (i+1), expected[i++], line);
		}	
		r.close();
	}

	/**
	 * @param expected
	 * @param r
	 * @throws IOException
	 */
	public void chkTrimFile(String id, Reader reader, String[] expected) throws IOException {
		String line;
		int i = 0;
		BufferedReader r = new BufferedReader(reader);
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(id + " " + (i+1), expected[i++], line.trim());
		}	
		r.close();
	}

}
