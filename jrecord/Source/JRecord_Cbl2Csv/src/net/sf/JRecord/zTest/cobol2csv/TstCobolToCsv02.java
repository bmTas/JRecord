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
import java.util.Arrays;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.cbl2csv.Cobol2Csv;
import net.sf.JRecord.cbl2csv.Csv2Cobol;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;

/**
 * Testing Different Delimiters and Field-Name conversions
 * @author Bruce Martin
 *
 */
public class TstCobolToCsv02 extends TestCase {

	static final String[] DELIMS = {
		"|", ",", "\t", "!", ";", "bar", "tab"
	};
	
	static final String[] CONV = {
		"Asis", "no-", "_", "Leave_Asis", "Change_Minus_To_Underscore", "Drop_Minus"
	};
	
	static ICobolIOBuilder iobCbl = JRecordInterface1.COBOL
			.newIOBuilder(TstConstants.COBOL_DIRECTORY + "DTAR020.cbl");
	
	public void testCsv01() throws IOException {
		for (String delim: DELIMS) {
			for (String repl : CONV) {
				tstCsv("4", delim, repl);
			}
		}
	};

	public void tstCsv(String id, String delim, String renameOpt) throws IOException{
		String csvFile = TstConstants.TEMP_DIRECTORY + "DTAR020_tst1" + id + ".csv";
		String[] args1= {
				"-I", Cbl2CsvCommonCode.DTAR020_FILE_NAME, 
				"-IFS", "Fixed_Length",     /* File Structure  */
				"-IC", "CP037",            /* Character set   */
				"-O", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "DTAR020.cbl", 
				"-D", delim,
				"-Q", "\"",                /* Quote           */
				"-Rename", renameOpt,
		}; /* Field Seperator will default to \t */
		ParseArgsCobol2Csv csvArgs = new ParseArgsCobol2Csv(true, args1);
		

        ByteArrayOutputStream os = new ByteArrayOutputStream(Cbl2CsvCommonCode.DTAR020_BYTES.length);
		
		Cobol2Csv.runCobol2Csv(csvArgs, iobCbl, 
				new ByteArrayInputStream(Cbl2CsvCommonCode.DTAR020_BYTES), os);

		byte[] csvBytes = os.toByteArray();
		String[] colNames = Cbl2CsvCommonCode.getColNames(csvArgs);

		chkFile(new InputStreamReader(new ByteArrayInputStream(csvBytes)), Cbl2CsvCommonCode.buildLines(delim, colNames));
		
		String binFile = csvFile + ".bin";
		String[] args2= {
				"-I", csvFile, 
				"-C", TstConstants.COBOL_DIRECTORY + "DTAR020.cbl", 
				"-D", delim,
				"-Q", "\"",                /* Quote           */
				"-O", binFile, 
				"-OFS", "Fixed_Length",     /* File Structure  */
				"-OC", "CP037",            /* Character set   */
				"-Rename", renameOpt,
		};
		
//		Csv2Cobol.main(args2);
		ByteArrayOutputStream osBin = new ByteArrayOutputStream(Cbl2CsvCommonCode.DTAR020_BYTES.length);
		Csv2Cobol.doCopy(new ParseArgsCobol2Csv(false, args2), iobCbl, new ByteArrayInputStream(csvBytes), osBin);
		
		byte[] binBytes = osBin.toByteArray();
		boolean equals = Arrays.equals(Cbl2CsvCommonCode.DTAR020_BYTES, binBytes);
		if (! equals) {
			System.out.println("Lengths: " + Cbl2CsvCommonCode.DTAR020_BYTES.length + ", " + binBytes.length);
			assertTrue(equals);
		}
	}



	/**
	 * @param expected
	 * @param r
	 * @throws IOException
	 */
	public void chkFile(Reader reader, String[] expected) throws IOException {
		String line;
		int i = 0;
		BufferedReader r = new BufferedReader(reader);
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(expected[i++], line);
		}	
		r.close();
	}

}
