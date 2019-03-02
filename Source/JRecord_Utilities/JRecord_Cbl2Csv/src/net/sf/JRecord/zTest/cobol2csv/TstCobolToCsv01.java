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
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;

import net.sf.JRecord.cbl2csv.Cobol2Csv;
import net.sf.JRecord.cbl2csv.Csv2Cobol;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import net.sf.JRecord.zTest.Common.TstConstants;
import junit.framework.TestCase;


/**
 * Basic Tests using the "Batch" interface
 * Other programs will large volume tests without accessing the Disks
 * @author Bruce Martin
 *
 */
public class TstCobolToCsv01 extends TestCase {

	
	public void testCsv01() throws IOException {
		tstCsv("1", "!", "Asis");
	};
	
	
	public void testCsv02() throws IOException {
		tstCsv("2", ";", "_");
	};
	
	
	public void testCsv03() throws IOException {
		tstCsv("3", ",", "no-");
	};
	
	
	public void testCsv04() throws IOException {
		tstCsv("4", "\u0001", "Asis");
	};
	
	
	public void testCsv05() throws IOException {
		tstCsv("5", "\u00FF", "Asis");
	};
	
	public void testCsv06() throws IOException {
		tstCsv("6", "x'08'", "Asis");
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
		
		Cobol2Csv.main(args1); 

		ParseArgsCobol2Csv argObj = new ParseArgsCobol2Csv(args1);
		String[] colNames = Cbl2CsvCommonCode.getColNames(argObj);
		

		chkFile(csvFile, Cbl2CsvCommonCode.buildLines(delim, colNames));
		
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
		
		Csv2Cobol.main(args2); 
		
		byte[] fileContents = TestCommonCode.loadFile(binFile);
		
		boolean equals = Arrays.equals(Cbl2CsvCommonCode.DTAR020_BYTES, fileContents);
		if (! equals) {
			System.out.println("Lengths: " + Cbl2CsvCommonCode.DTAR020_BYTES.length + ", " + fileContents.length);
			assertTrue(equals);
		}
	}
	

	public void chkFile(String csvFile, String[] expected) throws IOException {
		BufferedReader r = new BufferedReader(new FileReader(csvFile));
		String line;
		int i = 0;
		
		while ((line = r.readLine()) != null) {
			TestCase.assertEquals(expected[i++], line);
		}	
		r.close();
	}

}
