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

import net.sf.JRecord.Common.CommonBits;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.cbl2csv.args.ParseArgsCobol2Csv;
import net.sf.JRecord.zTest.Common.TestCommonCode;
import net.sf.JRecord.zTest.Common.TstConstants;

public class Cbl2CsvCommonCode {
	
	public static final String DTAR020_FILE_NAME = TstConstants.SAMPLE_DIRECTORY + "DTAR020_tst1.bin";
	public static final String[][]  CSV_CONTENTS = {
			{"63604808", "20", "40118", "170", "1", "4.87"},
			{"69684558", "20", "40118", "280", "1", "19.00"},
			{"69684558", "20", "40118", "280", "-1", "-19.00"},
			{"69694158", "20", "40118", "280", "1", "5.01"},
			{"62684671", "20", "40118", "685", "1", "69.99"},
			{"62684671", "20", "40118", "685", "-1", "-69.99"},
			{"61664713", "59", "40118", "335", "1", "17.99"},
			{"61664713", "59", "40118", "335", "-1", "-17.99"},
			{"61684613", "59", "40118", "335", "1", "12.99"},
			{"68634752", "59", "40118", "410", "1", "8.99"},
			{"60694698", "59", "40118", "620", "1", "3.99"},
			{"60664659", "59", "40118", "620", "1", "3.99"},
			{"60614487", "59", "40118", "878", "1", "5.95"},
			{"68654655", "166", "40118", "60", "1", "5.08"},
			{"69624033", "166", "40118", "80", "1", "18.19"},
			{"60604100", "166", "40118", "80", "1", "13.30"},
			{"68674560", "166", "40118", "170", "1", "5.99"},
	};
	
	public static final String[] COLUMN_NAMES = {
		"DTAR020-KEYCODE-NO", "DTAR020-STORE-NO", "DTAR020-DATE", "DTAR020-DEPT-NO", "DTAR020-QTY-SOLD", "DTAR020-SALE-PRICE"
	};
	
	public static final String[] COLUMN_NAMES2 = {
		"Field-1", "Field-2", "Field-3",
	};
	
	public static final byte[] DTAR020_BYTES = TestCommonCode.loadFile(DTAR020_FILE_NAME);
	
	
	public static final String[] COBOL_LINES2 = {
		"1234567.1234567.1234567.",
		"123     456     789",
		",       '       \"",
		";;      ''      \"\"",
		"12,34   '34'    \"45\"",
		"1,23,4  3'4     4\"5",
		"1,2,3,4 3''4    4\"\"5",
		"'12'34' '3''4'  \"4\"5\"",
		"1,2'3,4 '3'4'5  4\"55\"",
	};
	
	public static final String[][] VALUES2 = getValues2();
	public static final byte[] LINES2_BYTES = getBytes2();
	public static final String COBOL_COPBOOK2 
				= "        01  Rec.\n"
				+ "            03 Field-1    pic x(8).\n"
				+ "            03 Field-2    pic x(8).\n"
				+ "            03 Field-3    pic x(8).\n";

	
	private static String[][] getValues2() {
		String[][] v = new String[COBOL_LINES2.length][];
		for (int i = 0; i < COBOL_LINES2.length; i++) {
			v[i] = new String[3];
			v[i][0] = COBOL_LINES2[i].substring(0, 8).trim();
			v[i][1] = COBOL_LINES2[i].substring(8, 16).trim();
			v[i][2] = COBOL_LINES2[i].substring(16).trim();
		}

		return v;
	}
	private static byte[] getBytes2() {
		StringBuilder b = new StringBuilder(COBOL_LINES2.length * 20);
		String sep = "";
		for (int i = 0; i < COBOL_LINES2.length; i++) {
			b.append(sep).append(COBOL_LINES2[i]);
			sep = "\n";
		}
		
		return b.toString().getBytes();
	}
	
	/**
	 * @param csvArgs
	 * @return
	 */
	public static String[] getColNames(ParseArgsCobol2Csv csvArgs) {
		String[] colNames= new String[Cbl2CsvCommonCode.COLUMN_NAMES.length];
		
		for (int i = 0; i < Cbl2CsvCommonCode.COLUMN_NAMES.length; i++) {
			colNames[i] = csvArgs.updateName(Cbl2CsvCommonCode.COLUMN_NAMES[i]);
		}
		return colNames;
	}

	public static String[] buildLines2(ParseArgsCobol2Csv csvArgs) {
		String[] ret = new String[VALUES2.length+ 1];
		String[] l;
		String[] colNames = new String[COLUMN_NAMES2.length];
		String delim = csvArgs.sep;
		int adj = 0;
		boolean addColNames 
			=  (csvArgs.toCsv && CommonBits.areFieldNamesOnTheFirstLine(csvArgs.outputFileStructure))
			|| ((!csvArgs.toCsv) && CommonBits.areFieldNamesOnTheFirstLine(csvArgs.inputFileStructure));
		
		if (addColNames) {
			for (int j = 0; j < colNames.length; j++) {
				colNames[j] = csvArgs.updateName(COLUMN_NAMES2[j]);
			}
			ret[0] = buildLine(delim, colNames);
			adj = 1;
		}
		for (int i = 0; i < VALUES2.length; i++) {
			l = new String[VALUES2[i].length];
			for (int j = 0; j < l.length; j++) {
				l[j] = VALUES2[i][j];
				//System.out.print("\t> " + j + ": " + l[j] + " " + (csvArgs==null) + " " +csvArgs.quote);
				if (l[j].indexOf(csvArgs.quote) >= 0 
				|| l[j].indexOf(delim) >= 0) {
					l[j] =  csvArgs.quote +
							Conversion.replace(l[j], csvArgs.quote, csvArgs.quote + csvArgs.quote)
								.append(csvArgs.quote).toString();
				}
			}
			
			ret[i+adj] = buildLine(delim, l);
		}
		
		return ret;
	}
	

	public static String[] buildLines(String delim, String[] colNames) {
		String[] ret = new String[CSV_CONTENTS.length+ 1];
		
		
		ret[0] = buildLine(delim, colNames);
		for (int i = 0; i < CSV_CONTENTS.length; i++) {
			ret[i+1] = buildLine(delim, CSV_CONTENTS[i]);
		}
		
		return ret;
	}
	
	public static String buildLine(String delim, String[] fields) {
		StringBuilder b = new StringBuilder(40);
		char ch;
		String sep= "";
		if ("tab".equalsIgnoreCase(delim)) {
			delim = "\t";
		} else if ("bar".equalsIgnoreCase(delim)) {
			delim = "|";
		} else if (delim.charAt(0) == '\\' && ((ch = delim.charAt(1)) == 'u' || ch == 'U')) {
			char[] chars = { (char)Integer.parseInt(delim.substring(2), 16) };
			delim = new String(chars);
		} else if (delim.startsWith("x'")) {
			delim = Conversion.toString(new byte[] { Conversion.getByteFromHexString(delim) }, Conversion.DEFAULT_ASCII_CHARSET);
		} 
		
		for (String f : fields) {
			b.append(sep).append(f);
			sep = delim;
		}
		return b.toString();
	}
}
