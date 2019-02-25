/**
 * 
 */
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

package net.sf.JRecord.zExamples.cobol.toCsv.test;

import net.sf.JRecord.cbl2csv.Csv2Cobol;
import net.sf.JRecord.zData.Data;

/**
 * @author Bruce Martin
 *
 */
public class TestCsv2Cobol_1 {

	/**
	 * @param args
	 */
	public static void main(String[] a) {

		String inputFileName1 = Data.DTAR020_BIN_RESOURCE.getFile();
		String[] args1 = {
				"-I", inputFileName1, 
				"-O", ExampleConstants.TEMP_DIR + "o_DTAR020.bin", 
				"-C", Data.DTAR020_COPYBOOK_FILE_NAME, 
				"-Q", "\"",                /* Quote           */
				"-FS", "Fixed_Length",     /* File Structure  */
				"-OC", "CP273",            /* Input Character set   */
		}; /* Field Seperator will default to \t */
		
		Csv2Cobol.main(args1); 
		
		String inputFileName2 = TestCsv2Cobol_1.class.getResource("DTAR1000_Store_file_std_02.csv").getFile();
		String[] args2= {
				"-I", inputFileName2, 
				"-O", ExampleConstants.TEMP_DIR + "o_DTAR1000_Store_file_std_02.bin", 
				"-C", TestCobol2Csv02_2.class.getResource("DTAR1000.cbl").getFile(), 
				"-Q", "\"",               /* Quote           */
				"-FS", "Mainframe_VB",    /* File Structure  */
				"-OC", "CP273",           /* Character set   */
				"-D", ";"                 /* Field Seperator */ 
		}; /* Field Seperator will default to \t */
		
		Csv2Cobol.main(args2); 

	}

}
