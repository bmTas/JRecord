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



public class ZTstCsvCopy {

	public static void main(String[] args) {
		String copybookFileName = ZTstCsvCopy.class.getResource("OccursDepending1.cbl").getFile();
		
//		ICobolIOBuilder ioBuilder = CobolIoProvider.getInstance()
//				.newIOBuilder(copybookFileName, ICopybookDialects.FMT_MAINFRAME)
//					.setFileOrganization(io);
		
//		String fileName = ZTstCsvCopy.class.getResource("OccursDependingOn.txt").getFile();
//		String[] arguments = {
//				"-I", fileName,   "-FS", "Text",
//				"-C", copybookFileName,
//				"-Dialect", "Mainframe",
//				
//				"-O", "G:\\Temp\\OccursDep.csv"
//		};
//		Cobol2Csv.main(arguments);
		
		String[] arguments2 = {
				"-O", "G:\\Temp\\OccursDep.csv.txt",   "-OFS", "Text",
				"-C", copybookFileName,
				"-Dialect", "Mainframe",
				
				"-I", "G:\\Temp\\OccursDep.csv"
		};

		Csv2Cobol.main(arguments2);
	}
}
