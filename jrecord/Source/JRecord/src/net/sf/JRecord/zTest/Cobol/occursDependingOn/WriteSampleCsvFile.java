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

package net.sf.JRecord.zTest.Cobol.occursDependingOn;

import java.io.IOException;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;
import net.sf.JRecord.zTest.Common.TstConstants;

public class WriteSampleCsvFile {

	public static final int PURCH_COUNT = 16;
	public static final int SALES_COUNT = 5;
	
	
	public static void main(String[] args)  throws IOException, RecordException {
		ICsvIOBuilder ioBuilder = JRecordInterface1.CSV	.newIOBuilder("\t", "\"");
		IDefineCsvFields defineFields = ioBuilder.defineFields();
		defineFields		.addCsvField("Location-Number", Type.ftNumLeftJustified, 0)
							.addCsvField("Location-Name", Type.ftChar, 0)
							.addCsvField("months", Type.ftNumLeftJustified, 0);
		
		for (int monthNum = 0; monthNum < SALES_COUNT; monthNum++) {
			defineFields	.addCsvField("sales-count (" + monthNum + ")" , Type.ftNumLeftJustified, 0)
							.addCsvField("sales-value (" + monthNum + ")", Type.ftNumLeftJustified, 2);
		}
		defineFields		.addCsvField("total-sales", Type.ftNumLeftJustified, 2);
		defineFields		.addCsvField("week-no", Type.ftNumLeftJustified, 0);
		
		for (int weekNo = 0; weekNo < PURCH_COUNT; weekNo++) {
			defineFields	.addCsvField("purchase-count (" + weekNo + ")", Type.ftNumLeftJustified, 0)
							.addCsvField("purchase-value (" + weekNo + ")", Type.ftNumLeftJustified, 2);
		}
		defineFields	.addCsvField("total-purchase-count", Type.ftNumLeftJustified, 0)
						.addCsvField("total-purchase-value", Type.ftNumLeftJustified, 2);
		
		defineFields.endOfRecord();
					
		AbstractLineWriter w = ioBuilder.newWriter(TstConstants.TEMP_DIRECTORY + "OccursDependingOn.csv");
		try {
			for (int purchNum = 0; purchNum < PURCH_COUNT; purchNum++) {
				for (int salesNum = 0; salesNum < SALES_COUNT; salesNum++) {
					w.write(Code.generateLine(ioBuilder.newLine(), purchNum, salesNum));
				}
			}
		} finally {
			w.close();
		}
	}
}
