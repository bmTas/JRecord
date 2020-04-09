/*  -------------------------------------------------------------------------
 *
 *            Sub-Project: JRecord "Classic" Interface examples
 *    
 *    Sub-Project purpose: Examples of using JRecord Classic (or old interface)
 *                        to perform IO on Cobol Data files
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
      
package net.sf.JRecord.zExamples.recordEditorXml.manipulate;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.External.CopybookWriterManager;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.base.RecordEditorXmlWriter;
import net.sf.JRecord.Types.Type;

public class WriteSchemas {


	public static void main(String[] args) throws FileNotFoundException, Exception {
		
		ExternalRecord csvInputSchema  = ExternalRecord	
									.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", "\t", "\"") 
									.asExternalRecord();
// Define the output record (with the fields)
		ExternalRecord csvOutputSchema 
				= ExternalRecord
						.newCsvRecord("", Constants.IO_NAME_1ST_LINE, "", ";", "\"")
							.addCsvField("Sku",   Type.ftChar, 0)
							.addCsvField("Store", Type.ftNumAnyDecimal, 0)
							.addCsvField("Date",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Dept",  Type.ftNumAnyDecimal, 0)
							.addCsvField("Qty",   Type.ftNumAnyDecimal, 0)
							.addCsvField("Price", Type.ftNumAnyDecimal, 0)
							.addCsvField("GST",   Type.ftNumAnyDecimal, 0)
						.asExternalRecord();

		
    	CopybookWriterManager writerManager = CopybookWriterManager.getInstance();
    	RecordEditorXmlWriter writer
    			= (RecordEditorXmlWriter) writerManager.get(CopybookWriterManager.RECORD_EDITOR_XML_WRITER);

    	writer.writeCopyBook(new FileOutputStream("G:\\Temp\\XML_TabDelimCsvSchema.Xml"), csvInputSchema,  null);
    	writer.writeCopyBook(new FileOutputStream("G:\\Temp\\XML_SaleCsvSchema.Xml"),     csvOutputSchema, null);

	}

}
