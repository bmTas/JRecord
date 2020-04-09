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

package net.sf.JRecord.External.base;

import java.io.BufferedWriter;
import java.io.FileOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.External.Def.ExternalField;
import net.sf.JRecord.External.base.ExternalConversion;
import net.sf.JRecord.Log.AbsSSLogger;
import net.sf.JRecord.Log.TextLog;

/**
 * Write a RecordLayout as a RecordEditor-CSBV layout file.
 * Basically The first record holds Record Details
 * and each line holds a field definition:
 * 
 * <ol>
 * <li>Starting Position
 * <li>Length
 * <li>Number of places after the decimal point
 * <li>Field Name
 * <li>Field Type (String [or char], num, mainframe_zoned, fuji_zoned)
 * <li>Decimal - number of places after the decimal point
 * <li>Format Field Format (used in the Record Editor)
 * <li>Parameter - parameter for the Type / Format
 * </ol>
 * 
 * @author Bruce Martin
 *
 */
public class RecordEditorCSVWriter implements CopybookWriter {

	private String fldSeperator = "\t"; 
	
	/**
	 * Create Record Editor CSV Writer
	 * @param fieldSeperator field Seperator (i.e. Tab or Comma, etc)
	 */
	public RecordEditorCSVWriter(String fieldSeperator) {
		fldSeperator = fieldSeperator;
	}
	
	
	
	/**
	 * @see net.sf.JRecord.External.CopybookWriter#writeCopyBook(java.lang.String, net.sf.JRecord.External.ExternalRecord, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@Override
	public String writeCopyBook(String directory, BaseExternalRecord<?> copybook,
			AbsSSLogger log) throws Exception {
		String fileName;
		
		log = TextLog.getLog(log);
		directory = ExternalConversion.fixDirectory(directory);

		for (int i = 0; i < copybook.getNumberOfRecords(); i++) {
			writeCopyBook(directory, copybook.getRecord(i), log);
		}
		
		fileName = directory + copybook.getRecordName() + Constants.TXT_EXTENSION;
		copybook.setRecordName(ExternalConversion.copybookNameToFileName(copybook.getRecordName()));
		writeCopyBook(new FileOutputStream(fileName), 
				copybook, log);
		return fileName;
	}

	/**
	 * @see net.sf.JRecord.External.CopybookWriter#writeCopyBook(java.io.OutputStream, net.sf.JRecord.External.ExternalRecord, net.sf.JRecord.Log.AbsSSLogger)
	 */
	@SuppressWarnings("deprecation")
	@Override
	public void writeCopyBook(OutputStream outStream, BaseExternalRecord<?> copybook,
			AbsSSLogger log) throws Exception {

		int i;
		
//		if (copybook.getNumberOfRecordFields()> 0) {
		ExternalField field;
		BaseExternalRecord<?> sr;
		String description;
		BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(outStream));
		
		writer.write(Constants.RECORD_NAME + fldSeperator
				+ copybook.getFileStructure() + fldSeperator
				+ copybook.getRecordType() + fldSeperator
				+ copybook.getDelimiter() + fldSeperator
				+ copybook.getRecordStyle() + fldSeperator
				+ copybook.getQuote() + fldSeperator
				+ copybook.getListChar() + fldSeperator
				+ fixDescription(copybook.getDescription())
		);
		writer.newLine();

		for (i = 0; i < copybook.getNumberOfRecords(); i++) {
			sr = copybook.getRecord(i);
			writer.write(Constants.SUB_RECORD_NAME + fldSeperator
					+ sr.getRecordName() + fldSeperator
					+ sr.getTstField() + fldSeperator
					+ sr.getTstFieldValue() + fldSeperator
					+ sr.getParentRecord() + fldSeperator
					+ sr.getListChar()
			);
			writer.newLine();
		}

		for (i = 0; i < copybook.getNumberOfRecordFields(); i++) {
			field = copybook.getRecordField(i);
			description = field.getDescription();
			if (description == null) {
				description = "";
			} else if (description.indexOf(fldSeperator) >= 0
					||  description.indexOf('\n') >= 0) {
				description = fixDescription(description);
			}
			writer.write(field.getPos() + fldSeperator
					+ field.getLen() + fldSeperator
					+ field.getName() + fldSeperator
					+ description + fldSeperator
					+ field.getType() + fldSeperator
					+ field.getDecimal() + fldSeperator
					+ field.getCellFormat() + fldSeperator
					+ field.getParameter());
			writer.newLine();
		}
		writer.close();
		outStream.close();
//		}
	}
	
	private String fixDescription(String description) {	
		StringBuilder b = new StringBuilder(description);
		
		Conversion.replace(b,  fldSeperator, " ");
		Conversion.replace(b,  "\n", "\\n");
		return b.toString();
	}

}
