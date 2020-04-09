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

package net.sf.JRecord.IO.builders;

import java.io.IOException;

import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.def.IO.builders.ICsvIOBuilder;
import net.sf.JRecord.def.IO.builders.IDefineCsvFields;

public class CsvIOBuilder extends CblIOBuilderBase<ICsvIOBuilder> implements ICsvIOBuilder, IDefineCsvFields {

	private ExternalRecord record = new ExternalRecord();
	private boolean definedField = false;
	
//	private int parser = 0;

	
	private CsvIOBuilder(String delim, String quote) {

		super(0);
		super.setFileOrganization(Constants.IO_UNICODE_NAME_1ST_LINE);
		super.setFont("");

		record.setQuote(quote);
		record.setDelimiter(delim);
		record.setRecordType(Constants.rtDelimitedAndQuote);
	}

	@Override
	protected ExternalRecord getExternalRecordImpl() throws IOException {
	
		record.setFontName(super.getFont());
		return record;
	}

	
	

	/**
	 * @param val
	 * @see net.sf.JRecord.External.ExternalRecord#setDelimiter(java.lang.String)
	 */
	@Override
	public ICsvIOBuilder setDelimiter(String val) {
		record.setDelimiter(val);
		super.clearLayout();
		return this;
	}

	/**
	 * @param val
	 * @see net.sf.JRecord.External.ExternalRecord#setQuote(java.lang.String)
	 */
	@Override
	public ICsvIOBuilder setQuote(String val) {
		record.setQuote(val);
		super.clearLayout();
		record.setRecordType(Constants.rtDelimitedAndQuote);
		if (val == null || val.length() == 0) {
			record.setRecordType(Constants.rtDelimited);
		}
		return this;
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.def.IO.builders.ICsvIOBuilder#setParser(int)
	 */
	@Override
	public ICsvIOBuilder setParser(int csvParser) {
		record.setRecordStyle(csvParser);
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#checkOk(boolean)
	 */
	@Override
	protected void checkOk(boolean input) {
		if (definedField) {
		} else if (input) {
			switch (super.getFileOrganization()) {
			case Constants.IO_UNICODE_NAME_1ST_LINE:
			case Constants.IO_UNICODE_CSV_NAME_1ST_LINE:
			case Constants.IO_NAME_1ST_LINE:
			case Constants.IO_CSV_NAME_1ST_LINE:
			case Constants.IO_BIN_NAME_1ST_LINE:
			case Constants.IO_BIN_CSV_NAME_1ST_LINE:
				break;
			default:
				throw new RecordException("Unless you are reading from a file with field names on the first line, "
							+ "You must define fields (or columns) using the defineFields() method !! ");
			}		
		} else {
			throw new RecordException("For Output files, You must define fields (or columns) using the defineFields() method !! ");
		}
	}

//	/* (non-Javadoc)
//	 * @see net.sf.JRecord.IO.builders.CblIOBuilderBase#newLine()
//	 */
//	@Override
//	public AbstractLine newLine() throws IOException {
//		LayoutDetail layout = getLayout();
//		if (layout.)
//		return new CsvLine(layout);
//	}

	public IDefineCsvFields defineFields() {
		return this;
	}

	

	/* (non-Javadoc)
	 * @see net.sf.JRecord.External.ExternalRecord#addCsvField(java.lang.String, int, int)
	 */
	@Override
	public IDefineCsvFields addCsvField(String name, int type, int decimal) {
		definedField = true;
		record.addCsvField(name, type, decimal);
		super.clearLayout();
		return this;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.IO.builders.CsvIOBuilder.IDefineCsvFields#endOfRecord()
	 */
	@Override
	public CsvIOBuilder endOfRecord() {
		return this;
	}

	
	public static ICsvIOBuilder newCsvIOBuilder() {
		return new CsvIOBuilder(",", "\"");
	}
	
	
	public static ICsvIOBuilder newCsvIOBuilder(String delim, String quote) {
		return new CsvIOBuilder(delim, quote);
	}
}
