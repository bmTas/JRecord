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

package net.sf.JRecord.Details;

import java.util.ArrayList;

import net.sf.JRecord.CsvParser.ICsvCharLineParser;

public class CsvLine extends ListLine {


	public CsvLine(LayoutDetail layoutDetails, String s) {
		this(layoutDetails);
		
		setData(s);
	}


	public CsvLine(LayoutDetail layoutDetails) {
		super(layoutDetails);
		
		if (! layoutDetails.isCsvLayout()) {
			throw new RuntimeException("Layout must be a CsvLayout !!!");
		}
	}

	@Override
	public byte[] getData() {
		RecordDetail record = layout.getRecord( getPrefIdx());
		
		return record
				.getCsvByteParser()
					.formatFieldListByte(
						fields, record, record.getFieldTypes());
	}


	@Override
	public String getFullLine() {
		RecordDetail record = layout.getRecord( getPrefIdx());
		
		return record
				.getCharParser()
				.formatFieldList(
						fields, record, record.getFieldTypes());
	}
	
	@Override
	public void setData(String newVal) {
		RecordDetail record = layout.getRecord(getPrefIdx());

		ICsvCharLineParser parser = record.getCharParser();
		
		if (parser.isUpdatable()) {
			fields = new ArrayList<Object>(
					parser.getFieldList(newVal, record));
		}
	}

	
	

	@Override
	public void setData(byte[] newVal) {
		RecordDetail record = layout.getRecord(getPrefIdx());

		fields = new ArrayList<Object>(
					record.getCsvByteParser().getFieldList(newVal, record));
	}


	@Override
	protected final int getAdj() {
		return 1;
	}


	private int getPrefIdx() {
		int idx = getPreferredLayoutIdx();
		if (idx < 0 || idx >= layout.getRecordCount()) {
			idx = 0;
		}
		return idx;
	}
}
