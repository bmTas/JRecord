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

import java.io.UnsupportedEncodingException;
import java.util.ArrayList;

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
		String s = getFullLine();
		byte[] b;
		
		if ("".equals(layout.getFontName())) {
			b = s.getBytes();
		} else {
			try {
				b = s.getBytes(layout.getFontName());
			} catch (UnsupportedEncodingException e) {
				throw new RuntimeException(e);
			}
		}
		return b;
	}


	@Override
	public String getFullLine() {
		RecordDetail record = layout.getRecord( getPrefIdx());
		
		return record
				.getParser()
				.formatFieldList(
						fields, record, record.getFieldTypes());
	}
	
	@Override
	public void setData(String newVal) {
		RecordDetail record = layout.getRecord(getPrefIdx());

		fields = new ArrayList<Object>(
				record.getParser().getFieldList(newVal, record));
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
