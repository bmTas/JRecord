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

package net.sf.JRecord.IO;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.ByteTextWriter;
import net.sf.JRecord.ByteIO.BinaryByteWriter;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.CsvParser.CsvParserManagerChar;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;


/**
 * This class will write a AbstractLine to a standard Windows/*nix Text file. It is similar in function
 * to <b>TextLineWriter</b>, the difference being <b>TextLineWriter</b> uses Standard BufferedWriter
 * (String based) Class for Writing and can not handle Hex (i.e. x'FF') values correctly, while this class 
 * uses streams and is able to handle Hex values. This class is based on <b>ByteTextWriter</b>.
 * <p>This class was written to support hex field (x'FF') separators in csv (delimited) files.
 * 
 * @see TextLineWriter
 * @see ByteTextWriter
 * 
 * @author Bruce Martin
 * @version 0.68
 *
 */
public class BinTextWriter extends LineWriterWrapper {

	private OutputStream oStream = null;
	boolean toOpen = true;
	boolean names1stLine = false;
	
	public BinTextWriter(boolean nameOn1stLine) {
		super(null);
		names1stLine = nameOn1stLine;
	}

	/**
	 * @see net.sf.JRecord.IO.LineWriterWrapper#open(java.io.OutputStream)
	 */
	@Override
	public void open(OutputStream outputStream) throws IOException {
		oStream = outputStream;
		toOpen = true;
	}

	/**
	 * @see net.sf.JRecord.IO.LineWriterWrapper#write(net.sf.JRecord.Details.AbstractLine)
	 */
	@Override
	public void write(AbstractLine line) throws IOException {
		if (toOpen) {
			BinaryByteWriter writer = new BinaryByteWriter(false, false, line.getLayout().getRecordSep());
			toOpen = false;
			super.setWriter(writer);
			super.open(oStream);
			
			if (names1stLine) {
				LayoutDetail layout =  line.getLayout();
				RecordDetail rec = layout.getRecord(0);
				if (rec != null && rec.getFieldCount() > 0) {
					byte[] seperator = layout.getDelimiterDetails().asBytes();
					byte[] quote = layout.getQuoteDetails().asBytes();
					boolean addQuotes = false;
					if (quote != null && quote.length > 0 
					&& CsvParserManagerChar.getInstance().get(rec.getRecordStyle()).isQuoteInColumnNames()) {
						addQuotes = true;
					}
					if (addQuotes) {
						byte[] sep = {};
						for (int i = 0; i < rec.getFieldCount(); i++) {
							oStream.write(sep);
							oStream.write(quote);
							oStream.write(Conversion.getBytes(rec.getField(i).getName(), layout.getFontName()));
							oStream.write(quote);
							sep = seperator;
						}
					} else {
						oStream.write(Conversion.getBytes(rec.getField(0).getName(), layout.getFontName()));
						for (int i = 1; i < rec.getFieldCount(); i++) {
							oStream.write(seperator);
							oStream.write(Conversion.getBytes(rec.getField(i).getName(), layout.getFontName()));
						}
					}
				}
				oStream.write(layout.getRecordSep());
			}
			oStream = null;
		}
		super.write(line);
	}
	
	
}
