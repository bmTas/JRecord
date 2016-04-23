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
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.LineProvider;
import net.sf.JRecord.charIO.ICharReader;

public abstract class BasicTextLineReader extends AbstractLineReader {

	private InputStream inStream = null;
	private ICharReader reader;

	public BasicTextLineReader() {
		super();
	}

	public BasicTextLineReader(LineProvider provider) {
		super(provider);
	}

	  protected void open(ICharReader r, InputStream inputStream, LayoutDetail layout, String font)
	  throws IOException {

		  reader = r;
		  inStream = inputStream;
		  setLayout(layout);

		  reader.open(inputStream, font);
	  }

	/**
	 * Method to do the actual read. It is called by read in AbstractLineReader
	 * @see AbstractLineReader#read()
	 */
	@Override
	public AbstractLine readImplementation() throws IOException {
	    AbstractLine ret = null;
	
	    if (reader == null) {
	        throw new IOException(AbstractLineReader.NOT_OPEN_MESSAGE);
	    }
	    String s = reader.read();
	
	    if (s != null) {
	        ret = getLine(s);
	    }
	
	    return ret;
	}

	/**
	 * @see net.sf.JRecord.JRecordInterface1#close()
	 */
	public void close() throws IOException {
	
	    reader.close();
	    if (inStream != null) {
			inStream.close();
	    }
	
		inStream  = null;
	}

	public final ICharReader getReader() {
		return reader;
	}

}