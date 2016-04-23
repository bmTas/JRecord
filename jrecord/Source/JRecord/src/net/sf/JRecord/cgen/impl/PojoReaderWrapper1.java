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

package net.sf.JRecord.cgen.impl;

import java.io.IOException;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.cgen.def.IAsPojo;
import net.sf.JRecord.cgen.def.IReader;

public class PojoReaderWrapper1<Line> implements IReader<Line> {

	public static <Line> PojoReaderWrapper1<Line> newReader(AbstractByteReader reader, IAsPojo<Line> converter) {
		return new PojoReaderWrapper1<Line>(reader, converter);
	}
	
	private final AbstractByteReader reader;
	private final IAsPojo<Line> converter;
	
	private PojoReaderWrapper1(AbstractByteReader reader, IAsPojo<Line> converter) {
		super();
		this.reader = reader;
		this.converter = converter;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IReader#read()
	 */
	@Override
	public Line read() throws IOException {
		byte[] in = reader.read();
		if (in == null) {
			return null;
		}
		converter.setData(in);
		return converter.asPojo();
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.cgen.def.IReader#close()
	 */
	@Override
	public void close() throws IOException {
		reader.close();
	}

}
