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
import java.io.InputStream;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.cgen.def.IDeserializer;
import net.sf.JRecord.cgen.def.IReader;

public class ReadFromBytes<T> implements IReader<T> {

	public final IDeserializer<T> deserializer;
	public final AbstractByteReader reader;
	
	public ReadFromBytes(IBasicFileSchema schema, IDeserializer<T> deserializer) {
		this.deserializer = deserializer;
		this.reader = ByteIOProvider.getInstance().getByteReader(schema);
	}
	
	public ReadFromBytes<T> open(String fileName) throws IOException {
		reader.open(fileName);
		return this;
	}
	
	public ReadFromBytes<T> open(InputStream in) throws IOException {
		reader.open(in);
		return this;
	}
	
	@Override
	public T read()  throws IOException {
		byte[] value = reader.read();
		if (value == null) {
			return null;
		}
		return deserializer.deserialize(value);
	}

	@Override
	public void close() throws IOException {
		reader.close();
	}
	
	public static <TT extends Object> ReadFromBytes<TT> newReader(IBasicFileSchema schema, IDeserializer<TT> deserializer) {
		return new ReadFromBytes<TT>(schema, deserializer);
	}
}
