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
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.cgen.def.ISerializer;
import net.sf.JRecord.cgen.def.IWriter;

public class WriteAsBytes<T> implements IWriter<T> {
	
	private final ISerializer<T> serializer;
	private final AbstractByteWriter writer;

	public WriteAsBytes(IBasicFileSchema schema, ISerializer<T> serializer) {
		this.serializer = serializer;
		this.writer = ByteIOProvider.getInstance().getByteWriter(schema);
	}
	
	public WriteAsBytes<T> open(String fileName) throws IOException {
		writer.open(fileName);
		return this;
	}
	
	public WriteAsBytes<T> open(OutputStream in) throws IOException {
		writer.open(in);
		return this;
	}
	
	
	@Override
	public void write(T record) throws IOException {
		writer.write(serializer.serialize(record));
	}

	@Override
	public void close() throws IOException {
		writer.close();
	}

	public static <TT extends Object> WriteAsBytes<TT> newReader(IBasicFileSchema schema, ISerializer<TT> serializer) {
		return new WriteAsBytes<TT>(schema, serializer);
	}
}
