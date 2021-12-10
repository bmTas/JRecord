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

package net.sf.JRecord.cgen.impl.io;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.cgen.def.ISerializer;
import net.sf.JRecord.cgen.def.IWriter;
import net.sf.JRecord.cgen.impl.derSer.GetBytesSerializer;


/**
 * This class writes a user pojo to a file as a Cobol Record.
 * 
 * @author Bruce Martin
 *
 * @param <T> User Pojo top be written to a Cobol Data file
 */
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
	
	public WriteAsBytes<T> open(OutputStream out) throws IOException {
		writer.open(out);
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
	
	/**
	 * Create Writer for IGetBytes file
	 * @param schema file description
	 * @return requested writer
	 * @throws IOException 
	 */
	public static IWriter<IGetByteData> newGetBytesWriter(IBasicFileSchema schema, OutputStream out) throws IOException {
		WriteAsBytes<IGetByteData> writeAsBytes = new WriteAsBytes<IGetByteData>(schema, new GetBytesSerializer());
		writeAsBytes.open(out);
		return writeAsBytes;
	}

	public static <TT extends Object> WriteAsBytes<TT> newReader(IBasicFileSchema schema, ISerializer<TT> serializer) {
		return new WriteAsBytes<TT>(schema, serializer);
	}
	
	public static <TT extends Object> WriteAsBytes<TT> newReader(IBasicFileSchema schema, ISerializer<TT> serializer, OutputStream out) 
			throws IOException {
		WriteAsBytes<TT> writeAsBytes = new WriteAsBytes<TT>(schema, serializer);
		writeAsBytes.open(out);
		return writeAsBytes;
	}

}
