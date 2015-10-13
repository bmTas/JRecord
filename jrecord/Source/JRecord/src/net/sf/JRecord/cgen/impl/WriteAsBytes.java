package net.sf.JRecord.cgen.impl;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.ByteIO.AbstractByteWriter;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.cgen.def.IEncoder;
import net.sf.JRecord.cgen.def.IWriter;

public class WriteAsBytes<T> implements IWriter<T> {
	
	private final IEncoder<T> encoder;
	private final AbstractByteWriter writer;

	public WriteAsBytes(IBasicFileSchema schema, IEncoder<T> encoder) {
		this.encoder = encoder;
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
		writer.write(encoder.encode(record));
	}

	@Override
	public void close() throws IOException {
		writer.close();
	}

	public static <TT extends Object> WriteAsBytes<TT> newReader(IBasicFileSchema schema, IEncoder<TT> encoder) {
		return new WriteAsBytes<TT>(schema, encoder);
	}
}
