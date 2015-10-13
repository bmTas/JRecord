package net.sf.JRecord.cgen.impl;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.ByteIO.AbstractByteReader;
import net.sf.JRecord.ByteIO.ByteIOProvider;
import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.cgen.def.IDecoder;
import net.sf.JRecord.cgen.def.IReader;

public class ReadFromBytes<T> implements IReader<T> {

	public final IDecoder<T> encoder;
	public final AbstractByteReader reader;
	
	public ReadFromBytes(IBasicFileSchema schema, IDecoder<T> decoder) {
		this.encoder = decoder;
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
		return encoder.decode(value);
	}

	@Override
	public void close() throws IOException {
		reader.close();
	}
	
	public static <TT extends Object> ReadFromBytes<TT> newReader(IBasicFileSchema schema, IDecoder<TT> decoder) {
		return new ReadFromBytes<TT>(schema, decoder);
	}
}
