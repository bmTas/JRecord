package net.sf.JRecord.cgen.impl.io;

import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import net.sf.JRecord.Common.IBasicFileSchema;
import net.sf.JRecord.Details.IGetByteData;
import net.sf.JRecord.cgen.def.IDeserializer;
import net.sf.JRecord.cgen.def.IReader;
import net.sf.JRecord.cgen.def.IWriter;
import net.sf.JRecord.cgen.impl.derSer.GetBytesSerializer;

public class IoProvider<ReadClass, WriteClass extends IGetByteData> {

	private final IBasicFileSchema schema;
	private final IDeserializer<ReadClass> deserializer;
	
	public IoProvider(IBasicFileSchema schema, IDeserializer<ReadClass> deserializer) {
		super();
		this.schema = schema;
		this.deserializer = deserializer;
	}
	
	/**
	 * Create Reader for a file
	 * @param filename input file name
	 * @return Cobol Record Reader 
	 * @throws IOException
	 */
	public  IReader<ReadClass> newReader(String filename) throws IOException {
		return newReader(new FileInputStream(filename));
	}
	
	/**
	 * Create Reader for a input stream
	 * @param in input stream
	 * @return Cobol Record Reader 
	 * @throws IOException
	 */
	public  IReader<ReadClass> newReader(InputStream in) throws IOException {
		return ReadFromBytes.newReader(schema, deserializer, in);
	}


	public IWriter<WriteClass> newWriter(String filename) throws IOException {
		return newWriter(new FileOutputStream(filename));
	}


	public IWriter<WriteClass> newWriter(OutputStream out) throws IOException {
		WriteAsBytes<WriteClass> writeAsBytes = new WriteAsBytes<WriteClass>(schema, new GetBytesSerializer<WriteClass>());
		writeAsBytes.open(out);
		return writeAsBytes;
	}

}
