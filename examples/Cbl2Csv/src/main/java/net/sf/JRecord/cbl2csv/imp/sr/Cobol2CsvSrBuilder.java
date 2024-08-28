package net.sf.JRecord.cbl2csv.imp.sr;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

public class Cobol2CsvSrBuilder extends BasicOutputRecord<Cobol2CsvSrBuilder> {

	private InputStream in;
	private final ISchemaIOBuilder  ioBuilder;

	public Cobol2CsvSrBuilder(ISchemaIOBuilder ioBuilder) throws IOException {
		super(ioBuilder.getLayout(), 0, null, null, "", null, null);
		this.ioBuilder = ioBuilder;
	}
	
	public Cobol2CsvSrBuilder(LayoutDetail schema) {
		super(schema, 0, null, null, "", null, null);
		this.ioBuilder = JRecordInterface1.SCHEMA.newIOBuilder(schema);
	}
	
	
	/**
	 * set the filename of the input Cobol data file
	 * @param fileName filename of the input Cobol data file
	 * @return this builder for more updates
	 * @throws FileNotFoundException
	 */
	public Cobol2CsvSrBuilder setInputFileName(String fileName) throws FileNotFoundException {
		return setInputStream(new FileInputStream(fileName));
	}
	
	/**
	 * Define the Source of Cobol data
	 * @param in Stream containing Cobol Data File
	 * @return
	 */
	public Cobol2CsvSrBuilder setInputStream(InputStream in) {
		this.in = in;
		return this;
	}
	
//	public Cobol2CsvSrBuilder setOutputFileName(String fileName) throws IOException {
//		return setOutputWriter(new FileWriter(fileName));
//	}
//	
//	public Cobol2CsvSrBuilder setOutputWriter(Writer writer) {
//		super.writer = writer;
//		return this;
//	}
	
	public Cobol2CsvSrBuilder setCsvEncoding(String encoding) {
		super.encoding = encoding;
		return this;
	}

	/**
	 * Write the Csv File using the supplied parameters
	 * @param csvFileName
	 * @return
	 * @throws IOException
	 */
	@SuppressWarnings("resource")
	public Cobol2CsvSrBuilder writeCsv(String csvFileName) throws IOException {
		if (encoding == null | encoding.length() == 0) {
			return writeCsv(new FileWriter(csvFileName));
		}
		
		return writeCsv(new OutputStreamWriter(new FileOutputStream(csvFileName), encoding));
	}

	public Cobol2CsvSrBuilder writeCsv(Writer writer) throws IOException {
		
		if (in == null) {
			throw new RuntimeException("You must supply an input file");
		}
		if (writer == null) {
			throw new RuntimeException("You must supply an output file");
		}
		super.writer = writer;
		
		AbstractLine line;
		
		AbstractLineReader reader = ioBuilder.newReader(in);
		
		super.writeHeader();
		
		while ((line = reader.read()) != null) {
			super.writeRecord(line);
		}
		
		writer.close();
		reader.close();
		writer = null;
		in = null;
		
		
		return this;
	}
	

	
}
