package net.sf.JRecord.cbl2csv.imp.sr;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.External.ExternalRecord;
import net.sf.JRecord.External.ICsvSchemaBuilder;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.TextLineReader;
import net.sf.JRecord.charIO.StandardCharReader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;

public class Csv2CobolSrBuilder extends BasicOutputRecord<Csv2CobolSrBuilder> {

	private Reader reader;
	private final ISchemaIOBuilder  ioBuilder;
	private boolean columnNamesOnFirstLine = true;
	private String fileName;
	private String csvFieldSeperator = ",",
			       csvQuote="\"";

	public Csv2CobolSrBuilder(ISchemaIOBuilder ioBuilder) throws IOException {
		super(ioBuilder.getLayout(), 0, null, null, "", null, null);
		this.ioBuilder = ioBuilder;
	}
	
	public Csv2CobolSrBuilder(LayoutDetail schema) {
		super(schema, 0, null, null, "", null, null);
		this.ioBuilder = JRecordInterface1.SCHEMA.newIOBuilder(schema);
	}
	
	/**
	 * set the filename of the input Csv data file
	 * @param fileName filename of the input Csv data file
	 * @return this builder for more updates
	 * @throws FileNotFoundException
	 */
	public Csv2CobolSrBuilder setInputCsvFileName(String csvFileName) throws FileNotFoundException {
		return setInputCsvReader(new FileReader(csvFileName));
	}
	
	/**
	 * Define the Source of Csv data
	 * @param in Stream containing Csv Data File
	 * @return
	 */
	public Csv2CobolSrBuilder setInputCsvReader(Reader csvInputData) {
		this.reader = csvInputData;
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
	
	public Csv2CobolSrBuilder setCsvEncoding(String encoding) {
		super.encoding = encoding;
		return this;
	}

	/**
	 * @param csvFieldSeperator the csvFieldSeperator to set
	 */
	public void setCsvFieldSeperator(String csvFieldSeperator) {
		this.csvFieldSeperator = csvFieldSeperator;
	}

	/**
	 * @param csvQuote the csvQuote to set
	 */
	public void setCsvQuote(String csvQuote) {
		this.csvQuote = csvQuote;
	}

	/**
	 * @param columnNamesOnFirstLine the columnNamesOnFirstLine to set
	 */
	public Csv2CobolSrBuilder setColumnNamesOnFirstLine(boolean columnNamesOnFirstLine) {
		this.columnNamesOnFirstLine = columnNamesOnFirstLine;
		return this;
	}

	/**
	 * Write the Cobol data File using the supplied parameters
	 * @param cobolDataFileName file name of the Cobol data to be written
	 * @return This builder
	 * @throws IOException
	 */
	public Csv2CobolSrBuilder writeCobolData(String cobolDataFileName) throws IOException {

		return writeCobolData(new FileOutputStream(cobolDataFileName));
	}

	/**
	 * 
	 * @param cobolDataStream
	 * @return
	 * @throws IOException
	 */
	public Csv2CobolSrBuilder writeCobolData(OutputStream cobolDataStream) throws IOException {
		
		if (reader == null) {
			if (fileName == null || fileName.length() == 0) {
				throw new RuntimeException("You must supply an input file");
			}
			
			if (encoding == null || encoding.length() == 0) {
				reader = new FileReader(fileName);
			} else {
				reader = new InputStreamReader(new FileInputStream(fileName), encoding);
			}
		}
		if (cobolDataStream == null) {
			throw new RuntimeException("You must supply an output file");
		}
		
		if (columnNamesOnFirstLine) {
			processFileWithColumnNames(cobolDataStream);
		} else {
			
		}
		
		super.writeHeader();
		
		reader.close();
		//writer = null;
		reader = null;
		
		
		return this;
	}
	
	private void processFileWithColumnNames(OutputStream out) throws IOException {
		AbstractLineWriter writer = ioBuilder.newWriter(out);
		StandardCharReader charReader = new StandardCharReader();
		charReader.open(reader);
		ICsvSchemaBuilder csvRec = ExternalRecord.newCsvRecord(
				"CsvSource", 
				IFileStructureConstants.IO_NAME_1ST_LINE,
				encoding, csvFieldSeperator, csvQuote);
		TextLineReader csvReader = new TextLineReader(null, true, charReader);
		csvReader.openWithSuppliedReader(csvRec.asLayoutDetail());		
		
		writer.close();
	}
}
