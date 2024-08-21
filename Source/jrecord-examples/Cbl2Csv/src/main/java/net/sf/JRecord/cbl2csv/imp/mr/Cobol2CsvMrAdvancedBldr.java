package net.sf.JRecord.cbl2csv.imp.mr;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.schema.CobolSchemaReader;

public class Cobol2CsvMrAdvancedBldr extends CobolSchemaReader<Cobol2CsvMrAdvancedBldr> {

	//private ArrayList<OutputFile> outputFiles = new ArrayList<OutputFile>();
	String fieldSeparator=",", quote="\"";
	
	/**
	 * Create new <i>Cobol to  Csv Writer</i>
	 * @param copybookName Cobol-Copybook name
	 * @return Csv-Writer
	 */
	public static Cobol2CsvMrAdvancedBldr newCsvWriter(String copybookName) {
		Cobol2CsvMrAdvancedBldr csvWriter = new Cobol2CsvMrAdvancedBldr(copybookName, new CobolCopybookLoader());
		csvWriter.addCopyBook(copybookName);
		return csvWriter;
	}

	/**
	 * Create new <i>Cobol to  Csv Writer</i>
	 * @param copybookReader Reader for the Cobol Copybook
	 * @param copybookName Name of the Cobol-Copybook
	 * @return  Csv-Writer
	 */
	public static Cobol2CsvMrAdvancedBldr newCsvWriter(Reader copybookReader, String copybookName) {
		Cobol2CsvMrAdvancedBldr csvWriter = new Cobol2CsvMrAdvancedBldr(copybookName, new CobolCopybookLoader());
		csvWriter.addCopyBook(copybookReader, copybookName);
		return csvWriter;
	}

	private Cobol2CsvMrAdvancedBldr(String copybookName, ICopybookLoaderCobol loader) {
		super(copybookName, loader);
	}

	/**
	 * Define the default field-separator to be used in the Csv files. It can be over-ridden
	 * for individual files
	 * 
	 * @param fieldSeperator default field-separator
	 * 
	 * @return CsvDefinition object for more updates
	 */
	public Cobol2CsvMrAdvancedBldr setFieldSeparator(String fieldSeperator) {
		this.fieldSeparator = fieldSeperator;
		return this;
	}

	/**
	 * Define the default quote character to be used in the Csv files. It can be over-ridden
	 * for individual file
	 * 
	 * @param quote quote character.
	 * @return  CsvDefinition object for more updates
	 */
	public Cobol2CsvMrAdvancedBldr setQuote(String quote) {
		this.quote = quote;
		return this;
	}


	/**
	 * Create an output definition
	 * 
	 * @param inputFileName file to be read
	 * @return The output definition builder
	 * @throws IOException
	 */
	public OutputDefinition newOutputDefinition(String inputFileName) throws IOException {
		return new OutputDefinition(this, super.newReader(inputFileName));
	}

	/**
	 * Create an output definition
	 * 
	 * @param in stream to be read
	 * @return The output definition builder
	 * @throws IOException
	 */
	public OutputDefinition newOutputDefinition(InputStream in) throws IOException {
		return new OutputDefinition(this, super.newReader(in));
	}

//	
//	private static class OutputField {
//		private final String columnHeading;
//	}
}
