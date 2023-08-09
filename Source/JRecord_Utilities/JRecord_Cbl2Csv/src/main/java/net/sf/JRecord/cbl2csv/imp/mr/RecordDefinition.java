package net.sf.JRecord.cbl2csv.imp.mr;

import java.io.IOException;
import java.io.Writer;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.cbl2csv.imp.sr.BasicOutputRecord;

public class RecordDefinition extends BasicOutputRecord<RecordDefinition> {
	
	
	
	private final OutputDefinition parentDefinition;
//	final int generateIndex;
//	final Writer writer;
//	final LayoutDetail schema;
//	
//	BasicCsvLineParserExtended csvParser = BasicCsvLineParserExtended.getInstance();
//	CsvDefinition csvDef;
//	
//	//private
//
//	String fieldSeparator;
//	String quote;
//	private final String encoding;
//	boolean generateHeader = true;
	
//	final ArrayList<OutputField> fields = new ArrayList<OutputField>();
	//
	
	public RecordDefinition(OutputDefinition parentDefinition, String recordName, Writer writer, 
			String encoding, String fieldSeperator, String quote) {
		super(parentDefinition.getLayout(), parentDefinition.getLayout().getRecordIndex(recordName), 
				writer, null, encoding, fieldSeperator, quote);
		this.parentDefinition = parentDefinition;
		
		if (generateIndex < 0) {
			throw new RuntimeException("Record: " + recordName + " is not defined");
		}
	}

	int getRecordIndex() {
		return generateIndex;
	}

//	String getFieldSeperator() {
//		return fieldSeperator;
//	}


	


//	String getQuote() {
//		return quote;
//	}

	
	
//	/**
//	 * Set the Character-set of character encoding
//	 * @param encoding character-set encoding to be used in the file
//	 * @return
//	 */
//	public FileDefinition setEncoding(String encoding) {
//		this.encoding = encoding;
//		csvDef = null;
//		return this;
//	}

	/**
	 * Add a field (defined by the record and field name). This allows
	 * you to extract fields from past lines of a different record type
	 * 
	 * @param recordName Name of the record that holds the field
	 * @param fieldName Name of the field
	 * @return  File Definition for further updates
	 */
	public RecordDefinition addRecordField(String recordName, String fieldName) {
		int recordIdx = schema.getRecordIndex(recordName);
		addField(null, recordIdx, fieldName);
		
		return this;
	}


	/**
	 * Add a field (defined by the record and field name). This allows
	 * you to extract fields from past lines of a different record type

	 * @param columnName Column name to use in the Csv File
	 * @param recordName Name of the record that holds the field
	 * @param fieldName Name of the field
	 * @return  File Definition for further updates
	 */
	public RecordDefinition addRecordField(String columnName, String recordName, String fieldName) {
		
		int recordIdx = schema.getRecordIndex(recordName);
		if (recordIdx < 0) {
			throw new RuntimeException("Record: " + recordName + " does not exist, FieldName: " + fieldName);
		}
		addField(columnName, recordIdx, fieldName); 
		
		return this;
	}

	/**
	 * Add multiple fields from a previous record
	 * 
	 * @param recordName record-name where the fieldsare from
	 * @param fieldNames fields to be writtent to the output file
	 * @return File Definition for further updates
	 */
	public RecordDefinition addRecordFieldList(String recordName, String... fieldNames) {
		int recordIdx = schema.getRecordIndex(recordName);
		addFields(recordIdx, fieldNames);
		
		return this;
	}

	/**
	 * Return to the output definition to define more records /write the Csv files
	 * @return output-definition for more updates
	 */
	public OutputDefinition endRecordDefinition() {
		return parentDefinition;
	}

	@Override
	protected void writeHeader() throws IOException {
		super.writeHeader();
	}

	@Override
	protected void writeRecord(AbstractLine[] lines) throws IOException {
		super.writeRecord(lines);
	}

	protected Writer getWriter() {
		return writer;
	}
}
