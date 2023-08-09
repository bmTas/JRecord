package net.sf.JRecord.cbl2csv.imp.sr;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.CsvParser.BasicCsvLineParserExtended;
import net.sf.JRecord.CsvParser.CsvDefinition;
import net.sf.JRecord.CsvParser.ICsvDefinition;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;

public class BasicOutputRecord<RetClass extends BasicOutputRecord<RetClass>> {
	protected final int generateIndex;
	protected Writer writer;
	protected final LayoutDetail schema;
	
	BasicCsvLineParserExtended csvParser = BasicCsvLineParserExtended.getInstance();
	CsvDefinition csvDef;
	private int lineNumber;
	
	//private

	private String fieldSeparator;
	private String quote;
	protected String encoding;
	private boolean generateHeader = true;
	
	//private ConversionErrorAction conversionErrorAction = ConversionErrorAction.PassErrorThrough;
	//private String fieldErrorText = "";
	
	private IRetrieveFieldValue retrieveFieldMethod = FieldConversion.FIELD_CONVERSION_REPLACE_ERRORS_WITH_NULL;

	
	private final ArrayList<OutputField> fields = new ArrayList<OutputField>();
	private ArrayList<OutputField> reportFields;

	
	@SuppressWarnings("unchecked")
	private RetClass self = (RetClass) this;
	
//	private static final IRetrieveFieldValue NORMAL_FIELD_VALUE = new IRetrieveFieldValue() {
//		@Override public String getFieldValue(int lineNumber, IFieldValue fieldValue) {
//			return fieldValue.asString();
//		}		
//	};
//	
//	private static final IRetrieveFieldValue NULL_IF_ERROR_FIELD_VALUE = new RetrieveFieldValueErrorString("", false);

	public BasicOutputRecord(LayoutDetail schema,  int generateIndex,
			Writer writer, BasicCsvLineParserExtended csvParser,
			String encoding, String fieldSeparator, String quote) {
		super();
		this.schema = schema;
		this.generateIndex = generateIndex;
		this.writer = writer;
		this.encoding = encoding;
		this.fieldSeparator = fieldSeparator == null ? "," : fieldSeparator;
		this.quote = quote== null ? "\"" : quote;

		setCsvParser(csvParser);
	}

	/**
	 * define the Csv Field separator
	 * @param fieldSeparator field separator character
	 * @return File Definition for further updates
	 */
	public RetClass setFieldSeperator(String fieldSeparator) {
		this.fieldSeparator = fieldSeparator;
		csvDef = null;
		return self;
	}

	/**
	 * Set Csv Quote to be used in the Csv file
	 * @param quote Quote character
	 * @return File Definition for further updates
	 */
	public RetClass setQuote(String quote) {
		this.quote = quote;
		csvDef = null;
		return self;
	}

	/**
	 * @param csvParser the csvParser to set
	 */
	public RetClass setCsvParser(BasicCsvLineParserExtended csvParser) {
		this.csvParser = csvParser == null ? BasicCsvLineParserExtended.getInstance() : csvParser;
		return self;
	}


	/**
	 * Define how Cobol-Fields are converted to string and how errors are handled.
	 * Typical values of fieldConversion are<ul>
	 *  <li><b>FieldConversion.NORMAL_CONVERSION</b> - convert fields to string and terminate
	 *  the program on the first conversion error.
	 *  <li><b>FieldConversion.FIELD_CONVERSION_REPLACE_ERRORS_WITH_NULL</b> Any fields with
	 *  conversion errors are converted to "", the program will continue.
	 *  <li><b>FieldConversion.FIELD_CONVERSION_LOG_ERRORS_AND_REPLACE_WITH_NULL</b> Any fields with
	 *  conversion errors are converted to "", A message is written to System.err. The program will continue.
	 *  <li><b>FieldConversion.createFieldConversion(boolean logErrors, String errorText)</b> 
	 *  Create field conversion with a user supplied Conversion error Text
	 * </ul>
	 * @param retrieveField Class to convert a `FieldValue` to a string
	 */
	public RetClass setRetrieveField(IRetrieveFieldValue fieldConversion) {
		this.retrieveFieldMethod = fieldConversion;
		return self;
	}

	/**
	 * Set Whether a CSV header row is written or not
	 * @param generateHeader  Whether a CSV header row is written or not
	 * @return Builder for more updates
	 */
	public RetClass setGenerateHeader(boolean generateHeader) {
		this.generateHeader = generateHeader;
		return self;
	}

	/**
	 * Add a field from the current line to the outputCsv file
	 * @param fieldName field-name
	 * @return File Definition for further updatesCobolSchemaReader
	 */
	public RetClass addField(String fieldName) {
		
		addField(null, generateIndex, fieldName); 
		
		return self;
	}

	/**
	 * Add a field from the current line to the output-Csv file
	 * @param columnName column name to use for the field
	 * @param fieldName field-name
	 * @return File Definition for further updates
	 */
	public RetClass addField(String columnName, String fieldName) {
		
		addField(columnName, generateIndex, fieldName); 
		
		return self;
	}

	/**
	 * Add multiple fields from the current line
	 * @param fieldNames list of field names
	 * @return File Definition for further updates
	 */
	public RetClass addFieldList(String... fieldNames) {
		
		addFields(generateIndex, fieldNames);
		
		return self;
	}

	/**
	 * Add all the fields from the current line
	 * 
	 * @return File Definition for further updates
	 */
	public RetClass addAllFields() {
		
		addAllFieldsToList(fields);
		return self;
	}

	/**
	 * @param fieldsList
	 */
	protected void addAllFieldsToList(ArrayList<OutputField> fieldsList) {
		RecordDetail record = schema.getRecord(generateIndex);
	
		for (int i = 0; i < record.getFieldCount(); i++) {
			fieldsList.add(new OutputField(null, generateIndex, record.getField(i)));
		}
	}

	/**
	 * @param recordIdx
	 * @param fieldNames
	 */
	protected void addFields(int recordIdx, String[] fieldNames) {
		for (String fldName : fieldNames) {
			addField(null, recordIdx, fldName);
		}
	}

	/**
	 * @param schema schema
	 * @param columnName column name to use in Csv File
	 * @param recordIdx record Index
	 * @param fieldName field name
	 */
	protected void addField(String columnName, int recordIdx, String fieldName) {
		FieldDetail fld = schema.getRecord(recordIdx).getField(fieldName);
		fields.add(new OutputField(columnName, recordIdx, fld));
	}

	/**
	 * Write the header to the output file
	 * @throws IOException
	 */
	protected void writeHeader() throws IOException {
		
		checkFields();
		lineNumber = 1;
		if (this.generateHeader) {
			ArrayList<String> colHeaders = new ArrayList<String>(reportFields.size());
			for (OutputField f : reportFields) {
				colHeaders.add(f.columnHeading);
			}
			writer.write(csvParser.formatFieldList(colHeaders, getCsvDef(), null));
			writer.write('\n');
		}
	}

	/**
	 * 
	 */
	protected void checkFields() {
		this.reportFields = fields;
		if (reportFields.size() == 0) {
			addAllFieldsToList(reportFields);;
		}
	}

	protected void writeRecord(AbstractLine[] lines) throws IOException {
		
		//checkFields();
		ArrayList<String> fieldValues = new ArrayList<String>(reportFields.size());

		for (OutputField f : reportFields) {
			String s = "";
			if (lines[f.recordIndex] != null) {
				s = retrieveFieldMethod.getFieldValue(lineNumber++, lines[f.recordIndex].getFieldValue(f.field));
			}
			
			fieldValues.add(s);
		}
		writer.write(csvParser.formatFieldList(fieldValues, getCsvDef(), null));
		writer.write('\n');
	}
	
	void writeRecord(AbstractLine line) throws IOException {
		
		ArrayList<String> fieldValues = new ArrayList<String>(reportFields.size());
		for (OutputField f : reportFields) {
			fieldValues.add(retrieveFieldMethod.getFieldValue(lineNumber++, line.getFieldValue(f.field)));
		}
		writer.write(csvParser.formatFieldList(fieldValues, getCsvDef(), null));
		writer.write('\n');
	}


	ICsvDefinition getCsvDef() {
		if (this.csvDef == null) {
			csvDef = new CsvDefinition(fieldSeparator, quote, ICsvDefinition.NORMAL_SPLIT, reportFields.size(), encoding, true);
		}
		
		return csvDef;
	}

	static class OutputField {
		final String columnHeading;
		final int recordIndex;
		final FieldDetail field;
		
		public OutputField(String columnHeading, int recordIndex, FieldDetail field) {
			super();
			this.columnHeading = columnHeading == null ? field.getName() : columnHeading;
			this.recordIndex = recordIndex;
			this.field = field;
		}	
	}
//
//	private static class RetrieveFieldValueErrorString implements  IRetrieveFieldValue {
//		private final String errorValue;
//		private final boolean listError;
//		
//		
//		public RetrieveFieldValueErrorString(String errorValue, boolean listError) {
//			super();
//			this.errorValue = errorValue;
//			this.listError = listError;
//		}
//
//
//		@Override public String getFieldValue(int lineNumber, IFieldValue fieldValue) {
//			try {
//				return fieldValue.asString();
//			} catch (Exception e) {
//				if (listError) {
//					System.err.print("Error line: " + lineNumber + " Field: " + fieldValue.getFieldDetail().getName());
//					try {
//						System.err.print( " Hex Value: " + fieldValue.asHex());
//					} catch (Exception e1) {
//					}
//					System.err.println();
//				}
//				return errorValue;
//			}
//		}		
//
//	}
}