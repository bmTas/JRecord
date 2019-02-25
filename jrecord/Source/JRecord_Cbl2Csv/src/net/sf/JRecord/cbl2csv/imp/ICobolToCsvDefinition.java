package net.sf.JRecord.cbl2csv.imp;

import java.io.BufferedWriter;
import java.io.IOException;
import java.util.List;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.cbl2csv.args.IUpdateFieldName;

public interface ICobolToCsvDefinition {

	/**
	 * @return the schema
	 */
	LayoutDetail getSchema();

	/**
	 * @return the seperator
	 */
	String getSeparator();

	/**
	 * @return the quote
	 */
	String getQuote();

	/**
	 * @return the writeRecordName
	 */
	boolean isWriteRecordName();

	/**
	 * @return the recordList
	 * @throws IOException 
	 */
	List<RecordWriterDetails> getRecordList() throws IOException;

	/**
	 * Get the default CsvWriter
	 * @return default CsvWriter
	 */
	BufferedWriter getCsvWriter();

	/**
	 * Should Csv headings be written ???
	 * @return Csv Heading
	 */
	boolean isCsvHeader();

	/**
	 * Get the input file reader
	 * 
	 * @return  input file reader
	 */
	AbstractLineReader getLineReader();

	/**
	 * get the field/column name updater class
	 * @return field/column name updater class
	 */
	IUpdateFieldName getUpdateFieldName();

	boolean isReportInvalidFields();

	String getNumericSpacesTxt();

	String getHighValueTxt();

	String getLowValueTxt();

}