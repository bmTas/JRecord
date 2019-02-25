package net.sf.JRecord.cbl2csv.imp;

import java.io.BufferedWriter;
import java.io.IOException;

import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.cbl2csv.args.IUpdateFieldName;

public interface ICobolToCsvBldr {

	/**
	 * @param schema the schema to set
	 * @return this builder
	 */
	ICobolToCsvBldr setSchema(LayoutDetail schema);

	/**
	 * @param seperator the separator to set
	 */
	ICobolToCsvBldr setSeparator(String seperator);

	/**
	 * @param quote the quote to set
	 * @return this builder
	 */
	ICobolToCsvBldr setQuote(String quote);

	/**
	 * @param writeRecordName the writeRecordName to set
	 * @return this builder
	 */
	ICobolToCsvBldr setWriteRecordName(boolean writeRecordName);

	/**
	 * Add record Definition and writer for the file
	 * 
	 * @param recordName record name
	 * @param writer file writer to get the Csv record.
	 * @return this builder
	 */
	ICobolToCsvBldr addRecordDetails(String recordName, BufferedWriter writer);

	/**
	 * Set the filename and record variable. The file name <b>must</b> contain
	 * the <i>record variable</i>. 
	 * 
	 * if you have 3 record types (<i>HEADER, DETAIL, TRAILER</i>).
	 * If<ul>
	 * <li><b>fileName:</b> output_$record..csv
	 * <li><b>recordVariable:</b> $record.
	 * </ul>
	 * Then three output files will be produced:
	 * <ul>
	 *   <li>
	 * </ul>  
	 *   
	 * @param filename Output Csv File Name
	 * @param recordVariable 
	 * @return
	 */
	ICobolToCsvBldr setOutputFile(String filename, String recordVariable);

	/**
	 * 
	 * @param filename
	 * @return this builder
	 */
	ICobolToCsvBldr setOutputFile(String filename);

	/**
	 * Define the default Csv Writer. This will be used for any record
	 * that does not have a writer defined
	 * 
	 * @param csvWriter default Csv Writer
	 * @return this builder 
	 */
	ICobolToCsvBldr setCsvWriter(BufferedWriter csvWriter);

	
	/**
	 * Write Csv column headers on the first line ???
	 * 
	 * @param csvHeader wether to write Csv Header details
	 * 
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setCsvHeader(boolean csvHeader);

	/**
	 * Define the input file (Cobol data file ??)
	 * 
	 * @param reader input file reader
	 * 
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setLineReader(AbstractLineReader reader);

	/**
	 * Set the character-set / font / encoding
	 * 
	 * @param outputCharacterset output character set
	 * 
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setOutputCharacterSet(String outputCharacterset);

	/**
	 * Set the update field /column name class.
	 * @param updateFieldName class to update the Csv Field / Column names
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setUpdateFieldName(IUpdateFieldName updateFieldName);

	/**
	 * run the Cobol to Csv Conversion
	 * 
	 * @throws IOException any Io Error that occurs
	 */
	void run() throws IOException;

	/**
	 * 
	 * @param numericSpaces text to display for numeric field is spaces
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setNumericSpacesTxt(String numericSpaces);

	/**
	 * 
	 * @param highValueTxt text to display if there are high values in a field
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setHighValueTxt(String highValueTxt);

	/**
	 * 
	 * @param lowValueTxt text to display if there are low values in a field
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setLowValueTxt(String lowValueTxt);

	/**
	 * 
	 * @param reportInvalidFields set wether to report invalid fields (low-values / high-values / spaces in numeric) or not
	 * @return this builder for more updates
	 */
	ICobolToCsvBldr setReportInvalidFields(boolean reportInvalidFields);

}