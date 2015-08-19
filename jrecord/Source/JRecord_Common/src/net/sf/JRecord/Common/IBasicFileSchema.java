package net.sf.JRecord.Common;

/**
 * <p>A minimal <i>File Schema</i> definition. It provides a
 * minimal set of definitions (like RecordLength & character=set)
 * for creating Readers / Writer.
 * <p>The schema's of JRecord / RecordEditor / ReCsvEditor / ProtoBufEditor
 * extend / implement this interface.
 * 
 * @author Bruce Martin
 *
 */
public interface IBasicFileSchema {

	/**
	 * Return the file structure
	 *
	 * @return file structure
	 */
	public abstract int getFileStructure();

	/**
	 * wether it is a binary record
	 *
	 * @return wether it is a binary record
	 */
	public abstract boolean isBinary();

//	public abstract boolean isBinCSV();

//	/**
//	 * Get the record Seperator bytes
//	 *
//	 * @return Record Seperator
//	 */
//	public abstract byte[] getRecordSep();


	/**
	 * Get the Canonical Name (ie Font name)
	 *
	 * @return Canonical Name (ie Font name)
	 */
	public abstract String getFontName();

	/**
	 * get the field delimiter
	 * @return the field delimeter
	 */
	public abstract String getDelimiter();


//	/**
//	 * get the field delimiter
//	 * @return the field delimeter
//	 */
//	public abstract byte[] getDelimiterBytes();

	/**
	 * Get Quote (for CSV files)
	 * @return
	 */
	public abstract String getQuote();
	
	/**
	 * Get the maximum length of the Layout
	 *
	 * @return the maximum length
	 */
	public abstract int getMaximumRecordLength();

}
