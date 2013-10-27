package net.sf.JRecord.Common;

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

	public abstract boolean isBinCSV();

	/**
	 * Get the record Seperator bytes
	 *
	 * @return Record Seperator
	 */
	public abstract byte[] getRecordSep();


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


	/**
	 * get the field delimiter
	 * @return the field delimeter
	 */
	public abstract byte[] getDelimiterBytes();

	public abstract String getQuote();
}
