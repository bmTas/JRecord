package net.sf.JRecord.Common;

/**
 * Very Basic RecordLayout interface. It is used by the field Definitions to get
 * Data from the RecordDefinition that it belongs to
 *
 * @author Bruce Martin
 *
 */
public interface AbstractRecord {

	/**
	 * Get quote
	 * @return Get quote
	 */
	public abstract String getQuote();

	/**
	 * @return the parentRecordIndex
	 */
	public abstract int getParentRecordIndex();

	/**
	 * @return the recordStyle
	 */
	public abstract int getRecordStyle();

	/**
	 * @return the sourceIndex
	 */
	public abstract int getSourceIndex();
}