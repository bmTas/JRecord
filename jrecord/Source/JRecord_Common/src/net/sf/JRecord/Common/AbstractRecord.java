package net.sf.JRecord.Common;

import net.sf.JRecord.External.Def.DependingOnDtls;

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

	/**
	 * Calculate the actual position (adjusting for any depending on values
	 * @param pos initial unadjusted position
	 * @return adjusted position
	 */
	public abstract int calculateActualPosition(AbstractIndexedLine line, DependingOnDtls dependingOnDtls, int pos);
}