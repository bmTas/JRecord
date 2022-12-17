package net.sf.JRecord.util.copy;

import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Details.RecordDetail;

/**
 * Controls updating one field in the output file
 * 
 * @author Bruce Martin
 *
 */
public interface IFieldMapping {

	/**
	 * use last record for a line (or the current line)
	 * @return wether to use the currnt line or line for the record
	 */
	public boolean useRecordBasedLine();
	/**
	 * get the Record definition being used in the source data line
	 * @return the Record definition being used in the source data line
	 */
	public RecordDetail getSourceRecord();
	/**
	 * Get the field definition in the source line  that isto be retrieved
	 * to be assigned the the destination field
	 * @return source field definition
	 */
	public FieldDetail getSourceField();
	/** 
	 * Field definition of the field to getting updated in the output line
	 * @return Destination field in the output record.
	 */
	public FieldDetail getDestinationFieldField();
}
