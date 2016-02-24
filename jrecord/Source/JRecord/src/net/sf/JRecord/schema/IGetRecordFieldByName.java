package net.sf.JRecord.schema;

import net.sf.JRecord.Common.IFieldDetail;

/**
 * Retrieve field Description using a Record_Name / Field_Name
 * 
 * @author Bruce Martin
 *
 */
public interface IGetRecordFieldByName {
	
	/**
	 * Get a field description
	 * 
	 * @param recordName Record Name the field is in
	 * @param fieldName field Name
	 * @param indexs any array index's
	 * 
	 * @return the field
	 */
	public abstract IFieldDetail getField(String recordName, String fieldName, int[] indexs);
}
