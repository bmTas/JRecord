package net.sf.JRecord.Common;


public interface AbstractRecordX<FieldDefinition extends IFieldDetail> extends AbstractRecord {

	/**
	 * Get a specific field definition
	 * @param idx index of the required field
	 * @return requested field
	 */
	public abstract FieldDefinition getField(int idx);

	/**
	 * get the number of fields in the record
	 *
	 * @return the number of fields in the record
	 */
	public abstract int getFieldCount();


	/**
	 * Get a specific field definition (using the field name)
	 *
	 * @param fieldName record name being searched for
	 *
	 * @return index of the record
	 */
	public abstract FieldDefinition getField(String fieldName);

}