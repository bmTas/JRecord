package net.sf.JRecord.Common;

public interface IGetFieldByName {

	/**
	 * Get a specific field definition
	 * @param name of the field
	 * @return requested field
	 */
	public abstract IFieldDetail getField(String fieldName);

}
