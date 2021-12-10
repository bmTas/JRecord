package net.sf.JRecord.Details.fieldValue;

public interface IStringField extends IFieldAttributes {

	/**
	 * Get Field value as a String
	 * @return field value
	 */
	public String asString();

	/**
	 * Set the fields value
	 * @param value new value
	 */
	public void set(Object value);

}
