package net.sf.JRecord.Details.fieldValue;


/**
 * Field Value used in line
 * 
 * @author Bruce Martin
 *
 */

@SuppressWarnings("deprecation")
public interface IFieldValue extends net.sf.JRecord.Details.IFieldValue {
	public abstract void setToHighValues();

	public abstract void setToLowValues();

	public abstract void setHex(String s);

	public abstract boolean isFieldPresent();

	/**
	 * wether the value is "High-Values"
	 * 
	 * @return  is "High-Values"
	 */
	public abstract boolean isHighValues();

	/**
	 * wether the field is "Low-Values"
	 * @return if low values
	 */
	public abstract boolean isLowValues();

	
	/**
	 * Test the field value if it is spaces
	 * @return wether the field is spaces
	 */
	public abstract boolean isSpaces();
	
	
	/**
	 * wether hex update operations (setHex setToHigh/Low Values)
	 * are supported 
	 * 
	 * @return wether it is a Byte based record.
	 */
	public abstract boolean isByteRecord();
	
	/**
	 * For Cobol Records it checks wether a field in a Occurs-Depending
	 * array is actually there (i.e. wether the index < current-Max-Index).
	 * 
	 * @return wether the line is actually valid for this Record.
	 * For Cobol Lines it checks the status of Fields
	 * in an Occurs-Depending
	 */
	public abstract boolean isFieldInRecord();
}
