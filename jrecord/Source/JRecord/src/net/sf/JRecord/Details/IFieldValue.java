package net.sf.JRecord.Details;

import net.sf.JRecord.Common.AbstractFieldValue;


/**
 * Extended FieldValue interface for us in JRecord.
 * It mainly contains high/low value methods 
 * 
 * @author Bruce Martin
 *
 */
public interface IFieldValue extends AbstractFieldValue {

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
