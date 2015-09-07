package net.sf.JRecord.Details;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.RecordException;


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

	public abstract void setHex(String s) throws RecordException;

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
}
