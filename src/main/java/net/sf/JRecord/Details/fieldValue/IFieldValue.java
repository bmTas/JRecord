package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Common.AbstractFieldValue;

/**
 * Field Value used in line
 * 
 * @author Bruce Martin
 *
 */

public interface IFieldValue extends AbstractFieldValue,
	IDecimalField, ILongField, IStringField, IBigIntegerField {
	
	/**
	 * Set the field to Cobol `High-Values` - each byte is set to x'FF'
	 */
	public abstract void setToHighValues();

	
	/**
	 * Set the field to Cobol Low-Values` - each byte is set to x'00'
	 */
	public abstract void setToLowValues();

	/**
	 * 
	 * @param hexString hex value to assign to the field. For binary files, the Typer is ignored
	 * and the field is set to this exact value.
	 * 
	 */
	public abstract void setHex(String hexString);

	/**
	 * Check if it is a valid field in this line (record). This method will check:<ul>
	 * <li>Check the field position + field-Length < line (Record) length
	 * <li>Check the field value is not low-values (non comp fields)
	 * <li>For occurs depending array fields check the array index is less than the
	 * maximum array-index for this line (record) defined by the occurs depending field.
	 * </ul>
	 * 
	 * In Cobol <i>Occurs depending arrays</i> are variable size arrays with the array size determined by a field.
	 * These arrays can vary in size from one line to the next. This means an an index may be valid in one line but 
	 * not the next
	 * 
	 * <pre>
	 * Occurs Depending in Cobol:
	 * 
	 *       03  count           pic s99 comp.
	 *       03  field-1 occurs 1 to 5 depending on count
	 *                           pic xx.
	 * <pre>
	 * 
	 * If <b>Count=2</b> the array will only have 2 elements and fields
	 * with index's >= 2 will not exist in the array. This method checks for
	 * index's >= count variable (for occurs depending arrays.                           
	 * 
	 * @return whether the array-field exist in the line (Record). i.e. 
	 * the array-index < size specified on the occurs depending variable
	 */
	public abstract boolean isFieldPresent();

	/**
	 * Whether the value is "High-Values"
	 * 
	 * @return  is "High-Values"
	 */
	public abstract boolean isHighValues();

	/**
	 * Whether the field is "Low-Values"
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
	 * in an Occurs-Depending Array
	 */
	public abstract boolean isFieldInRecord();
	

	/**
	 * Set the fieldValue to spaces
	 */
	public void setToSpaces();
}
