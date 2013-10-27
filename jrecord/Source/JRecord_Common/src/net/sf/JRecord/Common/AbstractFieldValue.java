package net.sf.JRecord.Common;

import java.math.BigDecimal;
import java.math.BigInteger;


/**
 * Interface to one field from a line (or Record).
 * A FieldValue can be used to
 * <ul compact>
 *  <li>Get the value of the  value in a variety of formats.
 *  <li>Update the value of the field (Can use Long, Float, Double, Object etc).
 * </ul>
 * 
 * <p>Getting a field value:
 * <pre>
 * 	            long sku = saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").asLong();
 * </pre>
 * 
 * <p>Updating a field:
 * <pre>
 * 	            saleRecord.getFieldValue("<font color="blue"><b>KEYCODE-NO</b></font>").set(1331);
 * </pre>

 * @author Bruce Martin
 *
 */
public interface AbstractFieldValue {

	/**
	 * Get Field value as a String
	 * @return field value
	 */
	public String asString();
	
	/**
	 * Field value as long
	 * @return field value
	 */
	public long asLong();
	
	
	/**
	 * Field value as an integer
	 * @return value as an integer
	 */
	public int asInt();

	/**
	 * Field value as a double
	 * @return field value
	 */
	public double asDouble();

	/**
	 * field value as float
	 * @return field value
	 */
	public float asFloat();
	
	/**
	 * Field value as a Boolean
	 * @return Field value as a Boolean
	 */
	public boolean asBoolean();


	/**
	 * Field value as a BigInteger
	 */
	public BigInteger asBigInteger();
	
	/**
	 * Field value as a Big Decimal
	 * @return field value
	 */
	public BigDecimal asBigDecimal();
	
	/**
	 * get Hex equivalent of field
	 * @return Hex equivalent of field
	 */
	public String asHex();
	
	
	/**
	 * Set the fields value
	 * @param value new value
	 */
	public void set(Object value) throws RecordException;
	
	/**
	 * Set the fields value
	 * @param value new value
	 */
	public void set(long value) throws RecordException;
	
	/**
	 * Set the fields value
	 * @param value new value
	 */
	public void set(double value) throws RecordException;

	/**
	 * Set the fields value
	 * @param value new value
	 */
	public void set(float value) throws RecordException;

	/**
	 * Set the field Value
	 * @param value new value
	 * @throws RecordException
	 */
	public void set(boolean value) throws RecordException;

	public abstract IFieldDetail getFieldDetail();

	public abstract boolean isBinary();

	public abstract boolean isNumeric();

	public abstract String getTypeName();

}
