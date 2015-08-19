package net.sf.JRecord.Details;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.External.ExternalConversion;
import net.sf.JRecord.Types.Type;
import net.sf.JRecord.Types.TypeManager;

/**
 * Reference to one field in a line (or Record).
 * It allows the user to get / set the field value using either simple types (int, double etc) or
 * as a Object.
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
public class FieldValue implements IFieldValue {

	private final AbstractLine theLine;
	final IFieldDetail field;
	final int recordNum;
	final int fieldNum;;

	/**
	 * Create a field value
	 *
	 * @param line line the field value belongs to
	 * @param fieldDetails Field Description
	 */
	public FieldValue(AbstractLine line, IFieldDetail fieldDetails) {
		theLine = line;
		field = fieldDetails;
		recordNum = -1;
		fieldNum = -1;
	}

	/**
	 * Create a field value (using Record / Field Index's)
	 *
	 * @param line line the field value belongs to
	 * @param recordIndex record index of the field
	 * @param fieldIndex field index of the field
	 */
	public FieldValue(AbstractLine line, int recordIndex, int fieldIndex) {
		theLine = line;
		recordNum = recordIndex;
		fieldNum = fieldIndex;
		field = null;
	}

	/**
	 * @see IFieldValue#asBigDecimal()
	 */
	@Override
	public BigDecimal asBigDecimal() {
		Object ret = getValue();

		if (ret == null) {
			return null;
		} else if (ret instanceof BigDecimal) {
			return (BigDecimal) ret;
		} else {
			return new BigDecimal(ret.toString());
		}
	}

	/**
	 * @see IFieldValue#asBigInteger()
	 */
	@Override
	public BigInteger asBigInteger() {
		Object ret = getValue();

		if (ret == null) {
			return null;
		} else if (ret instanceof BigInteger) {
			return (BigInteger) ret;
		} else {
			return new BigInteger(ret.toString());
		}
	}

	/**
	 * @see IFieldValue#asDouble()
	 */
	@Override
	public double asDouble() {
		Object ret = getValue();

		if (ret == null) {
			return 0;
		} else if (ret instanceof Number) {
			return ((Number) ret).doubleValue();
		} else {
			return Double.parseDouble(ret.toString());
		}
	}


	/**
	 * @see IFieldValue#asFloat()
	 */
	@Override
	public float asFloat() {
		Object ret = getValue();

		if (ret == null) {
			return 0;
		} else if (ret instanceof Number) {
			return ((Number) ret).floatValue();
		} else {
			return Float.parseFloat(ret.toString());
		}
	}

	/**
	 * @see IFieldValue#asLong()
	 */
	@Override
	public long asLong() {
		Object ret = getValue();

		if (ret == null) {
			return 0;
		} else if (ret instanceof Number) {
			return ((Number) ret).longValue();
		} else {
			return Long.parseLong(ret.toString());
		}
	}


	/**
	 * @see IFieldValue#asInt()
	 */
	@Override
	public int asInt() {
		return (int) asLong();
	}



	/**
	 * @see IFieldValue#asBoolean()
	 */
	@Override
	public boolean asBoolean() {
		Object ret = getValue();

		if (ret == null) {
			return false;
		} else if (ret instanceof Boolean) {
			return ((Boolean) ret).booleanValue();
		} else {
			return Boolean.parseBoolean(ret.toString());
		}
	}


	/**
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return asString();
	}


	@Override
	public String asString() {
		Object ret = getValue();

		if (ret == null) {
			return "";
		}
		return ret.toString();
	}

	/**
	 * Get The fields value
	 * @return fields value
	 */
	@SuppressWarnings("deprecation")
	private Object getValue() {
		if (recordNum >= 0) {
			return theLine.getField(recordNum, fieldNum);
		}
		if (field == null) {
			return null;
		}
		return theLine.getField(field);
	}

	/**
	 * @see IFieldValue#asHex()
	 */
	@SuppressWarnings("deprecation")
	@Override
	public String asHex() {
		IFieldDetail fld = field;
		if (recordNum >= 0) {
			fld = theLine.getLayout().getField(recordNum, fieldNum);
		}
		if (theLine instanceof BaseLine) {
			return ((BaseLine) theLine).getField(Type.ftHex, fld).toString();
		}
		return theLine.getLayout().getField(theLine.getData(),
				Type.ftHex,
				fld).toString();
	}

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.AbstractFieldValue#set(net.sf.JRecord.Common.AbstractFieldValue)
	 */
	@Override
	public void set(AbstractFieldValue value) throws RecordException {
		set(value.asString());
	}

	/**
	 * @see IFieldValue#set(boolean)
	 */
	@Override
	public void set(boolean value) throws RecordException {
		set(Boolean.valueOf(value));
	}


	/**
	 * @see IFieldValue#set(double)
	 */
	@Override
	public void set(double value) throws RecordException {
		set(Double.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(float) 
	 */
	@Override
	public void set(float value) throws RecordException {
		set(Float.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(long)
	 */
	@Override
	public void set(long value) throws RecordException {
		set(Long.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(java.lang.Object)
	 */
	@SuppressWarnings("deprecation")
	@Override
	public void set(Object value) throws RecordException {
		if (recordNum >= 0) {
			theLine.setField(recordNum, fieldNum, value);
		} else if (field == null) {
			throw new RuntimeException("Unknown Field !!!");
		} else {
			theLine.setField(field, value);
		}
	}

	/**
	 * Get the Type name
	 * @return Type name
	 */
	@Override
	public String getTypeName() {
		return ExternalConversion.getTypeAsString(0, getFieldDetail().getType());
	}

	/**
	 * Wether it is a Numeric field
	 * @return is a Numeric field
	 */
	@Override
	public boolean isNumeric() {
		return getType().isNumeric();
	}

	/**
	 * Wether it is a binary Field
	 * @return is a binary field
	 */
	@Override
	public boolean isBinary() {
		return getType().isBinary();
	}
	
	@Override
	public boolean isFieldPresent() {
		
		if (recordNum >= 0) {
			return this.theLine.isDefined(recordNum, fieldNum);
		}
		if (field == null) {
			return false;
		} 
		
		return this.theLine.isDefined(field);
	}

	private Type getType() {
		return TypeManager.getInstance().getType(getFieldDetail().getType());
	}

	/**
	 * Get The field Definition
	 * @return Field Definition
	 */
	@Override
	public IFieldDetail getFieldDetail() {
		if (field != null) {
			return field;
		}
		return theLine.getLayout().getRecord(recordNum).getField(fieldNum);
	}
	
	@Override
	public boolean isByteRecord() {
		return theLine instanceof Line;
	}
	
	
	@Override
	public boolean isLowValues() {
		return false;
	}
	
	@Override
	public boolean isHighValues() {
		return false;
	}
	
	@Override
	public void setHex(String s) throws RecordException {
		throwError();
	}
	
	@Override
	public void setToLowValues() {
		throwError();
	}
	
	
	@Override
	public void setToHighValues() {
		throwError();
	}
	
	private void throwError() {
		String s = "";
		if (theLine != null) {
			s = theLine.getClass().getName();
		}
		
		throw new RuntimeException("Operation is not supported for a " + s);
	}
}
