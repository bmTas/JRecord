/*  -------------------------------------------------------------------------
 *
 *                Project: JRecord
 *    
 *    Sub-Project purpose: Provide support for reading Cobol-Data files 
 *                        using a Cobol Copybook in Java.
 *                         Support for reading Fixed Width / Binary / Csv files
 *                        using a Xml schema.
 *                         General Fixed Width / Csv file processing in Java.
 *    
 *                 Author: Bruce Martin
 *    
 *                License: LGPL 2.1 or latter
 *                
 *    Copyright (c) 2016, Bruce Martin, All Rights Reserved.
 *   
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *   
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 * ------------------------------------------------------------------------ */

package net.sf.JRecord.Details.fieldValue;

import java.math.BigDecimal;
import java.math.BigInteger;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.External.base.ExternalConversion;
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
public abstract class BaseFieldValue  {

	protected IFieldDetail field;


	/**
	 * Create a field value
	 *
	 * @param fieldDetails Field Description
	 */
	public BaseFieldValue(IFieldDetail fieldDetails) {
		field = fieldDetails;
	}

	/**
	 * @see IFieldValue#asBigDecimal()
	 */
	public final BigDecimal asBigDecimal() {
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
	public final BigInteger asBigInteger() {
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
	public final double asDouble() {
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
	public final float asFloat() {
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
	public final long asLong() {
		Object ret = getValue();

		if (ret == null) {
			return 0;
		} else if (ret instanceof Number) {
			return ((Number) ret).longValue();
		} else {
			String s = ret.toString();
			if (s.trim().length() == 0) {
				return 0;
			}
			if (s.indexOf(".") >= 0) {
				return new BigDecimal(s).longValue();
			}
			return Long.parseLong(s);
		}
	}


	/**
	 * @see IFieldValue#asInt()
	 */
	public final int asInt() {
		return (int) asLong();
	}



	/**
	 * @see IFieldValue#asBoolean()
	 */
	public final boolean asBoolean() {
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
	public final String toString() {
		return asString();
	}


	public final String asString() {
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
	protected abstract Object getValue(); 

	
	/* (non-Javadoc)
	 * @see net.sf.JRecord.Common.AbstractFieldValue#set(net.sf.JRecord.Common.AbstractFieldValue)
	 */
	public final void set(AbstractFieldValue value) {
		set(value.asString());
	}

	/**
	 * @see IFieldValue#set(boolean)
	 */
	public final void set(boolean value) {
		set(Boolean.valueOf(value));
	}


	/**
	 * @see IFieldValue#set(double)
	 */
	public final void set(double value) {
		set(Double.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(float) 
	 */
	public final void set(float value) {
		set(Float.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(long)
	 */
	public final void set(long value) {
		set(Long.valueOf(value));
	}

	/**
	 * @see IFieldValue#set(java.lang.Object)
	 */
	public abstract void set(Object value);

	/**
	 * Get the Type name
	 * @return Type name
	 */
	public final String getTypeName() {
		return ExternalConversion.getTypeAsString(0, getFieldDetail().getType());
	}

	/**
	 * Wether it is a Numeric field
	 * @return is a Numeric field
	 */
	public final boolean isNumeric() {
		return getType().isNumeric();
	}

	/**
	 * Wether it is a binary Field
	 * @return is a binary field
	 */
	public final boolean isBinary() {
		return getType().isBinary();
	}
	

	private Type getType() {
		return TypeManager.getInstance().getType(getFieldDetail().getType());
	}

	/**
	 * Get The field Definition
	 * @return Field Definition
	 */
	public IFieldDetail getFieldDetail() {
		return field;
	}
}
