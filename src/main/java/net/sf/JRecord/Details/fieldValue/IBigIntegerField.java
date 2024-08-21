package net.sf.JRecord.Details.fieldValue;

import java.math.BigInteger;

import net.sf.JRecord.Common.AbstractFieldValue;

public interface IBigIntegerField extends IFieldAttributes {

	/**
	 * @see AbstractFieldValue#asBigDecimal()
	 */
	BigInteger asBigInteger();

	/**
	 * @see AbstractFieldValue#set(java.lang.Object)
	 */
	void set(Object value);
}
