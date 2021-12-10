package net.sf.JRecord.Details.fieldValue;

import java.math.BigInteger;

public interface IIntField extends IFieldAttributes {

	/**
	 * @see IFieldValue#asBigInteger()
	 */
	BigInteger asBigInteger();

	
	int asInt();

	/**
	 * @see IFieldValue#set(long)
	 */
	void set(long value);

	/**
	 * @see IFieldValue#set(java.lang.Object)
	 */
	void set(Object value);
}