package net.sf.JRecord.Details.fieldValue;

import java.math.BigDecimal;

import net.sf.JRecord.Common.AbstractFieldValue;

public interface IDecimalField extends IFieldAttributes {

	/**
	 * @see AbstractFieldValue#asBigDecimal()
	 */
	BigDecimal asBigDecimal();

	/**
	 * @see AbstractFieldValue#set(java.lang.Object)
	 */
	void set(Object value);


}