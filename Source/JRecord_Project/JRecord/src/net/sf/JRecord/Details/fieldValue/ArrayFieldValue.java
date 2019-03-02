package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;

public class ArrayFieldValue extends FieldValue implements IArrayFieldValue {

	final IArrayAnyDimension array;
	ArrayFieldValue(AbstractLine theLine,  IArrayAnyDimension array) {
		super(theLine, array.getFirstField());
		this.array = array;
	}

	/* (non-Javadoc)
	 * @see net.sf.JRecord.Details.fieldValue.IArrayFieldValue#setIndex(int[])
	 */
	@Override
	public IFieldValue setIndex(int... indexs) {
		field = array.getField(indexs);
		return this;
	}

}
