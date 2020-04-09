package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;

public class ArrayFieldValueSmallBin extends FieldValueSmallBin implements IArrayFieldValue {

	final IArrayAnyDimension array;
	ArrayFieldValueSmallBin(Line theLine, IFieldDetail field, ITypeBinaryExtendedNumeric type, IArrayAnyDimension array) {
		super(theLine, field, type);
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
