package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Types.TypeManager;
import net.sf.JRecord.Types.smallBin.ITypeBinaryExtendedNumeric;
import net.sf.JRecord.cgen.def.IArrayAnyDimension;

public class LineFieldCreator {
	private static final TypeManager TYPE_MANAGER = TypeManager.getInstance();
	private static final LineFieldCreator INSTANCE = new LineFieldCreator();
	
	/**
	 * @return the instance
	 */
	public static LineFieldCreator getInstance() {
		return INSTANCE;
	}

	public IFieldValueUpdLine newFieldValue(Line line, IFieldDetail field) {
		ITypeBinaryExtendedNumeric binType = TYPE_MANAGER.getShortLengthType(field.getType());
		if (binType != null) {
			return new FieldValueSmallBin(line, field, binType);
		}
		return new FieldValueLine(line, field);
	}

	public IFieldValueUpdLine newFieldValue(AbstractLine line, IFieldDetail field) {
		if (line instanceof Line) {
			return newFieldValue((Line) line, field);
		}
		return new FieldValue(line, field);
	}

	public IArrayFieldValue newArrayFieldValue(AbstractLine line, IArrayAnyDimension array) {
		IFieldDetail firstField = array.getFirstField();
		ITypeBinaryExtendedNumeric binType = TYPE_MANAGER.getShortLengthType(firstField.getType());
		if (binType != null && line instanceof Line) {
			return new ArrayFieldValueSmallBin((Line) line, firstField, binType, array);
		}
		return new ArrayFieldValue(line, array);
	}

}
