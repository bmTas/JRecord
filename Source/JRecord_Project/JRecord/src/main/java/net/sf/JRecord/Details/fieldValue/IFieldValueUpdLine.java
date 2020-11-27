package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;

public interface IFieldValueUpdLine extends IFieldValue {
	public IFieldValueUpdLine setLine(Line line);
	public IFieldValueUpdLine setLine(AbstractLine line);
}
