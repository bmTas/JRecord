package net.sf.JRecord.Details.fieldValue;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.Line;

public interface IArrayFieldValue {// extends IFieldValueUpdLine {
	public IFieldValueUpdLine setLine(Line line);
	public IFieldValueUpdLine setLine(AbstractLine line);
	public IFieldValue setIndex(int... indexs);
}
