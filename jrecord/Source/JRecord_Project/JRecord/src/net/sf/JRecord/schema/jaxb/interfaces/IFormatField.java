package net.sf.JRecord.schema.jaxb.interfaces;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.schema.jaxb.IItem;

public interface IFormatField {
	public String format(IItem itemDef, IFieldDetail fieldDef, String value);
}
