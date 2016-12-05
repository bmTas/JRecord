package net.sf.JRecord.schema.jaxb.impl;

import net.sf.JRecord.Common.IFieldDetail;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;

public class DoNothingFormat implements IFormatField {
	public static final DoNothingFormat INSTANCE = new DoNothingFormat();
	
	@Override
	public String format(IItem itemDef, IFieldDetail fieldDef, String value) {
		return value;
	}

}
